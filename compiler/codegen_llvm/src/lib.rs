//! The LLVM-IR generator.
#![feature(let_chains)]
#![allow(clippy::match_same_arms)] // @Temporary

use diagnostics::{error::Result, Diag};
use hir::{special::Ty, DeclIdx};
use hir_format::SessionExt;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{FunctionType, IntType},
    values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, UnnamedAddress},
};
use llvm_sys as _;
use session::{Session, OUTPUT_FOLDER_NAME};
use std::{
    cell::RefCell,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use utility::{default, FormatError, HashMap, Str, PROGRAM_ENTRY};

pub fn compile_and_link(opts: Options, comp_root: &hir::Decl, sess: &Session<'_>) -> Result<()> {
    let cx = inkwell::context::Context::create();
    let module = compile(comp_root, &cx, opts, sess);

    if opts.emit_llvm_ir {
        // @Question shouldn't we print to stdout??
        module.print_to_stderr();
    }

    if opts.verify_llvm_ir
        && let Err(message) = module.verify()
    {
        return Err(Diag::bug()
            .message("the generated LLVM-IR is invalid")
            .note(message.to_string())
            .report(sess.rep()));
    }

    if let Err(error) = link(&module, sess) {
        return Err(Diag::error()
            .message("could not link object files")
            .with(|it| match error {
                // @Task print the path
                LinkingError::UnresolvedRuntimeSystem(error) => it
                    .note(format!(
                        "failed to load the runtime system ‘boot’: {}",
                        error.format()
                    ))
                    .help("consider rebuilding it"),
                LinkingError::ProcessExecutionFailed(error) => it.note(format!(
                    "failed to execute the linker `clang`: {}",
                    error.format()
                )),
                LinkingError::LinkerFailed(stderr) => it
                    .note("the linker `clang` exited unsuccessfully")
                    .note(stderr),
            })
            .report(sess.rep()));
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_llvm_ir: bool,
    pub verify_llvm_ir: bool,
}

fn compile<'ctx>(
    comp_root: &hir::Decl,
    cx: &'ctx Context,
    opts: Options,
    sess: &Session<'_>,
) -> Module<'ctx> {
    let gen = Generator::new(cx, opts, sess);
    gen.start_compile_decl(comp_root);
    gen.finish_compile_decl(comp_root);
    gen.module
}

// @Task support linkers other than clang
//       (e.g. "`cc`", `gcc` (requires the use of `llc`))
fn link(module: &inkwell::module::Module<'_>, sess: &Session<'_>) -> Result<(), LinkingError> {
    let buffer = module.write_bitcode_to_memory();
    let name = sess.comp().name().to_str();

    let output_file_path = match sess.root_pkg() {
        // @Task ensure that the build folder exists
        Some(package) => {
            let mut path = sess[package].folder().join(OUTPUT_FOLDER_NAME);
            path.push(name);
            path
        }
        None => PathBuf::from(name),
    };

    // @Task search the "distributed libraries path" (sysroot) instead
    let runtime_system_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/release")
        .canonicalize()
        .map_err(LinkingError::UnresolvedRuntimeSystem)?;

    let mut compiler = Command::new("clang")
        .args(["-x", "ir", "-", "-lboot", "-L"])
        .arg(runtime_system_path)
        .arg("-o")
        .arg(output_file_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()?;

    compiler
        .stdin
        .take()
        .unwrap()
        .write_all(buffer.as_slice())?;

    let output = compiler.wait_with_output()?;

    if !output.status.success() {
        let stderr = match String::from_utf8(output.stderr) {
            Ok(stderr) => stderr,
            Err(error) => String::from_utf8_lossy(&error.into_bytes()).into_owned(),
        };

        return Err(LinkingError::LinkerFailed(stderr));
    }

    Ok(())
}

enum LinkingError {
    UnresolvedRuntimeSystem(std::io::Error),
    ProcessExecutionFailed(std::io::Error),
    LinkerFailed(String),
}

impl From<std::io::Error> for LinkingError {
    fn from(error: std::io::Error) -> Self {
        Self::ProcessExecutionFailed(error)
    }
}

struct Generator<'a, 'ctx> {
    cx: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    bindings: RefCell<HashMap<DeclIdx, Entity<'a, 'ctx>>>,
    opts: Options,
    sess: &'a Session<'a>,
}

impl<'a, 'ctx> Generator<'a, 'ctx> {
    fn new(cx: &'ctx Context, opts: Options, sess: &'a Session<'a>) -> Self {
        let module = cx.create_module("main");
        module.set_triple(&TargetMachine::get_default_triple());
        let builder = cx.create_builder();

        Self {
            builder,
            cx,
            module,
            bindings: default(),
            opts,
            sess,
        }
    }

    /// Start compiling the given declaration.
    fn start_compile_decl(&self, decl: &'a hir::Decl) {
        use hir::BareDecl::*;

        match &decl.bare {
            Func(func) => {
                use ExprClass::*;

                let index = func.binder.idx.decl_idx().unwrap();

                // @Task somewhere store the info if we've already found the program entry or not
                let is_program_entry = func.binder.bare() == PROGRAM_ENTRY
                    && self.sess.parent_of(index).unwrap() == self.sess.comp().root();

                let class = func.body.as_ref().map(|expr| {
                    let class = if is_program_entry {
                        Thunk
                    } else {
                        expr.classify(self.sess)
                    };
                    (class, expr)
                });

                match class {
                    Some((Constant, expr)) => {
                        let ty = self.translate_ty(&func.ty);
                        let value = self.module.add_global(ty, None, self.name(index).as_ref());
                        value.set_unnamed_address(UnnamedAddress::Global);
                        value.set_linkage(Linkage::Internal);
                        value.set_constant(true);
                        value.set_initializer(&self.compile_const_expr(expr));

                        self.bindings
                            .borrow_mut()
                            .insert(index, Entity::Constant { value, ty });
                    }
                    // @Beacon @Task simplify
                    Some((Thunk, _)) => {
                        let name = if is_program_entry {
                            PROGRAM_ENTRY.to_str().into()
                        } else {
                            self.name(index)
                        };

                        let ty = self.translate_ty(&func.ty).fn_type(&[], false);

                        let thunk = self.module.add_function(
                            name.as_ref(),
                            ty,
                            if is_program_entry {
                                None
                            } else {
                                Some(Linkage::Internal)
                            },
                        );

                        thunk
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings
                            .borrow_mut()
                            .insert(index, Entity::Thunk(thunk));
                    }
                    Some((Fn(lambda), _)) => {
                        // @Beacon @Task handle functions of arbitrary "arity" here!

                        let func_value = self.module.add_function(
                            self.name(index).as_ref(),
                            self.translate_unary_func_ty(&func.ty),
                            Some(Linkage::Internal),
                        );

                        func_value
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings.borrow_mut().insert(
                            index,
                            Entity::UnaryFunc {
                                value: func_value,
                                lambda,
                            },
                        );
                    }
                    None => {
                        let hir::EntityKind::FuncIntr {
                            func: intrinsic, ..
                        } = self.sess[index].kind
                        else {
                            unreachable!();
                        };

                        // @Beacon @Task handle functions of arbitrary "arity" here!

                        let func_value = self.module.add_function(
                            intrinsic.name(),
                            self.translate_unary_func_ty(&func.ty),
                            None,
                        );

                        func_value
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings
                            .borrow_mut()
                            .insert(index, Entity::Intrinsic { value: func_value });
                    }
                }
            }
            // @Temporary skipping for now
            DataTy(_) => {}
            // @Temporary skipping for now
            Ctor(_) => {}
            Module(module) => {
                for decl in &module.decls {
                    self.start_compile_decl(decl);
                }
            }
            Use(_) => {}
            // @Question how should we handle that?
            Error(_) => todo!(),
        };
    }

    /// Finish compiling the given declaration.
    fn finish_compile_decl(&self, decl: &hir::Decl) {
        use hir::BareDecl::*;

        match &decl.bare {
            Func(func) => {
                let index = func.binder.idx.decl_idx().unwrap();

                if decl.attrs.has(hir::AttrName::Intrinsic) {
                    return;
                }

                let expr = func.body.as_ref().unwrap();

                match self.bindings.borrow()[&index] {
                    Entity::Constant { .. } | Entity::Intrinsic { .. } => {}
                    Entity::Thunk(thunk) => {
                        let start_block = self.cx.append_basic_block(thunk, "start");
                        self.builder.position_at_end(start_block);

                        let value = self.compile_expr(expr, Vec::new());

                        self.builder
                            .build_return(value.as_ref().map(|value| value as _))
                            .unwrap();
                    }
                    Entity::UnaryFunc {
                        value: unary_func,
                        lambda,
                    } => {
                        let start_block = self.cx.append_basic_block(unary_func, "start");
                        self.builder.position_at_end(start_block);

                        let value = self
                            .compile_expr(&lambda.body, vec![unary_func.get_nth_param(0).unwrap()]);

                        self.builder
                            .build_return(value.as_ref().map(|value| value as _))
                            .unwrap();
                    }
                }
            }
            // @Temporary skipping for now
            DataTy(_) => {}
            // @Temporary skipping for now
            Ctor(_) => {}
            Module(module) => {
                for declaration in &module.decls {
                    self.finish_compile_decl(declaration);
                }
            }
            Use(_) => {}
            // @Question how should we handle that?
            Error(_) => todo!(),
        };
    }

    fn name(&self, index: DeclIdx) -> Str {
        if self.opts.emit_llvm_ir {
            self.sess.index_to_path(index).into()
        } else {
            "".into()
        }
    }

    fn compile_const_expr(&self, expr: &hir::Expr) -> BasicValueEnum<'ctx> {
        use hir::BareExpr::*;

        // @Task return a unit-struct value for any types
        match &expr.bare {
            PiTy(_) => todo!(),
            NumLit(num) => self.compile_num(num).into(),
            TextLit(_) => todo!(),
            RecLit(_) => todo!(),
            Binding(_) => todo!(),
            LamLit(_) | CaseAnalysis(_) | App(_) | IntrApp(_) | Proj(_) => {
                unreachable!()
            }
            Substed(_) => todo!(),
            IO(_) => todo!(),
            Error(_) => todo!(),
        }
    }

    fn compile_expr(
        &self,
        expr: &hir::Expr,
        substs: Vec<BasicValueEnum<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        use hir::BareExpr::*;

        match &expr.bare {
            PiTy(_) => None,
            App(app) => {
                use hir::Index::*;

                let Binding(callee) = &app.callee.bare else {
                    todo!("compiling applications with non-binding callee");
                };

                let arg = self.compile_expr(&app.arg, substs).unwrap();

                let value = match callee.0.idx {
                    Decl(index) => {
                        let (Entity::UnaryFunc { value: func, .. }
                        | Entity::Intrinsic { value: func }) = self.bindings.borrow()[&index]
                        else {
                            unreachable!();
                        };

                        self.builder
                            .build_call(func, &[arg.into()], "")
                            .unwrap()
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                    DeBruijn(_) => todo!("compiling application with local callee"),
                    Param => unreachable!(),
                };

                Some(value)
            }
            NumLit(num) => Some(self.compile_num(num).into()),
            TextLit(_) => todo!("compiling text"),
            RecLit(_) => todo!("compiling records"),
            Binding(binding) if self.sess.specials().is(binding.0, Ty::Type) => None,
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.idx {
                    Decl(index) => {
                        let value = match self.bindings.borrow()[&index] {
                            Entity::Constant { value, ty } => self
                                .builder
                                .build_load(ty, value.as_pointer_value(), "")
                                .unwrap(),
                            Entity::Thunk(thunk) => self
                                .builder
                                .build_call(thunk, &[], "")
                                .unwrap()
                                .try_as_basic_value()
                                .left()
                                .unwrap(),
                            Entity::UnaryFunc { .. } | Entity::Intrinsic { .. } => {
                                unreachable!()
                            }
                        };

                        Some(value)
                    }
                    DeBruijn(index) => Some(substs[index.0]),

                    Param => unreachable!(),
                }
            }
            LamLit(_) => todo!("compiling lambdas"),
            CaseAnalysis(_) => todo!("compiling case analyses"),
            IntrApp(_) => todo!("compiling intrinsic applications"),
            Proj(_) => todo!("compiling record projections"),
            IO(_) => todo!("compiling IO actions"),
            Substed(_) | Error(_) => unreachable!(),
        }
    }

    fn compile_num(&self, number: &hir::NumLit) -> IntValue<'ctx> {
        use hir::NumLit::*;

        #[allow(clippy::cast_sign_loss, clippy::cast_lossless)]
        // @Question is the sign-extension stuff correct?
        match number {
            Nat(_) => todo!(),
            &Nat32(number) => self.cx.i32_type().const_int(number as _, false),
            &Nat64(number) => self.cx.i64_type().const_int(number, false),
            Int(_) => todo!(),
            &Int32(number) => self.cx.i32_type().const_int(number as _, true),
            &Int64(number) => self.cx.i64_type().const_int(number as _, true),
        }
    }

    // @Note currently only handles numeric types
    fn translate_ty(&self, ty: &hir::Expr) -> IntType<'ctx> {
        use hir::BareExpr::*;

        match &ty.bare {
            PiTy(_) => todo!(),
            App(_) => todo!(),
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.idx {
                    Decl(index) => {
                        use hir::special::{Binding, NumTy::*, Ty::*};

                        // @Task don't unwrap
                        let Binding::Ty(ty) = self.sess.specials().get(index).unwrap() else {
                            unreachable!();
                        };

                        match ty {
                            Type => todo!(),
                            Num(Nat) => todo!(),
                            Num(Int) => todo!(),
                            Num(Nat32 | Int32) => self.cx.i32_type(),
                            Num(Nat64 | Int64) => self.cx.i64_type(),
                            Text => todo!(),
                            IO => todo!(),
                            // @Temporary
                            Unit | Bool | Option | Seq(_) => unreachable!(),
                        }
                    }
                    DeBruijn(_) => todo!(),
                    Param => unreachable!(),
                }
            }
            LamLit(_) => todo!(),
            CaseAnalysis(_) => todo!(),
            Substed(_) => todo!(),
            IntrApp(_) => todo!(),
            Proj(_) => todo!(),
            Error(_) => todo!(),
            NumLit(_) | TextLit(_) | RecLit(_) | IO(_) => unreachable!(),
        }
    }

    fn translate_unary_func_ty(&self, ty: &hir::Expr) -> FunctionType<'ctx> {
        use hir::BareExpr::*;

        match &ty.bare {
            PiTy(pi_ty) => {
                let domain = self.translate_ty(&pi_ty.domain);
                // @Task pass along the parameter!
                let codomain = self.translate_ty(&pi_ty.codomain);

                codomain.fn_type(&[domain.into()], false)
            }
            App(_) => todo!(),
            Binding(_) => todo!(),
            LamLit(_) => todo!(),
            CaseAnalysis(_) => todo!(),
            Substed(_) => todo!(),
            IntrApp(_) => todo!(),
            Proj(_) => todo!(),
            Error(_) => todo!(),
            NumLit(_) | TextLit(_) | RecLit(_) | IO(_) => unreachable!(),
        }
    }
}

trait ExprExt {
    fn classify(&self, sess: &Session<'_>) -> ExprClass<'_>;
}

impl ExprExt for hir::Expr {
    fn classify(&self, sess: &Session<'_>) -> ExprClass<'_> {
        use hir::BareExpr::*;
        use ExprClass::*;

        match &self.bare {
            PiTy(pi_ty) => match (pi_ty.domain.classify(sess), pi_ty.codomain.classify(sess)) {
                (Constant, Constant) => Constant,
                _ => Thunk,
            },
            App(_) | IntrApp(_) => Thunk,
            NumLit(_) | TextLit(_) => Constant,
            Binding(binding) if sess.specials().is(binding.0, Ty::Type) => Constant,
            // @Note we could make this more nuanced (prefering Constant if possible)
            Binding(_) => Thunk,
            LamLit(lambda) => Fn(lambda),
            CaseAnalysis(_) => Thunk,
            Substed(_) => todo!(),
            Proj(_) => todo!(),
            IO(_) => todo!(),
            RecLit(_) => todo!(),
            Error(_) => todo!(),
        }
    }
}

enum Entity<'a, 'ctx> {
    Constant {
        value: GlobalValue<'ctx>,
        ty: IntType<'ctx>,
    },
    Thunk(FunctionValue<'ctx>),
    UnaryFunc {
        value: FunctionValue<'ctx>,
        lambda: &'a hir::LamLit,
    },
    Intrinsic {
        value: FunctionValue<'ctx>,
    },
}

enum ExprClass<'a> {
    Constant,
    Thunk,
    Fn(&'a hir::LamLit),
}
