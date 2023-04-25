//! The LLVM-IR generator.
#![feature(let_chains, io_error_other)]
#![allow(clippy::match_same_arms)] // @Temporary

use diagnostics::{error::Result, Diagnostic};
use hir::{special::Type, DeclarationIndex};
use hir_format::SessionExt;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{FunctionType, IntType},
    values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, UnnamedAddress},
};
use session::{Session, OUTPUT_FOLDER_NAME};
use std::{
    cell::RefCell,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use utility::{default, FormatError, HashMap, Str, PROGRAM_ENTRY};

pub fn compile_and_link(
    options: Options,
    component_root: &hir::Declaration,
    session: &Session<'_>,
) -> Result<()> {
    let context = inkwell::context::Context::create();
    let module = compile(component_root, &context, options, session);

    if options.emit_llvm_ir {
        // @Question shouldn't we print to stdout??
        module.print_to_stderr();
    }

    if options.verify_llvm_ir && let Err(message) = module.verify() {
        return Err(Diagnostic::bug()
            .message("the generated LLVM-IR is invalid")
            .note(message.to_string())
            .report(session.reporter()));
    }

    if let Err(error) = link(&module, session) {
        return Err(Diagnostic::error()
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
            .report(session.reporter()));
    }

    Ok(())
}

#[derive(Clone, Copy, Default)]
pub struct Options {
    pub emit_llvm_ir: bool,
    pub verify_llvm_ir: bool,
}

fn compile<'ctx>(
    component_root: &hir::Declaration,
    context: &'ctx Context,
    options: Options,
    session: &Session<'_>,
) -> Module<'ctx> {
    let generator = Generator::new(context, options, session);
    generator.start_compile_declaration(component_root);
    generator.finish_compile_declaration(component_root);
    generator.module
}

// @Task support linkers other than clang
//       (e.g. "`cc`", `gcc` (requires the use of `llc`))
fn link(module: &inkwell::module::Module<'_>, session: &Session<'_>) -> Result<(), LinkingError> {
    let buffer = module.write_bitcode_to_memory();
    let name = session.component().name().to_str();

    let output_file_path = match session.root_package() {
        // @Task ensure that the build folder exists
        Some(package) => {
            let mut path = session[package].folder().join(OUTPUT_FOLDER_NAME);
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
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    bindings: RefCell<HashMap<DeclarationIndex, Entity<'a, 'ctx>>>,
    options: Options,
    session: &'a Session<'a>,
}

impl<'a, 'ctx> Generator<'a, 'ctx> {
    fn new(context: &'ctx Context, options: Options, session: &'a Session<'a>) -> Self {
        let module = context.create_module("main");
        module.set_triple(&TargetMachine::get_default_triple());
        let builder = context.create_builder();

        Self {
            builder,
            context,
            module,
            bindings: default(),
            options,
            session,
        }
    }

    /// Start compiling the given declaration.
    fn start_compile_declaration(&self, declaration: &'a hir::Declaration) {
        use hir::BareDeclaration::*;

        match &declaration.bare {
            Function(function) => {
                use ExpressionClassification::*;

                let index = function.binder.index.declaration().unwrap();

                // @Task somewhere store the info if we've already found the program entry or not
                let is_program_entry = function.binder.bare() == PROGRAM_ENTRY
                    && self.session.parent_of(index).unwrap() == self.session.component().root();

                let classification = function.body.as_ref().map(|expression| {
                    let classification = if is_program_entry {
                        Thunk
                    } else {
                        expression.classify(self.session)
                    };
                    (classification, expression)
                });

                match classification {
                    Some((Constant, expression)) => {
                        let type_ = self.translate_type(&function.type_);
                        let value = self
                            .module
                            .add_global(type_, None, self.name(index).as_ref());
                        value.set_unnamed_address(UnnamedAddress::Global);
                        value.set_linkage(Linkage::Internal);
                        value.set_constant(true);
                        value.set_initializer(&self.compile_constant_expression(expression));

                        self.bindings
                            .borrow_mut()
                            .insert(index, Entity::Constant { value, type_ });
                    }
                    // @Beacon @Task simplify
                    Some((Thunk, _)) => {
                        let name = if is_program_entry {
                            PROGRAM_ENTRY.to_str().into()
                        } else {
                            self.name(index)
                        };

                        let type_ = self.translate_type(&function.type_).fn_type(&[], false);

                        let thunk = self.module.add_function(
                            name.as_ref(),
                            type_,
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
                    Some((Function(lambda), _)) => {
                        // @Beacon @Task handle functions of arbitrary "arity" here!

                        let function_value = self.module.add_function(
                            self.name(index).as_ref(),
                            self.translate_unary_function_type(&function.type_),
                            Some(Linkage::Internal),
                        );

                        function_value
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings.borrow_mut().insert(
                            index,
                            Entity::UnaryFunction {
                                value: function_value,
                                lambda,
                            },
                        );
                    }
                    None => {
                        let hir::EntityKind::IntrinsicFunction {
                            function: intrinsic,
                            ..
                        } = self.session[index].kind
                        else {
                            unreachable!();
                        };

                        // @Beacon @Task handle functions of arbitrary "arity" here!

                        let function_value = self.module.add_function(
                            intrinsic.name(),
                            self.translate_unary_function_type(&function.type_),
                            None,
                        );

                        function_value
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings.borrow_mut().insert(
                            index,
                            Entity::Intrinsic {
                                value: function_value,
                            },
                        );
                    }
                }
            }
            // @Temporary skipping for now
            Data(_) => {}
            // @Temporary skipping for now
            Constructor(_) => {}
            Module(module) => {
                for declaration in &module.declarations {
                    self.start_compile_declaration(declaration);
                }
            }
            Use(_) => {}
            // @Question how should we handle that?
            Error(_) => todo!(),
        };
    }

    /// Finish compiling the given declaration.
    fn finish_compile_declaration(&self, declaration: &hir::Declaration) {
        use hir::BareDeclaration::*;

        match &declaration.bare {
            Function(function) => {
                let index = function.binder.index.declaration().unwrap();

                if declaration.attributes.has(hir::AttributeName::Intrinsic) {
                    return;
                }

                let expression = function.body.as_ref().unwrap();

                match self.bindings.borrow()[&index] {
                    Entity::Constant { .. } | Entity::Intrinsic { .. } => {}
                    Entity::Thunk(thunk) => {
                        let start_block = self.context.append_basic_block(thunk, "start");
                        self.builder.position_at_end(start_block);

                        let value = self.compile_expression(expression, Vec::new());

                        self.builder
                            .build_return(value.as_ref().map(|value| value as _));
                    }
                    Entity::UnaryFunction {
                        value: unary_function,
                        lambda,
                    } => {
                        let start_block = self.context.append_basic_block(unary_function, "start");
                        self.builder.position_at_end(start_block);

                        let value = self.compile_expression(
                            &lambda.body,
                            vec![unary_function.get_nth_param(0).unwrap()],
                        );

                        self.builder
                            .build_return(value.as_ref().map(|value| value as _));
                    }
                }
            }
            // @Temporary skipping for now
            Data(_) => {}
            // @Temporary skipping for now
            Constructor(_) => {}
            Module(module) => {
                for declaration in &module.declarations {
                    self.finish_compile_declaration(declaration);
                }
            }
            Use(_) => {}
            // @Question how should we handle that?
            Error(_) => todo!(),
        };
    }

    fn name(&self, index: DeclarationIndex) -> Str {
        if self.options.emit_llvm_ir {
            self.session.index_to_path(index).into()
        } else {
            "".into()
        }
    }

    fn compile_constant_expression(&self, expression: &hir::Expression) -> BasicValueEnum<'ctx> {
        use hir::BareExpression::*;

        // @Task return a unit-struct value for any types
        match &expression.bare {
            PiType(_) => todo!(),
            Number(number) => self.compile_number(number).into(),
            Text(_) => todo!(),
            Binding(_) => todo!(),
            Lambda(_)
            | CaseAnalysis(_)
            | Application(_)
            | IntrinsicApplication(_)
            | Projection(_) => {
                unreachable!()
            }
            Substituted(_) => todo!(),
            IO(_) => todo!(),
            Error(_) => todo!(),
        }
    }

    fn compile_expression(
        &self,
        expression: &hir::Expression,
        substitutions: Vec<BasicValueEnum<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        use hir::BareExpression::*;

        match &expression.bare {
            PiType(_) => None,
            Application(application) => {
                use hir::Index::*;

                let Binding(callee) = &application.callee.bare else {
                    todo!("compiling applications with non-binding callee");
                };

                let argument = self
                    .compile_expression(&application.argument, substitutions)
                    .unwrap();

                let value = match callee.0.index {
                    Declaration(index) => {
                        let (Entity::UnaryFunction {
                            value: function, ..
                        }
                        | Entity::Intrinsic { value: function }) = self.bindings.borrow()[&index]
                        else {
                            unreachable!();
                        };

                        self.builder
                            .build_call(function, &[argument.into()], "")
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                    DeBruijn(_) => todo!("compiling application with local callee"),
                    Parameter => unreachable!(),
                };

                Some(value)
            }
            Number(number) => Some(self.compile_number(number).into()),
            Text(_) => todo!("compiling text"),
            Binding(binding) if self.session.specials().is(binding.0, Type::Type) => None,
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.index {
                    Declaration(index) => {
                        let value = match self.bindings.borrow()[&index] {
                            Entity::Constant { value, type_ } => {
                                self.builder.build_load(type_, value.as_pointer_value(), "")
                            }
                            Entity::Thunk(thunk) => self
                                .builder
                                .build_call(thunk, &[], "")
                                .try_as_basic_value()
                                .left()
                                .unwrap(),
                            Entity::UnaryFunction { .. } | Entity::Intrinsic { .. } => {
                                unreachable!()
                            }
                        };

                        Some(value)
                    }
                    DeBruijn(index) => Some(substitutions[index.0]),

                    Parameter => unreachable!(),
                }
            }
            Lambda(_) => todo!("compiling lambdas"),
            CaseAnalysis(_) => todo!("compiling case analyses"),
            IntrinsicApplication(_) => todo!("compiling intrinsic applications"),
            Projection(_) => todo!("compiling record projections"),
            IO(_) => todo!("compiling IO actions"),
            Substituted(_) | Error(_) => unreachable!(),
        }
    }

    fn compile_number(&self, number: &hir::Number) -> IntValue<'ctx> {
        use hir::Number::*;

        #[allow(clippy::cast_sign_loss, clippy::cast_lossless)]
        // @Question is the sign-extension stuff correct?
        match number {
            Nat(_) => todo!(),
            &Nat32(number) => self.context.i32_type().const_int(number as _, false),
            &Nat64(number) => self.context.i64_type().const_int(number, false),
            Int(_) => todo!(),
            &Int32(number) => self.context.i32_type().const_int(number as _, true),
            &Int64(number) => self.context.i64_type().const_int(number as _, true),
        }
    }

    // @Note currently only handles numeric types
    fn translate_type(&self, type_: &hir::Expression) -> IntType<'ctx> {
        use hir::BareExpression::*;

        match &type_.bare {
            PiType(_) => todo!(),
            Application(_) => todo!(),
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.index {
                    Declaration(index) => {
                        use hir::special::{Binding, NumericType::*, Type::*};

                        // @Task don't unwrap
                        let Binding::Type(type_) = self.session.specials().get(index).unwrap()
                        else {
                            unreachable!();
                        };

                        match type_ {
                            Type => todo!(),
                            Numeric(Nat) => todo!(),
                            Numeric(Int) => todo!(),
                            Numeric(Nat32 | Int32) => self.context.i32_type(),
                            Numeric(Nat64 | Int64) => self.context.i64_type(),
                            Text => todo!(),
                            IO => todo!(),
                            // @Temporary
                            Unit | Bool | Option | Sequential(_) => unreachable!(),
                        }
                    }
                    DeBruijn(_) => todo!(),
                    Parameter => unreachable!(),
                }
            }
            Lambda(_) => todo!(),
            CaseAnalysis(_) => todo!(),
            Substituted(_) => todo!(),
            IntrinsicApplication(_) => todo!(),
            Projection(_) => todo!(),
            Error(_) => todo!(),
            Number(_) | Text(_) | IO(_) => unreachable!(),
        }
    }

    fn translate_unary_function_type(&self, type_: &hir::Expression) -> FunctionType<'ctx> {
        use hir::BareExpression::*;

        match &type_.bare {
            PiType(pi) => {
                let domain = self.translate_type(&pi.domain);
                // @Task pass along the parameter!
                let codomain = self.translate_type(&pi.codomain);

                codomain.fn_type(&[domain.into()], false)
            }
            Application(_) => todo!(),
            Number(_) | Text(_) => unreachable!(),
            Binding(_) => todo!(),
            Lambda(_) => todo!(),
            CaseAnalysis(_) => todo!(),
            Substituted(_) => todo!(),
            IntrinsicApplication(_) => todo!(),
            Projection(_) => todo!(),
            IO(_) => todo!(),
            Error(_) => todo!(),
        }
    }
}

trait ExpressionExt {
    fn classify(&self, session: &Session<'_>) -> ExpressionClassification<'_>;
}

impl ExpressionExt for hir::Expression {
    fn classify(&self, session: &Session<'_>) -> ExpressionClassification<'_> {
        use hir::BareExpression::*;
        use ExpressionClassification::*;

        match &self.bare {
            PiType(pi) => match (pi.domain.classify(session), pi.codomain.classify(session)) {
                (Constant, Constant) => Constant,
                _ => Thunk,
            },
            Application(_) | IntrinsicApplication(_) => Thunk,
            Number(_) | Text(_) => Constant,
            Binding(binding) if session.specials().is(binding.0, Type::Type) => Constant,
            // @Note we could make this more nuanced (prefering Constant if possible)
            Binding(_) => Thunk,
            Lambda(lambda) => Function(lambda),
            CaseAnalysis(_) => Thunk,
            Substituted(_) => todo!(),
            Projection(_) => todo!(),
            IO(_) => todo!(),
            Error(_) => todo!(),
        }
    }
}

enum Entity<'a, 'ctx> {
    Constant {
        value: GlobalValue<'ctx>,
        type_: IntType<'ctx>,
    },
    Thunk(FunctionValue<'ctx>),
    UnaryFunction {
        value: FunctionValue<'ctx>,
        lambda: &'a hir::Lambda,
    },
    Intrinsic {
        value: FunctionValue<'ctx>,
    },
}

enum ExpressionClassification<'a> {
    Constant,
    Thunk,
    Function(&'a hir::Lambda),
}
