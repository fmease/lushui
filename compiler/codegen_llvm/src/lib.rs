//! The LLVM-IR generator.
#![feature(default_free_fn, let_chains)]
#![allow(clippy::match_same_arms)] // @Temporary

use diagnostics::Diagnostic;
use error::Result;
use hir::{DeclarationIndex, Entity};
use hir_format::ComponentExt;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::{FunctionType, IntType},
    values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, UnnamedAddress},
};
use resolver::ProgramEntryExt;
use session::{BuildSession, Component, DeclarationIndexExt};
use std::{
    cell::RefCell,
    default::default,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};
use utilities::{HashMap, Str};

const PROGRAM_ENTRY_NAME: &str = "main";

pub fn compile_and_link(
    options: Options,
    component_root: &hir::Declaration,
    component: &Component,
    session: &BuildSession,
) -> Result<()> {
    if !component.is_target(session) {
        return Err(Diagnostic::error()
            .message("extern components cannot be built yet with the LLVM backend")
            .report(session.reporter()));
    }

    let context = inkwell::context::Context::create();
    let module = compile(component_root, &context, options, component, session);

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

    link(module, component, session)
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
    component: &Component,
    session: &BuildSession,
) -> Module<'ctx> {
    let generator = Generator::new(context, options, component, session);
    generator.start_compile_declaration(component_root);
    generator.finish_compile_declaration(component_root);
    generator.module
}

// @Task support linkers other than clang
//       (e.g. "`cc`", `gcc` (requires the use of `llc`))
fn link(
    module: inkwell::module::Module<'_>,
    component: &Component,
    session: &BuildSession,
) -> Result {
    let buffer = module.write_bitcode_to_memory();
    let name = component.name().as_str();

    // @Task error handling!
    let mut compiler = Command::new("clang")
        .args(["-x", "ir", "-", "-o"])
        .arg(match session.target_package() {
            // @Task ensure that the build folder exists
            Some(package) => {
                let mut path = session[package].path.join(BuildSession::OUTPUT_FOLDER_NAME);
                path.push(name);
                path
            }
            None => PathBuf::from(name),
        })
        .stdin(Stdio::piped())
        // @Task smh store stderr for reporting later
        .spawn()
        .unwrap();

    compiler
        .stdin
        .take()
        .unwrap()
        .write_all(buffer.as_slice())
        .unwrap();

    let status = compiler.wait().unwrap();

    if !status.success() {
        return Err(Diagnostic::error()
            .message("failed to link object files")
            .report(session.reporter()));
    }

    Ok(())
}
struct Generator<'a, 'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    bindings: RefCell<HashMap<DeclarationIndex, Definition<'a, 'ctx>>>,
    options: Options,
    component: &'a Component,
    session: &'a BuildSession,
}

impl<'a, 'ctx> Generator<'a, 'ctx> {
    fn new(
        context: &'ctx Context,
        options: Options,
        component: &'a Component,
        session: &'a BuildSession,
    ) -> Self {
        let module = context.create_module("main");
        module.set_triple(&TargetMachine::get_default_triple());
        let builder = context.create_builder();

        Self {
            builder,
            context,
            module,
            bindings: default(),
            options,
            component,
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
                let is_program_entry = function.binder.as_str()
                    == Component::PROGRAM_ENTRY_IDENTIFIER
                    && self.look_up_parent(index).unwrap() == self.component.root();

                if declaration.attributes.has(hir::AttributeName::Intrinsic) {
                    todo!("compiling intrinsics");
                }

                let Some(expression) = &function.expression else { return };

                let classification = if is_program_entry {
                    Thunk
                } else {
                    expression.classify(self.session)
                };

                match classification {
                    Constant => {
                        let constant = self.module.add_global(
                            self.translate_type(&function.type_annotation),
                            None,
                            self.name(index).as_ref(),
                        );
                        constant.set_unnamed_address(UnnamedAddress::Global);
                        constant.set_linkage(Linkage::Internal);
                        constant.set_constant(true);
                        constant.set_initializer(&self.compile_constant_expression(expression));

                        self.bindings
                            .borrow_mut()
                            .insert(index, Definition::Constant(constant));
                    }
                    // @Beacon @Task simplify
                    Thunk => {
                        let name = if is_program_entry {
                            PROGRAM_ENTRY_NAME.into()
                        } else {
                            self.name(index)
                        };

                        let type_ = self
                            .translate_type(&function.type_annotation)
                            .fn_type(&[], false);

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
                            .insert(index, Definition::Thunk(thunk));
                    }
                    Function(lambda) => {
                        // @Beacon @Task handle functions of arbitrary "arity" here!

                        let function_value = self.module.add_function(
                            self.name(index).as_ref(),
                            self.translate_unary_function_type(&function.type_annotation),
                            Some(Linkage::Internal),
                        );

                        function_value
                            .as_global_value()
                            .set_unnamed_address(UnnamedAddress::Global);

                        self.bindings.borrow_mut().insert(
                            index,
                            Definition::UnaryFunction {
                                value: function_value,
                                lambda,
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
            Error => todo!(),
        };
    }

    /// Finish compiling the given declaration.
    fn finish_compile_declaration(&self, declaration: &hir::Declaration) {
        use hir::BareDeclaration::*;

        match &declaration.bare {
            Function(function) => {
                let index = function.binder.index.declaration().unwrap();

                if declaration.attributes.has(hir::AttributeName::Intrinsic) {
                    todo!("compiling intrinsics");
                }

                let Some(expression) = &function.expression else { return };

                match self.bindings.borrow()[&index] {
                    Definition::Constant(_) => {}
                    Definition::Thunk(thunk) => {
                        let start_block = self.context.append_basic_block(thunk, "start");
                        self.builder.position_at_end(start_block);

                        let value = self.compile_expression(expression, Vec::new());

                        self.builder
                            .build_return(value.as_ref().map(|value| value as _));
                    }
                    Definition::UnaryFunction {
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
            Error => todo!(),
        };
    }

    fn name(&self, index: DeclarationIndex) -> Str {
        if self.options.emit_llvm_ir {
            self.component.index_to_path(index, self.session).into()
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
            Error => todo!(),
        }
    }

    fn compile_expression(
        &self,
        expression: &hir::Expression,
        substitutions: Vec<BasicValueEnum<'ctx>>,
    ) -> Option<BasicValueEnum<'ctx>> {
        use hir::{intrinsic::Type, BareExpression::*};

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
                        let Definition::UnaryFunction { value: function, .. } = self.bindings.borrow()[&index] else {
                            unreachable!();
                        };

                        self.builder
                            .build_call(function, &[argument.into()], "")
                            .try_as_basic_value()
                            .left()
                            .unwrap()
                    }
                    DeBruijn(_) => todo!("compiling application with local callee"),
                    DeBruijnParameter => unreachable!(),
                };

                Some(value)
            }
            Number(number) => Some(self.compile_number(number).into()),
            Text(_) => todo!("compiling text"),
            Binding(binding) if self.session.is_intrinsic_type(Type::Type, &binding.0) => None,
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.index {
                    Declaration(index) => {
                        let value = match self.bindings.borrow()[&index] {
                            Definition::Constant(constant) => {
                                self.builder.build_load(constant.as_pointer_value(), "")
                            }
                            Definition::Thunk(thunk) => self
                                .builder
                                .build_call(thunk, &[], "")
                                .try_as_basic_value()
                                .left()
                                .unwrap(),
                            Definition::UnaryFunction { .. } => unreachable!(),
                        };

                        Some(value)
                    }
                    DeBruijn(index) => Some(substitutions[index.0]),

                    DeBruijnParameter => unreachable!(),
                }
            }
            Lambda(_) => todo!("compiling lambdas"),
            CaseAnalysis(_) => todo!("compiling case analyses"),
            IntrinsicApplication(_) => todo!("compiling intrinsic applications"),
            Projection(_) => todo!("compiling record projections"),
            IO(_) => todo!("compiling IO actions"),
            Substituted(_) | Error => unreachable!(),
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
                        use hir::intrinsic::{NumericType::*, Type::*};

                        // @Beacon @Task use a HashMap<DeclarationIndex, IntrinsicType> for this!
                        // @Task don't unwrap
                        let type_ = self
                            .session
                            .intrinsic_types()
                            .find(|(_, binder)| binder.index.declaration().unwrap() == index)
                            .unwrap()
                            .0;

                        match type_ {
                            Type => todo!(),
                            Numeric(Nat) => todo!(),
                            Numeric(Int) => todo!(),
                            Numeric(Nat32 | Int32) => self.context.i32_type(),
                            Numeric(Nat64 | Int64) => self.context.i64_type(),
                            Text => todo!(),
                            IO => todo!(),
                        }
                    }
                    DeBruijn(_) => todo!(),
                    DeBruijnParameter => unreachable!(),
                }
            }
            Lambda(_) => todo!(),
            CaseAnalysis(_) => todo!(),
            Substituted(_) => todo!(),
            IntrinsicApplication(_) => todo!(),
            Projection(_) => todo!(),
            Error => todo!(),
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
            Error => todo!(),
        }
    }

    // @Task dedup with name resolver, documenter, interpreter
    fn look_up(&self, index: DeclarationIndex) -> &'a Entity {
        match index.local(self.component) {
            Some(index) => &self.component[index],
            None => &self.session[index],
        }
    }

    fn look_up_parent(&self, index: DeclarationIndex) -> Option<DeclarationIndex> {
        Some(DeclarationIndex::new(
            index.component(),
            self.look_up(index).parent?,
        ))
    }
}

trait ExpressionExt {
    fn classify(&self, session: &BuildSession) -> ExpressionClassification<'_>;
}

impl ExpressionExt for hir::Expression {
    fn classify(&self, session: &BuildSession) -> ExpressionClassification<'_> {
        use hir::{intrinsic::Type, BareExpression::*};
        use ExpressionClassification::*;

        match &self.bare {
            PiType(pi) => match (pi.domain.classify(session), pi.codomain.classify(session)) {
                (Constant, Constant) => Constant,
                _ => Thunk,
            },
            Application(_) | IntrinsicApplication(_) => Thunk,
            Number(_) | Text(_) => Constant,
            Binding(binding) if session.is_intrinsic_type(Type::Type, &binding.0) => Constant,
            // @Note we could make this more nuanced (prefering Constant if possible)
            Binding(_) => Thunk,
            Lambda(lambda) => Function(lambda),
            CaseAnalysis(_) => Thunk,
            Substituted(_) => todo!(),
            Projection(_) => todo!(),
            IO(_) => todo!(),
            Error => todo!(),
        }
    }
}

enum Definition<'a, 'ctx> {
    Constant(GlobalValue<'ctx>),
    Thunk(FunctionValue<'ctx>),
    UnaryFunction {
        value: FunctionValue<'ctx>,
        lambda: &'a hir::Lambda,
    },
}

enum ExpressionClassification<'a> {
    Constant,
    Thunk,
    Function(&'a hir::Lambda),
}
