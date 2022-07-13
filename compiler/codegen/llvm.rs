//! The LLVM-IR generator.
#![allow(clippy::match_same_arms)] // @Temporary

use super::Options;
use crate::{
    component::Component,
    diagnostics::Diagnostic,
    entity::Entity,
    error::Result,
    hir,
    hir::DeclarationIndex,
    session::BuildSession,
    utility::{HashMap, Str},
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    types::IntType,
    values::{BasicValueEnum, FunctionValue, GlobalValue, IntValue, UnnamedAddress},
};
use std::{
    cell::RefCell,
    default::default,
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

const PROGRAM_ENTRY_NAME: &str = "main";

pub fn compile_and_link(
    options: Options,
    component_root: &hir::Declaration,
    component: &Component,
    session: &BuildSession,
) -> Result<()> {
    if !component.is_goal(session) {
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

    // @Task error handling!
    let mut compiler = Command::new("clang")
        .args(["-x", "ir", "-", "-o"])
        .arg(match session.goal_package() {
            // @Task ensure that the build folder exists
            Some(package) => session.build_folder().join(session[package].name.as_str()),
            None => PathBuf::from(component.name().as_str()),
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
    bindings: RefCell<HashMap<DeclarationIndex, Definition<'ctx>>>,
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
    fn start_compile_declaration(&self, declaration: &hir::Declaration) {
        use hir::BareDeclaration::*;

        match &declaration.bare {
            Function(function) => {
                let index = function.binder.index.declaration().unwrap();

                // @Task somewhere store the info if we've already found the program entry or not
                let is_program_entry = function.binder.as_str()
                    == Component::PROGRAM_ENTRY_IDENTIFIER
                    && self.look_up_parent(index).unwrap() == self.component.root();

                match &function.expression {
                    Some(expression) => {
                        use ExpressionClassification::*;

                        let mut classification = expression.classify();
                        if is_program_entry && let Constant = classification {
                            classification = Thunk;
                        }

                        match classification {
                            Constant => {
                                let constant = self.module.add_global(
                                    self.type_(&function.type_annotation),
                                    None,
                                    self.name(index).as_ref(),
                                );
                                constant.set_unnamed_address(UnnamedAddress::Global);
                                constant.set_linkage(Linkage::Internal);
                                constant.set_constant(true);
                                constant
                                    .set_initializer(&self.compile_constant_expression(expression));

                                self.bindings
                                    .borrow_mut()
                                    .insert(index, Definition::Constant(constant));
                            }
                            // @Beacon @Beacon @Beacon @Task simplify
                            Thunk => {
                                let name = if is_program_entry {
                                    PROGRAM_ENTRY_NAME.into()
                                } else {
                                    self.name(index)
                                };

                                let type_ =
                                    self.type_(&function.type_annotation).fn_type(&[], false);

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
                            // @Beacon @Beacon @Beacon @Task
                            Function(_) => todo!(),
                        }
                    }
                    None => todo!(),
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

                if let Definition::Thunk(thunk) = self.bindings.borrow()[&index] {
                    let start_block = self.context.append_basic_block(thunk, "start");
                    self.builder.position_at_end(start_block);

                    self.compile_expression_into_basic_block(function.expression.as_ref().unwrap());
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
            self.component.path_to_string(index, self.session).into()
        } else {
            "".into()
        }
    }

    fn compile_constant_expression(&self, expression: &hir::Expression) -> BasicValueEnum<'ctx> {
        use hir::BareExpression::*;

        match &expression.bare {
            // @Task return a unit struct value
            PiType(_) | Type => todo!(),
            Number(number) => self.compile_number(number).into(),
            Text(_) => todo!(),
            Binding(_) => todo!(),
            Lambda(_)
            | UseIn
            | CaseAnalysis(_)
            | Application(_)
            | IntrinsicApplication(_)
            | Projection(_) => {
                unreachable!()
            }
            Substitution(_) => todo!(),
            IO(_) => todo!(),
            Error => todo!(),
        }
    }

    fn compile_expression_into_basic_block(&self, expression: &hir::Expression) {
        use hir::BareExpression::*;

        match &expression.bare {
            PiType(_) | Type => {
                self.builder.build_return(None);
            }
            Application(_) => todo!(),
            Number(number) => {
                self.builder
                    .build_return(Some(&self.compile_number(number)));
            }
            Text(_) => todo!(),
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
                        };

                        self.builder.build_return(Some(&value));
                    }
                    DeBruijn(_) => todo!(),
                    DeBruijnParameter => todo!(),
                }
            }
            Lambda(_) => todo!(),
            UseIn => todo!(),
            CaseAnalysis(_) => todo!(),
            Substitution(_) => todo!(),
            IntrinsicApplication(_) => todo!(),
            Projection(_) => todo!(),
            IO(_) => todo!(),
            // @Question how should we handle that?
            Error => todo!(),
        };
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
    fn type_(&self, type_: &hir::Expression) -> IntType<'ctx> {
        use hir::BareExpression::*;

        match &type_.bare {
            PiType(_) => todo!(),
            Application(_) => todo!(),
            Type => todo!(),
            Number(_) => todo!(),
            Text(_) => todo!(),
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.index {
                    Declaration(index) => {
                        use crate::session::{IntrinsicNumericType::*, IntrinsicType::*};

                        // @Beacon @Beacon @Beacon @Task use a HashMap<DeclarationIndex, IntrinsicType> for this!
                        // @Task don't unwrap
                        let type_ = self
                            .session
                            .intrinsic_types()
                            .find(|(_, binder)| binder.index.declaration().unwrap() == index)
                            .unwrap()
                            .0;

                        match type_ {
                            Numeric(Nat) => todo!(),
                            Numeric(Int) => todo!(),
                            Numeric(Nat32 | Int32) => self.context.i32_type(),
                            Numeric(Nat64 | Int64) => self.context.i64_type(),
                            Text => todo!(),
                            IO => todo!(),
                        }
                    }
                    DeBruijn(_) => todo!(),
                    DeBruijnParameter => todo!(),
                }
            }
            Lambda(_) => todo!(),
            UseIn => todo!(),
            CaseAnalysis(_) => todo!(),
            Substitution(_) => todo!(),
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

impl hir::Expression {
    fn classify(&self) -> ExpressionClassification<'_> {
        use hir::BareExpression::*;
        use ExpressionClassification::*;

        match &self.bare {
            PiType(pi) => match (pi.domain.classify(), pi.codomain.classify()) {
                (Constant, Constant) => Constant,
                _ => Thunk,
            },
            Application(_) | IntrinsicApplication(_) => Thunk,
            Type | Number(_) | Text(_) => Constant,
            // @Note we could make this more nuanced (prefering Constant if possible)
            Binding(_) => Thunk,
            Lambda(lambda) => Function(lambda),
            UseIn => todo!(),
            CaseAnalysis(_) => Thunk,
            Substitution(_) => todo!(),
            Projection(_) => todo!(),
            IO(_) => todo!(),
            Error => todo!(),
        }
    }
}

enum Definition<'ctx> {
    Constant(GlobalValue<'ctx>),
    Thunk(FunctionValue<'ctx>),
}

enum ExpressionClassification<'a> {
    Constant,
    Thunk,
    Function(&'a hir::Lambda),
}
