//! The LLVM-IR code generator.

use crate::{
    component::Component,
    entity::Entity,
    hir::{self, DeclarationIndex},
    session::BuildSession,
    utility::HashMap,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    targets::TargetMachine,
    values::{FunctionValue, UnnamedAddress},
};
use std::{cell::RefCell, default::default};

pub(super) fn compile<'ctx>(
    component_root: &hir::Declaration,
    context: &'ctx Context,
    component: &Component,
    session: &BuildSession,
) -> Module<'ctx> {
    let generator = Generator::new(context, component, session);
    generator.compile_declaration(component_root);
    generator.module
}

struct Generator<'a, 'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    functions: RefCell<HashMap<DeclarationIndex, FunctionValue<'ctx>>>,
    component: &'a Component,
    session: &'a BuildSession,
}

impl<'a, 'ctx> Generator<'a, 'ctx> {
    fn new(context: &'ctx Context, component: &'a Component, session: &'a BuildSession) -> Self {
        let module = context.create_module("main");
        module.set_triple(&TargetMachine::get_default_triple());
        let builder = context.create_builder();

        Self {
            builder,
            context,
            module,
            functions: default(),
            component,
            session,
        }
    }

    fn compile_declaration(&self, declaration: &hir::Declaration) {
        use hir::DeclarationKind::*;

        #[allow(clippy::match_same_arms)] // @Temporary
        match &declaration.bare {
            Function(function) => {
                let index = function.binder.index.declaration().unwrap();

                // @Task somewhere store the info if we've already function the program entry
                let is_program_entry = function.binder.as_str()
                    == Component::PROGRAM_ENTRY_IDENTIFIER
                    && self.look_up_parent(index).unwrap() == self.component.root();

                let llvm_function = self.module.add_function(
                    // @Task introduce debug mode where we use the path
                    if is_program_entry { "main" } else { "" },
                    // @Task do not hard-code this!
                    self.context.i32_type().fn_type(&[], false),
                    if is_program_entry {
                        None
                    } else {
                        Some(Linkage::Internal)
                    },
                );

                llvm_function
                    .as_global_value()
                    .set_unnamed_address(UnnamedAddress::Global);

                self.functions.borrow_mut().insert(index, llvm_function);

                let start_block = self.context.append_basic_block(llvm_function, "start");
                self.builder.position_at_end(start_block);

                // @Task handle intrinsics!
                self.compile_expression(function.expression.as_ref().unwrap());
            }
            // @Temporary skipping for now
            Data(_) => {}
            // @Temporary skipping for now
            Constructor(_) => {}
            Module(module) => {
                for declaration in &module.declarations {
                    self.compile_declaration(declaration);
                }
            }
            Use(_) => {}
            // @Question how should we handle that?
            Error => todo!(),
        };
    }

    fn compile_expression(&self, expression: &hir::Expression) {
        use hir::ExpressionKind::*;

        #[allow(clippy::match_same_arms)] // @Temporary
        match &expression.bare {
            PiType(_) | Type => {
                self.builder.build_return(None);
            }
            Application(_) => todo!(),
            Number(number) => {
                use hir::Number::*;

                #[allow(clippy::cast_sign_loss, clippy::cast_lossless)]
                let value = match &**number {
                    Nat(_) => todo!(),
                    &Nat32(number) => self.context.i32_type().const_int(number as _, false),
                    &Nat64(number) => self.context.i64_type().const_int(number, false),
                    Int(_) => todo!(),
                    &Int32(number) => self.context.i32_type().const_int(number as _, true),
                    &Int64(number) => self.context.i64_type().const_int(number as _, true),
                };

                self.builder.build_return(Some(&value));
            }
            Text(_) => todo!(),
            Binding(binding) => {
                use hir::Index::*;

                match binding.0.index {
                    Declaration(index) => {
                        let value = self
                            .builder
                            .build_call(self.functions.borrow()[&index], &[], "")
                            .try_as_basic_value()
                            .left()
                            .unwrap();
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
