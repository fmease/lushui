//! The type checker.
// @Task Where possible, continue type checking on errors

use diagnostics::{
    Diagnostic, ErrorCode,
    error::{Handler, Health, Result, Stain},
    reporter::ErasedReportedError,
};
use hir::{
    AttributeName, Declaration, EntityKind, Expression, Identifier,
    special::{self, Type},
};
use hir_format::Display;
use interpreter::{BareDefinition, Definition, FunctionScope, Interpreter};
use joinery::JoinableIterator;
use session::{
    Session,
    component::{IdentifierExt, LocalDeclarationIndexExt},
};
use utility::{OwnedOrBorrowed::*, QuoteExt, displayed, pluralize};

pub mod interpreter;

pub fn check(declaration: &Declaration, session: &mut Session<'_>) -> Result {
    let mut typer = Typer::new(session);

    typer
        .start_infer_types_in_declaration(declaration, Context::default())
        .stain(&mut typer.health);
    typer.infer_types_of_out_of_order_bindings().stain(&mut typer.health);

    typer.health.into()
}

/// The state of the typer.
struct Typer<'sess, 'ctx> {
    // @Task add recursion depth field
    session: &'sess mut Session<'ctx>,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: ComponentIndex, expression: TypeAnnotation|Value|Both|... }
    out_of_order_bindings: Vec<Definition>,
    health: Health,
}

impl<'sess, 'ctx> Typer<'sess, 'ctx> {
    fn new(session: &'sess mut Session<'ctx>) -> Self {
        Self { session, out_of_order_bindings: Vec::new(), health: Health::Untainted }
    }

    fn interpreter(&self) -> Interpreter<'_> {
        Interpreter::new(self.session)
    }

    // @Task documentation
    fn start_infer_types_in_declaration(
        &mut self,
        declaration: &Declaration,
        mut context: Context,
    ) -> Result {
        use hir::BareDeclaration::*;

        match &declaration.bare {
            Function(function) => {
                self.evaluate_definition(Definition {
                    attributes: declaration.attributes.clone(),
                    bare: if declaration.attributes.has(AttributeName::Intrinsic) {
                        BareDefinition::IntrinsicFunction {
                            binder: function.binder,
                            type_: function.type_.clone(),
                        }
                    } else {
                        BareDefinition::Function {
                            binder: function.binder,
                            type_: function.type_.clone(),
                            value: Some(function.body.clone().unwrap()),
                        }
                    },
                })?;
            }
            Data(type_) => {
                // @Question don't return early??
                self.evaluate_definition(Definition {
                    attributes: declaration.attributes.clone(),
                    bare: BareDefinition::Data { binder: type_.binder, type_: type_.type_.clone() },
                })?;

                if let Some(constructors) = &type_.constructors {
                    let health = &mut Health::Untainted;

                    for constructor in constructors {
                        self.start_infer_types_in_declaration(
                            constructor,
                            Context { owning_data_type: Some(type_.binder) },
                        )
                        .stain(health);
                    }

                    return Result::from(*health);
                }
            }
            Constructor(constructor) => {
                let owner_data_type = context.owning_data_type.take().unwrap();

                self.evaluate_definition(Definition {
                    attributes: declaration.attributes.clone(),
                    bare: BareDefinition::Constructor {
                        binder: constructor.binder,
                        type_: constructor.type_.clone(),
                        owner_data_type,
                    },
                })?;
            }
            Module(module) => {
                let health = &mut Health::Untainted;

                for declaration in &module.declarations {
                    self.start_infer_types_in_declaration(declaration, Context::default())
                        .stain(health);
                }

                return Result::from(*health);
            }
            Use(_) | Error(_) => {}
        }

        Ok(())
    }

    // @Note very strange API going on here
    // @Note we might want to store the evaluated types into the scopes instead of the
    // unevaluated ones. This dependends on how we'd like to normalize (WeakHead|Normal)
    // @Task @Beacon somehow (*somehow*!) restructure this code so it is not DRY.
    // it is DRY even though we use an ugly macro..how sad is that??
    // we need to design the error handling here, it's super difficult, fragile, …
    fn evaluate_definition(&mut self, definition: Definition) -> Result {
        use BareDefinition::*;

        match definition.clone().bare {
            Function { binder, type_, value } => {
                let value = value.unwrap();

                if let Err(error) = self.it_is_a_type(&type_, &FunctionScope::Module) {
                    return self.handle_definition_error(error, &type_, None, definition, |_| ());
                }

                let type_ = self.interpreter().evaluate_expression(
                    &type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                let inferred_type =
                    match self.infer_type_of_expression(&value, &FunctionScope::Module) {
                        Ok(type_) => type_,
                        Err(error) => {
                            let attributes = definition.attributes.clone();

                            return self.handle_definition_error(
                                error,
                                &value,
                                // @Question is this correct?
                                None,
                                definition,
                                |typer| {
                                    typer.carry_out_definition(Definition {
                                        attributes,
                                        bare: Function { binder, type_, value: None },
                                    });
                                },
                            );
                        }
                    };

                if let Err(error) =
                    self.it_is_actual(&type_, &inferred_type, &FunctionScope::Module)
                {
                    return self.handle_definition_error(
                        error,
                        &value,
                        // @Question is this correct?
                        Some(&type_),
                        definition,
                        |_| (),
                    );
                }

                self.carry_out_definition(Definition {
                    attributes: definition.attributes,
                    bare: Function { binder, type_: inferred_type, value: Some(value) },
                });
            }
            Data { binder, type_ } => {
                if let Err(error) = self.it_is_a_type(&type_, &FunctionScope::Module) {
                    return self.handle_definition_error(error, &type_, None, definition, |_| ());
                }

                let type_ = self.interpreter().evaluate_expression(
                    &type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.assert_constructor_is_instance_of_type(
                    &type_,
                    &self.session.require_special(Type::Type, None)?.to_item(),
                )?;

                self.carry_out_definition(Definition {
                    attributes: definition.attributes,
                    bare: Data { binder, type_ },
                });
            }
            Constructor { binder, type_, owner_data_type: data } => {
                if let Err(error) = self.it_is_a_type(&type_, &FunctionScope::Module) {
                    return self.handle_definition_error(error, &type_, None, definition, |_| ());
                }

                let type_ = self.interpreter().evaluate_expression(
                    &type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.assert_constructor_is_instance_of_type(&type_, &data.to_item())?;

                self.carry_out_definition(Definition {
                    attributes: definition.attributes,
                    bare: Constructor { binder, type_, owner_data_type: data },
                });
            }
            IntrinsicFunction { binder, type_ } => {
                if let Err(error) = self.it_is_a_type(&type_, &FunctionScope::Module) {
                    return self.handle_definition_error(error, &type_, None, definition, |_| ());
                }

                let type_ = self.interpreter().evaluate_expression(
                    &type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.carry_out_definition(Definition {
                    attributes: definition.attributes,
                    bare: IntrinsicFunction { binder, type_ },
                });
            }
        }

        Ok(())
    }

    // @Note bad name
    fn carry_out_definition(&mut self, definition: Definition) {
        use BareDefinition::*;

        match definition.bare {
            Function { binder, type_, value } => {
                let index = binder.local_declaration_index(self.session).unwrap();
                let entity = &mut self.session[index];
                // @Question can't we just remove the bodiless check as intrinsic functions
                // (the only legal bodiless functions) are already handled separately via
                // IntrinsicFunction?
                debug_assert!(entity.is_untyped() || entity.is_bodiless_function());

                entity.kind = EntityKind::Function { type_, expression: value };
            }
            Data { binder, type_ } => {
                let index = binder.local_declaration_index(self.session).unwrap();
                let entity = &mut self.session[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::DataType {
                    namespace: std::mem::take(entity.namespace_mut().unwrap()),
                    type_,
                    constructors: Vec::new(),
                };
            }
            Constructor { binder, type_, owner_data_type: data } => {
                let index = binder.local_declaration_index(self.session).unwrap();
                let entity = &mut self.session[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::Constructor { type_ };

                let data_index = data.local_declaration_index(self.session).unwrap();

                match &mut self.session[data_index].kind {
                    EntityKind::DataType { constructors, .. } => constructors.push(binder),
                    _ => unreachable!(),
                }
            }
            IntrinsicFunction { binder, type_ } => {
                let index = binder.local_declaration_index(self.session).unwrap();
                debug_assert!(self.session[index].is_untyped());

                let function = self.session.specials().get(index.global(self.session)).unwrap();
                let special::Binding::Function(function) = function else { unreachable!() };

                self.session[index].kind = EntityKind::IntrinsicFunction { function, type_ };
            }
        }
    }

    // @Task heavily improve API / architecture
    fn handle_definition_error(
        &mut self,
        error: Error,
        actual_value: &Expression,
        expectation_cause: Option<&Expression>,
        definition: Definition,
        out_of_order_handler: impl FnOnce(&mut Self),
    ) -> Result {
        match error {
            Erased(error) => Err(error),
            OutOfOrderBinding => {
                self.out_of_order_bindings.push(definition);
                out_of_order_handler(self);
                Ok(())
            }
            TypeMismatch { expected, actual } => Err(Diagnostic::error()
                .code(ErrorCode::E032)
                // @Task put back some more information into the message: use `_`s to shorten the type
                .message("type mismatch")
                .span(actual_value, "has the wrong type")
                .with(|it| match expectation_cause {
                    Some(cause) => it.label(cause, "expected due to this"),
                    None => it,
                })
                .note(format!(
                    "\
expected type ‘{}’
 but got type ‘{}’",
                    self.display(&expected),
                    self.display(&actual),
                ))
                .report(self.session.reporter())),
        }
    }

    fn infer_types_of_out_of_order_bindings(&mut self) -> Result {
        while !self.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                self.evaluate_definition(binding)?;
            }

            if previous_amount == self.out_of_order_bindings.len() {
                if let Health::Tainted(_) = self.health {
                    return Ok(());
                }

                return Err(Diagnostic::bug()
                    .message(format!(
                        "found unresolvable {} during type checking",
                        pluralize!(previous_amount, "binding")
                    ))
                    .note(format!(
                        "namely {}",
                        self.out_of_order_bindings
                            .iter()
                            .map(|binding| displayed(|f| binding.write(self.session, f)).quote())
                            .join_with(", ")
                    ))
                    .report(self.session.reporter()));
            }
        }

        Ok(())
    }

    /// Try to infer the type of an expression.
    // @Beacon @Task verify and implement that all it_is_a_type and it_is_actual lead to good error messages
    // and keep it DRY (try to abstract over error handling, find a good API)
    // @Task make independent (^^) type errors non-fatal in respect to each other, i.e. return more than one
    // type error in possible cases
    fn infer_type_of_expression(
        // @Task change to &mut self (for warnings), this also means
        // changing the definition of FunctionScope... it's FunctionScope::Component now, no payload
        // this means more boilerplate methods (either duplication or as in resolver: every FunctionScope method takes
        // a component: &Component parameter)
        &self,
        expression: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<Expression, Error> {
        use hir::{BareExpression::*, Substitution::*};

        Ok(match &expression.bare {
            // @Task explanation why we need to special-case Type here!
            Binding(binding) if self.session.specials().is(binding.0, Type::Type) => {
                self.session.require_special(Type::Type, None)?.to_item()
            }
            Binding(binding) => {
                self.interpreter().look_up_type(binding.0, scope).ok_or(OutOfOrderBinding)?
            }
            Number(number) => {
                self.session.require_special(number.type_(), Some(expression.span))?.to_item()
            }
            Text(_) => self.session.require_special(Type::Text, Some(expression.span))?.to_item(),
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type_of_expression`) has been normalized?
                self.it_is_a_type(&literal.domain, scope)?;

                let scope = if literal.binder.is_some() {
                    Owned(scope.extend_with_parameter(&literal.domain))
                } else {
                    Borrowed(scope)
                };

                self.it_is_a_type(&literal.codomain, scope.as_ref())?;

                self.session.require_special(Type::Type, None)?.to_item()
            }
            Lambda(lambda) => {
                let domain = lambda
                    .domain
                    .as_ref()
                    .ok_or_else(|| missing_annotation_error().report(self.session.reporter()))?;

                self.it_is_a_type(domain, scope)?;

                let scope = scope.extend_with_parameter(domain);
                let inferred_body_type = self.infer_type_of_expression(&lambda.body, &scope)?;

                if let Some(codomain) = &lambda.codomain {
                    self.it_is_a_type(codomain, &scope)?;
                    self.it_is_actual(codomain, &inferred_body_type, &scope)?;
                }

                Expression::new(
                    expression.attributes.clone(),
                    expression.span,
                    hir::PiType {
                        kind: hir::ParameterKind::Explicit,
                        binder: lambda.binder,
                        domain: domain.clone(),
                        codomain: inferred_body_type,
                    }
                    .into(),
                )
            }
            Application(application) => {
                // @Note this is an example where we normalize after an infer_type_of_expression which means infer_type_of_expression
                // returns possibly non-normalized expressions, can we do better?
                let type_of_callee = self.infer_type_of_expression(&application.callee, scope)?;
                let type_of_callee = self
                    .interpreter()
                    .evaluate_expression(&type_of_callee, interpreter::Context::new(scope))?;

                if let PiType(pi) = &type_of_callee.bare {
                    let argument_type =
                        self.infer_type_of_expression(&application.argument, scope)?;

                    // @Beacon @Beacon @Beacon @Task re-introduce `lazy` with `@lazy`
                    // let argument_type = if pi.laziness.is_some() {
                    //     Expression::new(
                    //         default(),
                    //         argument_type.span,
                    //         hir::PiType {
                    //             explicitness: Explicitness::Explicit,
                    //             binder: None,
                    //             domain: self
                    //                 .session
                    //                 .require_special(Type::Unit, Some(application.callee.span))?,
                    //             codomain: argument_type,
                    //         }
                    //         .into(),
                    //     )
                    // } else {
                    //     argument_type
                    // };

                    self.it_is_actual(&pi.domain, &argument_type, scope)
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        .map_err(|error| match error {
                            Erased(error) => error,
                            TypeMismatch { expected, actual } => Diagnostic::error()
                                .code(ErrorCode::E032)
                                // @Task put back some more information into the message: use `_`s to shorten the type
                                .message("type mismatch")
                                .span(&application.argument, "has the wrong type")
                                .label(&expected, "expected due to this")
                                .note(format!(
                                    "\
expected type ‘{}’
 but got type ‘{}’",
                                    self.display(&expected),
                                    self.display(&actual),
                                ))
                                .report(self.session.reporter()),
                            OutOfOrderBinding => unreachable!(),
                        })?;

                    match pi.binder {
                        Some(_) => Expression::bare(
                            hir::Substituted {
                                substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                                expression: pi.codomain.clone(),
                            }
                            .into(),
                        ),
                        None => pi.codomain.clone(),
                    }
                } else {
                    return Err(Diagnostic::error()
                        .code(ErrorCode::E031)
                        // @Task put back some more information into the message: use `_`s to shorten the type
                        .message("type mismatch")
                        .span(&application.callee, "has wrong type")
                        .label(&application.argument, "applied to this")
                        .note(format!(
                            "\
expected type ‘_ -> _’
 but got type ‘{}’",
                            self.display(&type_of_callee)
                        ))
                        .report(self.session.reporter())
                        .into());
                }
            }
            Substituted(substituted) => {
                let expression = substituted.expression.substitute(&substituted.substitution);
                self.infer_type_of_expression(&expression, scope)?
            }
            CaseAnalysis(analysis) => {
                let subject_type = self.infer_type_of_expression(&analysis.scrutinee, scope)?;
                // to force substitutions
                let subject_type = self
                    .interpreter()
                    .evaluate_expression(&subject_type, interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. type_ is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all analysis.cases>>.expressions are of the same type

                match &subject_type.bare {
                    Binding(_) => {}
                    Application(_application) => todo!("polymorphic types in patterns"),
                    _ if self.is_a_type(&subject_type, scope)? => {
                        return Err(Diagnostic::error()
                            .code(ErrorCode::E035)
                            .message("attempt to analyze a type")
                            .unlabeled_span(expression.span)
                            .note("forbidden to uphold parametricity and type erasure")
                            .report(self.session.reporter())
                            .into());
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        self.display(&subject_type)
                    ),
                }

                let mut type_of_previous_body = None::<Expression>;

                for case in &analysis.cases {
                    use hir::BarePattern::*;

                    let mut binder_types = Vec::new();

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pattern.bare {
                        Number(number) => {
                            let number_type = self
                                .session
                                .require_special(number.type_(), Some(case.pattern.span))?
                                .to_item();
                            self.it_is_actual(&subject_type, &number_type, scope).map_err(
                                |error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pattern,
                                        &analysis.scrutinee,
                                    )
                                },
                            )?;
                        }
                        Text(_) => {
                            let text_type = self
                                .session
                                .require_special(Type::Text, Some(case.pattern.span))?
                                .to_item();
                            self.it_is_actual(&subject_type, &text_type, scope).map_err(
                                |error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pattern,
                                        &analysis.scrutinee,
                                    )
                                },
                            )?;
                        }
                        Binding(binding) => {
                            let constructor_type =
                                self.interpreter().look_up_type(binding.0, scope).unwrap();

                            self.it_is_actual(&subject_type, &constructor_type, scope).map_err(
                                |error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pattern,
                                        &analysis.scrutinee,
                                    )
                                },
                            )?;
                        }
                        LetBinding(_) => {
                            // @Temporary @Beacon @Bug error prone (once we try to impl deappl)
                            // @Update @Note don't push the type of subject but the type of the binder
                            binder_types.push(&subject_type);
                        }
                        // @Task
                        Application(application) => {
                            // @Beacon @Task check that subject type is a pi type

                            match (&application.callee.bare, &application.argument.bare) {
                                // @Note should be an error obviously but does this need to be special-cased
                                // or can we defer this to an it_is_actual call??
                                (Number(_) | Text(_), _argument) => todo!(),
                                (Binding(binding), _argument) => {
                                    let _constructor_type =
                                        self.interpreter().look_up_type(binding.0, scope).unwrap();

                                    todo!();
                                }
                                // @Task make error less fatal (keep processing next cases (match arms))
                                (LetBinding(binder), _) => {
                                    return Err(Diagnostic::error()
                                        .code(ErrorCode::E034)
                                        .message(format!(
                                            "binder ‘{binder}’ used in callee position inside pattern",
                                        ))
                                        .unlabeled_span(binder)
                                        .help("consider referring to a concrete binding")
                                        .report(self.session.reporter())
                                        .into());
                                }
                                (Application(_), _argument) => todo!(),
                                (Error(_), _) => unreachable!(),
                            };
                        }
                        Error(_) => unreachable!(),
                    }

                    let body_type = self.infer_type_of_expression(
                        &case.body,
                        &scope.extend_with_pattern_binders(binder_types),
                    )?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            self.it_is_actual(previous_type, &body_type, scope)?;
                        }
                        None => {
                            type_of_previous_body = Some(body_type);
                        }
                    }
                }

                //  @Temporary unhandled case
                type_of_previous_body.expect("caseless case analyses")
            }
            // @Task
            IntrinsicApplication(_) => todo!("inferring the type of intrinsic applications"),
            // @Task
            Projection(projection) => {
                let _basis_type = self.infer_type_of_expression(&projection.basis, scope)?;

                // @Task check if the basis type is a record and if it has given field
                // @Note somehow substitute stuff, consider

                // record R A of
                //     f: A
                // main = R.{f = Nat.0}::f ;;; 0 : Nat

                // This might need unification

                todo!("inferring the type of projections")
            }
            // @Beacon @Beacon @Beacon @Bug unsound, check if the type is record type first!
            // @Task type-check the fields, too!
            Record(record) => {
                // @Task check if `type_` is a record type
                // @Task check if the fields are correct (amount, names, types)

                // @Note this is so ugly!
                hir::Identifier::new(
                    record.type_.bare,
                    self.session[record.type_.bare].source.respan(record.type_.span),
                )
                .to_item()
            }
            IO(_) => self.session.require_special(Type::IO, Some(expression.span))?.to_item(),
            Error(_) => expression.clone(),
        })
    }

    fn handle_case_analysis_type_mismatch(
        &self,
        error: Error,
        pattern: &hir::Pattern,
        scrutinee: &Expression,
    ) -> Error {
        match error {
            TypeMismatch { expected, actual } => Diagnostic::error()
                .code(ErrorCode::E032)
                // @Task put back some more information into the message: use `_`s to shorten the type
                .message("type mismatch")
                .span(pattern, "has the wrong type")
                .label(scrutinee, "expected due to this")
                .note(format!(
                    "\
expected type ‘{}’
but got type ‘{}’",
                    self.display(&expected),
                    self.display(&actual)
                ))
                .report(self.session.reporter())
                .into(),
            error => error,
        }
    }

    /// Assert that an expression is of type `Type`.
    fn it_is_a_type(
        &self,
        expression: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<(), Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.it_is_actual(&self.session.require_special(Type::Type, None)?.to_item(), &type_, scope)
    }

    fn is_a_type(&self, expression: &Expression, scope: &FunctionScope<'_>) -> Result<bool, Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.is_actual(&self.session.require_special(Type::Type, None)?.to_item(), &type_, scope)
            .map_err(Into::into)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn it_is_actual(
        &self,
        expected: &Expression,
        actual: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<(), Error> {
        let context = interpreter::Context::new(scope);
        let expected = self.interpreter().evaluate_expression(expected, context)?;
        let actual = self.interpreter().evaluate_expression(actual, context)?;

        if !self.interpreter().equals(&expected, &actual, scope)? {
            return Err(TypeMismatch { expected, actual });
        }

        Ok(())
    }

    fn is_actual(
        &self,
        expected: &Expression,
        actual: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        let context = interpreter::Context::new(scope);
        let expected = self.interpreter().evaluate_expression(expected, context)?;
        let actual = self.interpreter().evaluate_expression(actual, context)?;

        self.interpreter().equals(&expected, &actual, scope)
    }

    /// Instance checking.
    ///
    /// I.e. does a constructor of an algebraïc data type return a valid
    /// instance of the respective type?
    ///
    /// Note: Currently, the checker does allow existential type parameters
    /// and specialized instances. This will complicate the implementation
    /// of case analysis. Of course, feature-complete Lushui shall support
    /// existentials and specialized instances but we first might want to
    /// feature-gate them.
    fn assert_constructor_is_instance_of_type(
        &self,
        constructor: &Expression,
        type_: &Expression,
    ) -> Result {
        let codomain = constructor.innermost_codomain();
        let callee = codomain.innermost_callee();

        if self.interpreter().equals(type_, callee, &FunctionScope::Module)? {
            Ok(())
        } else {
            Err(Diagnostic::error()
                .code(ErrorCode::E033)
                .message(format!(
                    "‘{}’ is not an instance of ‘{}’",
                    self.display(&codomain),
                    self.display(type_),
                ))
                .unlabeled_span(codomain.span)
                .report(self.session.reporter()))
        }
    }

    fn display<'f>(&'f self, value: &'f impl Display) -> impl std::fmt::Display + 'f {
        displayed(|f| value.write(self.session, f))
    }
}

impl Handler for &mut Typer<'_, '_> {
    fn embed<T: diagnostics::error::PossiblyErroneous>(self, diagnostic: Diagnostic) -> T {
        let error = diagnostic.report(self.session.reporter());
        self.health.taint(error);
        T::error(error)
    }
}

trait ExpressionExt {
    /// The innermost codomain of an expression.
    ///
    /// # Example
    ///
    /// The `R` in `A -> B -> C -> R`.
    fn innermost_codomain(&self) -> Expression;

    /// The innermost callee of an expression.
    ///
    /// # Example
    ///
    /// The `f` in `f a b c`.
    fn innermost_callee(&self) -> &Expression;
}

impl ExpressionExt for Expression {
    fn innermost_codomain(&self) -> Expression {
        fn innermost_codomain(expression: &Expression, scope: &FunctionScope<'_>) -> Expression {
            match &expression.bare {
                hir::BareExpression::PiType(pi) => {
                    let scope = if pi.binder.is_some() {
                        Owned(scope.extend_with_parameter(&pi.domain))
                    } else {
                        Borrowed(scope)
                    };

                    innermost_codomain(&pi.codomain, scope.as_ref())
                }
                _ => expression.clone(),
            }
        }

        innermost_codomain(self, &FunctionScope::Module)
    }

    fn innermost_callee(mut self: &Self) -> &Expression {
        loop {
            self = match &self.bare {
                hir::BareExpression::Application(application) => &application.callee,
                _ => return self,
            }
        }
    }
}

#[derive(Default)]
struct Context {
    owning_data_type: Option<Identifier>,
}

fn missing_annotation_error() -> Diagnostic {
    // @Task span
    Diagnostic::error()
        .code(ErrorCode::E030)
        .message("currently lambda literal parameters and patterns must be type-annotated")
}

// @Note maybe we should redesign this as a trait (object) looking at those
// methods mirroring the variants
enum Error {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    OutOfOrderBinding,
    TypeMismatch {
        expected: Expression,
        actual: Expression,
    },
}

use Error::*;

use crate::interpreter::Substitute;

impl From<ErasedReportedError> for Error {
    fn from(error: ErasedReportedError) -> Self {
        Self::Erased(error)
    }
}
