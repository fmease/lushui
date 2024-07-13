//! The type checker.
// @Task Where possible, continue type checking on errors

use diagnostics::{
    error::{Handler, Health, Result, Stain},
    reporter::ErasedReportedError,
    Diag, ErrorCode,
};
use hir::{
    special::{self, Ty},
    AttrName, Decl, EntityKind, Expr, Ident,
};
use hir_format::Display;
use interp::{BareDef, Def, FuncScope, Interp};
use joinery::JoinableIterator;
use session::{
    component::{IdentExt, LocalDeclIdxExt},
    Session,
};
use utility::{displayed, pluralize, OwnedOrBorrowed::*, QuoteExt};

pub mod interp;

pub fn check(decl: &Decl, sess: &mut Session<'_>) -> Result {
    let mut typer = Typer::new(sess);

    typer
        .start_infer_tys_in_decl(decl, Context::default())
        .stain(&mut typer.health);
    typer
        .infer_tys_of_out_of_order_bindings()
        .stain(&mut typer.health);

    typer.health.into()
}

/// The state of the typer.
struct Typer<'sess, 'ctx> {
    // @Task add recursion depth field
    sess: &'sess mut Session<'ctx>,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CompIdx, expr: TyAnn|Value|Both|... }
    out_of_order_bindings: Vec<Def>,
    health: Health,
}

impl<'sess, 'ctx> Typer<'sess, 'ctx> {
    fn new(sess: &'sess mut Session<'ctx>) -> Self {
        Self {
            sess,
            out_of_order_bindings: Vec::new(),
            health: Health::Untainted,
        }
    }

    fn interp(&self) -> Interp<'_> {
        Interp::new(self.sess)
    }

    // @Task documentation
    fn start_infer_tys_in_decl(&mut self, decl: &Decl, mut cx: Context) -> Result {
        use hir::BareDecl::*;

        match &decl.bare {
            Func(func) => {
                self.eval_def(Def {
                    attrs: decl.attrs.clone(),
                    bare: if decl.attrs.has(AttrName::Intrinsic) {
                        BareDef::IntrFn {
                            binder: func.binder,
                            ty: func.ty.clone(),
                        }
                    } else {
                        BareDef::Fn {
                            binder: func.binder,
                            ty: func.ty.clone(),
                            value: Some(func.body.clone().unwrap()),
                        }
                    },
                })?;
            }
            DataTy(ty) => {
                // @Question don't return early??
                self.eval_def(Def {
                    attrs: decl.attrs.clone(),
                    bare: BareDef::Data {
                        binder: ty.binder,
                        ty: ty.ty.clone(),
                    },
                })?;

                if let Some(ctors) = &ty.ctors {
                    let health = &mut Health::Untainted;

                    for ctor in ctors {
                        self.start_infer_tys_in_decl(
                            ctor,
                            Context {
                                owning_data_ty: Some(ty.binder),
                            },
                        )
                        .stain(health);
                    }

                    return Result::from(*health);
                }
            }
            Ctor(ctor) => {
                let owner_data_ty = cx.owning_data_ty.take().unwrap();

                self.eval_def(Def {
                    attrs: decl.attrs.clone(),
                    bare: BareDef::Ctor {
                        binder: ctor.binder,
                        ty: ctor.ty.clone(),
                        owner_data_ty,
                    },
                })?;
            }
            Module(module) => {
                let health = &mut Health::Untainted;

                for decl in &module.decls {
                    self.start_infer_tys_in_decl(decl, Context::default())
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
    fn eval_def(&mut self, def: Def) -> Result {
        use BareDef::*;

        match def.clone().bare {
            Fn { binder, ty, value } => {
                let value = value.unwrap();

                if let Err(error) = self.it_is_a_ty(&ty, &FuncScope::Module) {
                    return self.handle_def_error(error, &ty, None, def, |_| ());
                };

                let ty = self
                    .interp()
                    .eval_expr(&ty, interp::Context::new(&FuncScope::Module))?;

                let inferred_ty = match self.infer_ty_of_expr(&value, &FuncScope::Module) {
                    Ok(ty) => ty,
                    Err(error) => {
                        let attributes = def.attrs.clone();

                        return self.handle_def_error(
                            error,
                            &value,
                            // @Question is this correct?
                            None,
                            def,
                            |typer| {
                                typer.apply_def(Def {
                                    attrs: attributes,
                                    bare: Fn {
                                        binder,
                                        ty,
                                        value: None,
                                    },
                                });
                            },
                        );
                    }
                };

                if let Err(error) = self.it_is_actual(&ty, &inferred_ty, &FuncScope::Module) {
                    return self.handle_def_error(
                        error,
                        &value,
                        // @Question is this correct?
                        Some(&ty),
                        def,
                        |_| (),
                    );
                };

                self.apply_def(Def {
                    attrs: def.attrs,
                    bare: Fn {
                        binder,
                        ty: inferred_ty,
                        value: Some(value),
                    },
                });
            }
            Data { binder, ty } => {
                if let Err(error) = self.it_is_a_ty(&ty, &FuncScope::Module) {
                    return self.handle_def_error(error, &ty, None, def, |_| ());
                };

                let ty = self
                    .interp()
                    .eval_expr(&ty, interp::Context::new(&FuncScope::Module))?;

                self.assert_ctor_is_instance_of_ty(
                    &ty,
                    &self.sess.require_special(Ty::Type, None)?.to_item(),
                )?;

                self.apply_def(Def {
                    attrs: def.attrs,
                    bare: Data { binder, ty },
                });
            }
            Ctor {
                binder,
                ty,
                owner_data_ty: data,
            } => {
                if let Err(error) = self.it_is_a_ty(&ty, &FuncScope::Module) {
                    return self.handle_def_error(error, &ty, None, def, |_| ());
                };

                let ty = self
                    .interp()
                    .eval_expr(&ty, interp::Context::new(&FuncScope::Module))?;

                self.assert_ctor_is_instance_of_ty(&ty, &data.to_item())?;

                self.apply_def(Def {
                    attrs: def.attrs,
                    bare: Ctor {
                        binder,
                        ty,
                        owner_data_ty: data,
                    },
                });
            }
            IntrFn { binder, ty } => {
                if let Err(error) = self.it_is_a_ty(&ty, &FuncScope::Module) {
                    return self.handle_def_error(error, &ty, None, def, |_| ());
                };

                let ty = self
                    .interp()
                    .eval_expr(&ty, interp::Context::new(&FuncScope::Module))?;

                self.apply_def(Def {
                    attrs: def.attrs,
                    bare: IntrFn { binder, ty },
                });
            }
        }

        Ok(())
    }

    // @Note bad name
    fn apply_def(&mut self, def: Def) {
        use BareDef::*;

        match def.bare {
            Fn { binder, ty, value } => {
                let index = binder.local_decl_idx(self.sess).unwrap();
                let entity = &mut self.sess[index];
                // @Question can't we just remove the bodiless check as intrinsic functions
                // (the only legal bodiless functions) are already handled separately via
                // IntrinsicFunction?
                debug_assert!(entity.is_untyped() || entity.is_bodiless_fn());

                entity.kind = EntityKind::Func {
                    ty,
                    expression: value,
                };
            }
            Data { binder, ty } => {
                let index = binder.local_decl_idx(self.sess).unwrap();
                let entity = &mut self.sess[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::DataTy {
                    namespace: std::mem::take(entity.namespace_mut().unwrap()),
                    ty,
                    ctors: Vec::new(),
                };
            }
            Ctor {
                binder,
                ty,
                owner_data_ty: owner,
            } => {
                let index = binder.local_decl_idx(self.sess).unwrap();
                let entity = &mut self.sess[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::Ctor { ty };

                let owner = owner.local_decl_idx(self.sess).unwrap();

                match &mut self.sess[owner].kind {
                    EntityKind::DataTy { ctors, .. } => ctors.push(binder),
                    _ => unreachable!(),
                }
            }
            IntrFn { binder, ty } => {
                let index = binder.local_decl_idx(self.sess).unwrap();
                debug_assert!(self.sess[index].is_untyped());

                let function = self.sess.specials().get(index.global(self.sess)).unwrap();
                let special::Binding::Func(function) = function else {
                    unreachable!()
                };

                self.sess[index].kind = EntityKind::FuncIntr { func: function, ty };
            }
        }
    }

    // @Task heavily improve API / architecture
    fn handle_def_error(
        &mut self,
        error: Error,
        actual_value: &Expr,
        expectation_cause: Option<&Expr>,
        def: Def,
        out_of_order_handler: impl FnOnce(&mut Self),
    ) -> Result {
        match error {
            Erased(error) => Err(error),
            OutOfOrderBinding => {
                self.out_of_order_bindings.push(def);
                out_of_order_handler(self);
                Ok(())
            }
            TypeMismatch { expected, actual } => Err(Diag::error()
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
                .report(self.sess.rep())),
        }
    }

    fn infer_tys_of_out_of_order_bindings(&mut self) -> Result {
        while !self.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                self.eval_def(binding)?;
            }

            if previous_amount == self.out_of_order_bindings.len() {
                if let Health::Tainted(_) = self.health {
                    return Ok(());
                }

                return Err(Diag::bug()
                    .message(format!(
                        "found unresolvable {} during type checking",
                        pluralize!(previous_amount, "binding")
                    ))
                    .note(format!(
                        "namely {}",
                        self.out_of_order_bindings
                            .iter()
                            .map(|binding| displayed(|f| binding.write(self.sess, f)).quote())
                            .join_with(", ")
                    ))
                    .report(self.sess.rep()));
            }
        }

        Ok(())
    }

    /// Try to infer the type of an expression.
    // @Beacon @Task verify and implement that all it_is_a_ty and it_is_actual lead to good error messages
    // and keep it DRY (try to abstract over error handling, find a good API)
    // @Task make independent (^^) type errors non-fatal in respect to each other, i.e. return more than one
    // type error in possible cases
    fn infer_ty_of_expr(
        // @Task change to &mut self (for warnings), this also means
        // changing the definition of FunctionScope... it's FunctionScope::Component now, no payload
        // this means more boilerplate methods (either duplication or as in resolver: every FunctionScope method takes
        // a component: &Component parameter)
        &self,
        expr: &Expr,
        scope: &FuncScope<'_>,
    ) -> Result<Expr, Error> {
        use hir::{BareExpr::*, Subst::*};

        Ok(match &expr.bare {
            // @Task explanation why we need to special-case Type here!
            Binding(binding) if self.sess.specials().is(binding.0, Ty::Type) => {
                self.sess.require_special(Ty::Type, None)?.to_item()
            }
            Binding(binding) => self
                .interp()
                .look_up_ty(binding.0, scope)
                .ok_or(OutOfOrderBinding)?,
            NumLit(num) => self
                .sess
                .require_special(num.ty(), Some(expr.span))?
                .to_item(),
            TextLit(_) => self
                .sess
                .require_special(Ty::Text, Some(expr.span))?
                .to_item(),
            PiTy(pi_ty) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type_of_expression`) has been normalized?
                self.it_is_a_ty(&pi_ty.domain, scope)?;

                let scope = if pi_ty.binder.is_some() {
                    Owned(scope.extend_with_param(&pi_ty.domain))
                } else {
                    Borrowed(scope)
                };

                self.it_is_a_ty(&pi_ty.codomain, scope.as_ref())?;

                self.sess.require_special(Ty::Type, None)?.to_item()
            }
            LamLit(lambda) => {
                let domain = lambda
                    .domain
                    .as_ref()
                    .ok_or_else(|| missing_annotation_error().report(self.sess.rep()))?;

                self.it_is_a_ty(domain, scope)?;

                let scope = scope.extend_with_param(domain);
                let inferred_body_ty = self.infer_ty_of_expr(&lambda.body, &scope)?;

                if let Some(codomain) = &lambda.codomain {
                    self.it_is_a_ty(codomain, &scope)?;
                    self.it_is_actual(codomain, &inferred_body_ty, &scope)?;
                }

                Expr::new(
                    expr.attrs.clone(),
                    expr.span,
                    hir::PiTy {
                        kind: hir::ParamKind::Explicit,
                        binder: lambda.binder,
                        domain: domain.clone(),
                        codomain: inferred_body_ty,
                    }
                    .into(),
                )
            }
            App(app) => {
                // @Note this is an example where we normalize after an infer_type_of_expression which means infer_type_of_expression
                // returns possibly non-normalized expressions, can we do better?
                let callee_ty = self.infer_ty_of_expr(&app.callee, scope)?;
                let callee_ty = self
                    .interp()
                    .eval_expr(&callee_ty, interp::Context::new(scope))?;

                if let PiTy(pi_ty) = &callee_ty.bare {
                    let arg_ty = self.infer_ty_of_expr(&app.arg, scope)?;

                    // @Beacon @Beacon @Beacon @Task re-introduce `lazy` with `@lazy`
                    // let arg_ty = if pi.laziness.is_some() {
                    //     Expr::new(
                    //         default(),
                    //         argument_ty.span,
                    //         hir::PiType {
                    //             explicitness: Explicitness::Explicit,
                    //             binder: None,
                    //             domain: self
                    //                 .session
                    //                 .require_special(Type::Unit, Some(app.callee.span))?,
                    //             codomain: argument_ty,
                    //         }
                    //         .into(),
                    //     )
                    // } else {
                    //     argument_ty
                    // };

                    self.it_is_actual(&pi_ty.domain, &arg_ty, scope)
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        .map_err(|error| match error {
                            Erased(error) => error,
                            TypeMismatch { expected, actual } => Diag::error()
                                .code(ErrorCode::E032)
                                // @Task put back some more information into the message: use `_`s to shorten the type
                                .message("type mismatch")
                                .span(&app.arg, "has the wrong type")
                                .label(&expected, "expected due to this")
                                .note(format!(
                                    "\
expected type ‘{}’
 but got type ‘{}’",
                                    self.display(&expected),
                                    self.display(&actual),
                                ))
                                .report(self.sess.rep()),
                            OutOfOrderBinding => unreachable!(),
                        })?;

                    match pi_ty.binder {
                        Some(_) => Expr::bare(
                            hir::Substed {
                                subst: Use(Box::new(Shift(0)), app.arg.clone()),
                                expr: pi_ty.codomain.clone(),
                            }
                            .into(),
                        ),
                        None => pi_ty.codomain.clone(),
                    }
                } else {
                    return Err(Diag::error()
                        .code(ErrorCode::E031)
                        // @Task put back some more information into the message: use `_`s to shorten the type
                        .message("type mismatch")
                        .span(&app.callee, "has wrong type")
                        .label(&app.arg, "applied to this")
                        .note(format!(
                            "\
expected type ‘_ -> _’
 but got type ‘{}’",
                            self.display(&callee_ty)
                        ))
                        .report(self.sess.rep())
                        .into());
                }
            }
            Substed(substed) => {
                let expr = substed.expr.subst(&substed.subst);
                self.infer_ty_of_expr(&expr, scope)?
            }
            CaseAnalysis(analysis) => {
                let subject_ty = self.infer_ty_of_expr(&analysis.scrutinee, scope)?;
                // to force substitutions
                let subject_ty = self
                    .interp()
                    .eval_expr(&subject_ty, interp::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. ty is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all analysis.cases>>.expressions are of the same type

                match &subject_ty.bare {
                    Binding(_) => {}
                    App(_application) => todo!("polymorphic types in patterns"),
                    _ if self.is_a_ty(&subject_ty, scope)? => {
                        return Err(Diag::error()
                            .code(ErrorCode::E035)
                            .message("attempt to analyze a type")
                            .unlabeled_span(expr.span)
                            .note("forbidden to uphold parametricity and type erasure")
                            .report(self.sess.rep())
                            .into());
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        self.display(&subject_ty)
                    ),
                };

                let mut prev_body_ty = None::<Expr>;

                for case in &analysis.cases {
                    use hir::BarePat::*;

                    let mut binder_tys = Vec::new();

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pat.bare {
                        NumLit(num) => {
                            let num_ty = self
                                .sess
                                .require_special(num.ty(), Some(case.pat.span))?
                                .to_item();
                            self.it_is_actual(&subject_ty, &num_ty, scope)
                                .map_err(|error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pat,
                                        &analysis.scrutinee,
                                    )
                                })?;
                        }
                        TextLit(_) => {
                            let text_ty = self
                                .sess
                                .require_special(Ty::Text, Some(case.pat.span))?
                                .to_item();
                            self.it_is_actual(&subject_ty, &text_ty, scope)
                                .map_err(|error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pat,
                                        &analysis.scrutinee,
                                    )
                                })?;
                        }
                        Binding(binding) => {
                            let ctor_ty = self.interp().look_up_ty(binding.0, scope).unwrap();

                            self.it_is_actual(&subject_ty, &ctor_ty, scope)
                                .map_err(|error| {
                                    self.handle_case_analysis_type_mismatch(
                                        error,
                                        &case.pat,
                                        &analysis.scrutinee,
                                    )
                                })?;
                        }
                        LetBinding(_) => {
                            // @Temporary @Beacon @Bug error prone (once we try to impl deappl)
                            // @Update @Note don't push the type of subject but the type of the binder
                            binder_tys.push(&subject_ty);
                        }
                        // @Task
                        App(app) => {
                            // @Beacon @Task check that subject type is a pi type

                            match (&app.callee.bare, &app.arg.bare) {
                                // @Note should be an error obviously but does this need to be special-cased
                                // or can we defer this to an it_is_actual call??
                                (NumLit(_) | TextLit(_), _argument) => todo!(),
                                (Binding(binding), _argument) => {
                                    let _constructor_ty =
                                        self.interp().look_up_ty(binding.0, scope).unwrap();

                                    todo!();
                                }
                                // @Task make error less fatal (keep processing next cases (match arms))
                                (LetBinding(binder), _) => {
                                    return Err(Diag::error()
                                        .code(ErrorCode::E034)
                                        .message(format!(
                                            "binder ‘{binder}’ used in callee position inside pattern",
                                        ))
                                        .unlabeled_span(binder)
                                        .help("consider referring to a concrete binding")
                                        .report(self.sess.rep())
                                        .into());
                                }
                                (App(_), _argument) => todo!(),
                                (Error(_), _) => unreachable!(),
                            };
                        }
                        Error(_) => unreachable!(),
                    }

                    let body_ty = self
                        .infer_ty_of_expr(&case.body, &scope.extend_with_pat_binders(binder_tys))?;

                    match prev_body_ty {
                        Some(ref previous_ty) => {
                            self.it_is_actual(previous_ty, &body_ty, scope)?;
                        }
                        None => {
                            prev_body_ty = Some(body_ty);
                        }
                    }
                }

                //  @Temporary unhandled case
                prev_body_ty.expect("caseless case analyses")
            }
            // @Task
            IntrApp(_) => todo!("inferring the type of intrinsic applications"),
            // @Task
            Proj(proj) => {
                let _basis_ty = self.infer_ty_of_expr(&proj.basis, scope)?;

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
            RecLit(record) => {
                // @Task check if `ty` is a record type
                // @Task check if the fields are correct (amount, names, types)

                // @Note this is so ugly!
                hir::Ident::new(
                    record.ty.bare,
                    self.sess[record.ty.bare].src.respan(record.ty.span),
                )
                .to_item()
            }
            IO(_) => self
                .sess
                .require_special(Ty::IO, Some(expr.span))?
                .to_item(),
            Error(_) => expr.clone(),
        })
    }

    fn handle_case_analysis_type_mismatch(
        &self,
        error: Error,
        pat: &hir::Pat,
        scrutinee: &Expr,
    ) -> Error {
        match error {
            TypeMismatch { expected, actual } => Diag::error()
                .code(ErrorCode::E032)
                // @Task put back some more information into the message: use `_`s to shorten the type
                .message("type mismatch")
                .span(pat, "has the wrong type")
                .label(scrutinee, "expected due to this")
                .note(format!(
                    "\
expected type ‘{}’
but got type ‘{}’",
                    self.display(&expected),
                    self.display(&actual)
                ))
                .report(self.sess.rep())
                .into(),
            error => error,
        }
    }

    /// Assert that an expression is of type `Type`.
    fn it_is_a_ty(&self, expr: &Expr, scope: &FuncScope<'_>) -> Result<(), Error> {
        let ty = self.infer_ty_of_expr(expr, scope)?;
        self.it_is_actual(
            &self.sess.require_special(Ty::Type, None)?.to_item(),
            &ty,
            scope,
        )
    }

    fn is_a_ty(&self, expr: &Expr, scope: &FuncScope<'_>) -> Result<bool, Error> {
        let ty = self.infer_ty_of_expr(expr, scope)?;
        self.is_actual(
            &self.sess.require_special(Ty::Type, None)?.to_item(),
            &ty,
            scope,
        )
        .map_err(Into::into)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn it_is_actual(
        &self,
        expected: &Expr,
        actual: &Expr,
        scope: &FuncScope<'_>,
    ) -> Result<(), Error> {
        let context = interp::Context::new(scope);
        let expected = self.interp().eval_expr(expected, context)?;
        let actual = self.interp().eval_expr(actual, context)?;

        if !self.interp().equals(&expected, &actual, scope)? {
            return Err(TypeMismatch { expected, actual });
        }

        Ok(())
    }

    fn is_actual(&self, expected: &Expr, actual: &Expr, scope: &FuncScope<'_>) -> Result<bool> {
        let cx = interp::Context::new(scope);
        let expected = self.interp().eval_expr(expected, cx)?;
        let actual = self.interp().eval_expr(actual, cx)?;

        self.interp().equals(&expected, &actual, scope)
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
    fn assert_ctor_is_instance_of_ty(&self, constructor: &Expr, ty: &Expr) -> Result {
        let codomain = constructor.innermost_codomain();
        let callee = codomain.innermost_callee();

        if self.interp().equals(ty, callee, &FuncScope::Module)? {
            Ok(())
        } else {
            Err(Diag::error()
                .code(ErrorCode::E033)
                .message(format!(
                    "‘{}’ is not an instance of ‘{}’",
                    self.display(&codomain),
                    self.display(ty),
                ))
                .unlabeled_span(codomain.span)
                .report(self.sess.rep()))
        }
    }

    fn display<'f>(&'f self, value: &'f impl Display) -> impl std::fmt::Display + 'f {
        displayed(|f| value.write(self.sess, f))
    }
}

impl Handler for &mut Typer<'_, '_> {
    fn embed<T: diagnostics::error::PossiblyErroneous>(self, diag: Diag) -> T {
        let error = diag.report(self.sess.rep());
        self.health.taint(error);
        T::error(error)
    }
}

trait ExprExt {
    /// The innermost codomain of an expression.
    ///
    /// # Example
    ///
    /// The `R` in `A -> B -> C -> R`.
    fn innermost_codomain(&self) -> Expr;

    /// The innermost callee of an expression.
    ///
    /// # Example
    ///
    /// The `f` in `f a b c`.
    fn innermost_callee(&self) -> &Expr;
}

impl ExprExt for Expr {
    fn innermost_codomain(&self) -> Expr {
        fn innermost_codomain(expr: &Expr, scope: &FuncScope<'_>) -> Expr {
            match &expr.bare {
                hir::BareExpr::PiTy(pi_ty) => {
                    let scope = if pi_ty.binder.is_some() {
                        Owned(scope.extend_with_param(&pi_ty.domain))
                    } else {
                        Borrowed(scope)
                    };

                    innermost_codomain(&pi_ty.codomain, scope.as_ref())
                }
                _ => expr.clone(),
            }
        }

        innermost_codomain(self, &FuncScope::Module)
    }

    fn innermost_callee(mut self: &Self) -> &Expr {
        loop {
            self = match &self.bare {
                hir::BareExpr::App(app) => &app.callee,
                _ => return self,
            }
        }
    }
}

#[derive(Default)]
struct Context {
    owning_data_ty: Option<Ident>,
}

fn missing_annotation_error() -> Diag {
    // @Task span
    Diag::error()
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
        expected: Expr,
        actual: Expr,
    },
}

use Error::*;

use crate::interp::Substitute;

impl From<ErasedReportedError> for Error {
    fn from(error: ErasedReportedError) -> Self {
        Self::Erased(error)
    }
}
