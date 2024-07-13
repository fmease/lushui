//! The HIRI – [HIR](hir) interpreter.
//!
//! A tree-walk interpreter. Used during type-checking.
//!
//! # Issues
//!
//! * too many bugs
//! * full case analysis not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * does not use weak-head normal form

// @Beacon @Task use weak-normal form extensively again!!!

use super::{missing_annotation_error, Expr};
use diagnostics::{
    error::{PossiblyErroneous, Result},
    reporter::ErasedReportedError,
    Diag,
};
use hir::{interfaceable, special::Ty, Attrs, DeBruijnIdx, Ident, Subst::Shift, ValueView};
use hir_format::Display;
use resolver::ProgramEntryExt;
use session::{interfaceable::InterfaceableBindingExt, Session};
use std::fmt;
use utility::debugged;

/// Run the entry point of the given executable component.
pub fn eval_main_func(sess: &Session<'_>) -> Result<Expr> {
    Interp::new(sess).eval_expr(
        &sess.look_up_program_entry().unwrap().to_item(),
        Context::new(&FuncScope::Module),
    )
}

pub(crate) struct Interp<'a> {
    // @Task add recursion depth
    sess: &'a Session<'a>,
}

impl<'a> Interp<'a> {
    pub(super) fn new(sess: &'a Session<'a>) -> Self {
        Self { sess }
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    pub(crate) fn eval_expr(&self, expr: &Expr, cx: Context<'_>) -> Result<Expr> {
        use hir::{BareExpr::*, Subst::*};

        // @Bug we currently don't support zero-arity intrinsic functions
        Ok(match &expr.bare {
            Binding(binding) if self.sess.specials().is(binding.0, Ty::Type) => expr.clone(),
            Binding(binding) => {
                match self.look_up_value(binding.0) {
                    // @Question is this normalization necessary? I mean, yes, we got a new scope,
                    // but the thing in the previous was already normalized (well, it should have been
                    // at least). I guess it is necessary because it can contain parameters which could not
                    // be resolved yet but potentially can be now.
                    ValueView::Reducible(expression) => self.eval_expr(&expression, cx)?,
                    ValueView::Neutral => expr.clone(),
                }
            }
            App(app) => {
                let callee = self.eval_expr(&app.callee, cx)?;
                let arg = app.arg.clone();
                match callee.bare {
                    LamLit(lambda) => {
                        // @Bug because we don't reduce to weak-head normal form anywhere,
                        // diverging expressions are still going to diverge even if "lazy"/
                        // auto-closured to thunks since the body always gets evaluated to
                        // normal form
                        // @Beacon @Beacon @Beacon @Task re-introduce `lazy` via `@lazy`
                        // let argument = if lambda.laziness.is_some() {
                        //     Expr::new(
                        //         default(),
                        //         argument.span,
                        //         hir::Lambda {
                        //             // @Bug we should not create a fake identifier at all!!
                        //             // @Note this problem is solved once we allow identifier to be
                        //             // an Option<_> (that's gonna happen when we finally implement
                        //             // the discarding identifier `_`)
                        //             parameter: Identifier::parameter("__"),
                        //             parameter_type_annotation: Some(
                        //                 self.session.require_special(Type::Unit, None)?,
                        //             ),
                        //             body_type_annotation: None,
                        //             body: Expr::new(
                        //                 default(),
                        //                 default(),
                        //                 hir::Substituted {
                        //                     substitution: Shift(1),
                        //                     expression: argument,
                        //                 }
                        //                 .into(),
                        //             ),
                        //             explicitness: Explicit,
                        //             laziness: None,
                        //         }
                        //         .into(),
                        //     )
                        // } else {
                        //     argument
                        // };

                        self.eval_expr(
                            &Expr::bare(
                                hir::Substed {
                                    subst: Use(Box::new(Shift(0)), arg),
                                    expr: lambda.body.clone(),
                                }
                                .into(),
                            ),
                            cx,
                        )?
                    }
                    Binding(binding) if self.is_intr_func(binding.0) => self.eval_expr(
                        &Expr::new(
                            expr.attrs.clone(),
                            expr.span,
                            hir::IntrApp {
                                callee: binding.0,
                                args: vec![arg],
                            }
                            .into(),
                        ),
                        cx,
                    )?,
                    Binding(_) | App(_) => Expr::new(
                        expr.attrs.clone(),
                        expr.span,
                        hir::App {
                            callee,
                            // argument: match context.form {
                            //     Form::Normal => argument.evaluate_expression(context)?,
                            //     Form::WeakHeadNormal => argument,
                            // },
                            arg: self.eval_expr(&arg, cx)?,
                            kind: hir::ParamKind::Explicit,
                        }
                        .into(),
                    ),
                    IntrApp(app) => self.eval_expr(
                        &Expr::new(
                            expr.attrs.clone(),
                            expr.span,
                            hir::IntrApp {
                                callee: app.callee,
                                args: {
                                    let mut args = app.args.clone();
                                    args.push(arg);
                                    args
                                },
                            }
                            .into(),
                        ),
                        cx,
                    )?,
                    _ => unreachable!(),
                }
            }
            NumLit(_) | TextLit(_) | IO(_) => expr.clone(),
            RecLit(_) => todo!("evaluating records"),
            Proj(_) => todo!("evaluating projections"),
            PiTy(pi_ty) => match cx.form {
                Form::Normal => {
                    let domain = self.eval_expr(&pi_ty.domain, cx)?;

                    let codomain = if pi_ty.binder.is_some() {
                        // @Beacon @Question whyy do we need type information here in *evaluate*???
                        let scope = cx.scope.extend_with_param(&domain);
                        self.eval_expr(&pi_ty.codomain, cx.with_scope(&scope))?
                    } else {
                        self.eval_expr(&pi_ty.codomain, cx)?
                    };

                    Expr::new(
                        expr.attrs.clone(),
                        expr.span,
                        hir::PiTy {
                            kind: pi_ty.kind,
                            binder: pi_ty.binder,
                            domain,
                            codomain,
                        }
                        .into(),
                    )
                }
                Form::WeakHeadNormal => expr.clone(),
            },
            LamLit(lambda) => match cx.form {
                Form::Normal => {
                    let domain = self.eval_expr(
                        lambda
                            .domain
                            .as_ref()
                            .ok_or_else(|| missing_annotation_error().report(self.sess.rep()))?,
                        cx,
                    )?;
                    let codomain = lambda
                        .codomain
                        .as_ref()
                        .map(|ty| {
                            // @Beacon @Question why do we need type information here in *evaluate*???
                            let scope = cx.scope.extend_with_param(&domain);
                            self.eval_expr(ty, cx.with_scope(&scope))
                        })
                        .transpose()?;
                    // @Beacon @Question why do we need type information here in *evaluate*???
                    let scope = cx.scope.extend_with_param(&domain);
                    let body = self.eval_expr(&lambda.body, cx.with_scope(&scope))?;

                    Expr::new(
                        expr.attrs.clone(),
                        expr.span,
                        hir::LamLit {
                            binder: lambda.binder,
                            domain: Some(domain),
                            body,
                            codomain,
                            kind: hir::ParamKind::Explicit,
                        }
                        .into(),
                    )
                }
                Form::WeakHeadNormal => expr.clone(),
            },
            Substed(substed) => {
                let expr = substed.expr.subst(&substed.subst);
                self.eval_expr(&expr, cx)?
            }
            // @Note partially applied constructors differ from normal values
            // I guess it's very likely that the first code we write will handle them incorrectly
            // because the code will not check for the arity of the neutral application
            // @Note how to handle them: just like functions: case analysis only wotks with a binder-case
            // (default case)
            // @Bug panics if the subject reduces to a neutral identifier (i.e. lambda parameter)
            // contains one
            CaseAnalysis(analysis) => {
                let scrutinee = self.eval_expr(&analysis.scrutinee, cx)?;

                // @Note we assume, subject is composed of only applications, bindings etc corresponding to the pattern types
                // everything else should be impossible because of type checking but I might be wrong.
                // possible counter examples: unevaluated case analysis expression
                // @Note @Beacon think about having a variable `matches: bool` (whatever) to avoid repetition
                match scrutinee.bare {
                    Binding(scrutinee) => {
                        // @Temporary hack (bc we do not follow any principled implementation right now):
                        // a case analysis is indirectly neutral if the subject is a neutral binding
                        if self.look_up_value(scrutinee.0).is_neutral() {
                            return Ok(Expr::new(
                                expr.attrs.clone(),
                                expr.span,
                                hir::CaseAnalysis {
                                    scrutinee: scrutinee.0.to_item(),
                                    cases: analysis.cases.clone(),
                                }
                                .into(),
                            ));
                        }

                        for case in &analysis.cases {
                            use hir::BarePat::*;

                            match &case.pat.bare {
                                Binding(binding) => {
                                    if binding.0 == scrutinee.0 {
                                        // @Task @Beacon extend with parameters when evaluating
                                        return self.eval_expr(&case.body, cx);
                                    }
                                }
                                NumLit(_) | TextLit(_) | LetBinding(_) | App(_) => todo!(),
                                Error(_) => unreachable!(),
                            }
                        }

                        // we should not be here
                        // @Note this is currently reachable because we don't do a check for
                        // exhaustiveness in `infer_ty`, just fyi
                        unreachable!()
                    }
                    // @Beacon @Task
                    App(_app) => todo!(),
                    NumLit(num0) => {
                        for case in &analysis.cases {
                            use hir::BarePat::*;

                            match &case.pat.bare {
                                NumLit(num1) => {
                                    if &num0 == num1 {
                                        return self.eval_expr(&case.body, cx);
                                    }
                                }
                                LetBinding(_) => {
                                    // @Beacon @Question whyy do we need type information here in *evaluate*???
                                    // @Task get rid of it as well as the call to new_unchecked
                                    let error = PossiblyErroneous::error(
                                        ErasedReportedError::new_unchecked(),
                                    );
                                    let scope = cx.scope.extend_with_param(&error);

                                    return self.eval_expr(&case.body, cx.with_scope(&scope));
                                }
                                TextLit(_) | Binding(_) | App(_) => todo!(),
                                Error(_) => unreachable!(),
                            }
                        }
                        // we should not be here
                        // @Note this is currently reachable because we don't do a check for
                        // exhaustiveness in `infer_ty`, just fyi
                        unreachable!()
                    }
                    // @Note reachable if they contain neutrals, right??
                    _ => unreachable!(),
                }
            }
            IntrApp(app) => {
                let args = app
                    .args
                    .iter()
                    .map(|argument| self.eval_expr(argument, cx))
                    .collect::<Result<Vec<_>, _>>()?;
                self.apply_intr_func(app.callee, args.clone())?
                    .unwrap_or_else(|| {
                        Expr::new(
                            expr.attrs.clone(),
                            expr.span,
                            hir::IntrApp {
                                callee: app.callee,
                                args,
                            }
                            .into(),
                        )
                    })
            }
            &Error(error) => PossiblyErroneous::error(error),
        })
    }

    /// Try applying an intrinsic function.
    ///
    /// # Panics
    ///
    /// Panics if `binder` is either not bound or not an intrinsic.
    // @Task correctly handle
    // * pure vs impure
    // * polymorphism
    // * (unexpected) neutrals
    // * types (arguments of type `Type`): skip them
    // @Note: we need to convert to be able to convert to ffi::Value
    pub(crate) fn apply_intr_func(&self, binder: Ident, args: Vec<Expr>) -> Result<Option<Expr>> {
        match self.sess[binder.decl_idx().unwrap()].kind {
            hir::EntityKind::FuncIntr { func: function, .. } => {
                Ok(if args.len() == function.arity() {
                    let mut value_args = Vec::new();

                    for arg in args {
                        if let Some(arg) = interfaceable::Value::from_expr(&arg, self.sess) {
                            value_args.push(arg);
                        } else {
                            return Ok(None);
                        }
                    }

                    Some(function.eval(value_args).into_expr(self.sess)?)
                } else {
                    None
                })
            }
            _ => unreachable!(),
        }
    }

    // @Question move into its own module?
    #[allow(clippy::unused_self)]
    fn _is_ffi_compatible(&self, _: &Expr) -> bool {
        todo!() // @Task
    }

    /// Dictates if two expressions are alpha-equivalent.
    // @Task write a unifier
    // @Task rename to expressions_equal
    pub(crate) fn equals(&self, expr0: &Expr, expr1: &Expr, scope: &FuncScope<'_>) -> Result<bool> {
        use hir::BareExpr::*;

        Ok(match (&expr0.bare, &expr1.bare) {
            (Binding(binding0), Binding(binding1)) => binding0.0 == binding1.0,
            (App(app0), App(app1)) => {
                self.equals(&app0.callee, &app1.callee, scope)?
                    && self.equals(&app0.arg, &app1.arg, scope)?
            }
            (NumLit(num0), NumLit(num1)) => num0 == num1,
            (TextLit(text0), TextLit(text1)) => text0 == text1,
            // @Question what about explicitness?
            (PiTy(pi_ty0), PiTy(pi_ty1)) => {
                self.equals(&pi_ty0.domain, &pi_ty1.domain, scope)?
                    && match (pi_ty0.binder, pi_ty1.binder) {
                        (Some(_), Some(_)) => self.equals(
                            &pi_ty0.codomain,
                            &pi_ty1.codomain,
                            &scope.extend_with_param(&pi_ty0.domain),
                        )?,
                        (Some(_), None) => {
                            let codomain1 = pi_ty1.codomain.subst(&Shift(1));
                            self.equals(
                                &codomain1,
                                &pi_ty0.codomain,
                                &scope.extend_with_param(&pi_ty0.domain),
                            )?
                        }
                        (None, Some(_)) => {
                            let codomain0 = pi_ty0.codomain.subst(&Shift(1));
                            self.equals(
                                &pi_ty1.codomain,
                                &codomain0,
                                &scope.extend_with_param(&pi_ty1.domain),
                            )?
                        }
                        (None, None) => self.equals(&pi_ty0.codomain, &pi_ty1.codomain, scope)?,
                    }
            }
            // @Question what about the codomain? what about the ParameterKind?
            (LamLit(lambda0), LamLit(lambda1)) => {
                let domain0 = lambda0
                    .domain
                    .as_ref()
                    .ok_or_else(|| missing_annotation_error().report(self.sess.rep()))?;
                let domain1 = lambda1
                    .domain
                    .as_ref()
                    .ok_or_else(|| missing_annotation_error().report(self.sess.rep()))?;

                self.equals(domain0, domain1, scope)?
                    && self.equals(
                        &lambda0.body,
                        &lambda1.body,
                        &scope.extend_with_param(domain0),
                    )?
            }
            // @Temporary implementation
            (CaseAnalysis(analysis0), CaseAnalysis(analysis1)) => {
                self.equals(&analysis0.scrutinee, &analysis1.scrutinee, scope)?
            }
            (IntrApp(app0), IntrApp(app1)) => {
                app0.callee == app1.callee
                    && app0
                        .args
                        .iter()
                        .zip(&app1.args)
                        .map(|(arg0, arg1)| self.equals(arg0, arg1, scope))
                        .try_fold(true, |all, this| Ok(all && this?))?
            }
            // @Question is that what we want or should we just evaluate again?
            (Substed(_), Substed(_)) => {
                return Err(Diag::bug()
                    .message("attempt to check two lazily substituted expressions for equivalence")
                    .note("they should not exist in this part of the code but should have already been evaluated")
                    .report(self.sess.rep()));
            }
            (Proj(_), Proj(_)) => {
                return Err(Diag::error()
                    .message("field projections are not implemented yet")
                    .unlabeled_span(expr0)
                    .unlabeled_span(expr1)
                    .report(self.sess.rep()))
            }
            // @Task probably should just be `true` once we support errors in subexpressions
            (Error(_), _) | (_, Error(_)) => {
                panic!("trying to check equality on erroneous expressions")
            }
            _ => false,
        })
    }

    pub(crate) fn look_up_value(&self, binder: Ident) -> ValueView {
        use hir::Index::*;

        match binder.idx {
            Decl(index) => self.sess[index].value(),
            DeBruijn(_) => ValueView::Neutral,
            Param => unreachable!(),
        }
    }

    pub(crate) fn look_up_ty(&self, binder: Ident, scope: &FuncScope<'_>) -> Option<Expr> {
        use hir::Index::*;

        match binder.idx {
            Decl(index) => self.sess[index].ty(),
            DeBruijn(index) => Some(scope.look_up_ty(index)),
            Param => unreachable!(),
        }
    }

    pub(crate) fn is_intr_func(&self, binder: Ident) -> bool {
        use hir::Index::*;

        match binder.idx {
            Decl(index) => self.sess[index].is_intr_func(),
            DeBruijn(_) => false,
            Param => unreachable!(),
        }
    }
}

pub(crate) trait Substitute {
    fn subst(&self, subst: &hir::Subst) -> Self;
}

impl Substitute for Expr {
    fn subst(&self, subst: &hir::Subst) -> Self {
        use hir::{BareExpr::*, Subst::*};

        #[allow(clippy::match_same_arms)] // @Temporary
        match (&self.bare, subst) {
            (Binding(binding), &Shift(amount)) => hir::Expr::new(
                self.attrs.clone(),
                self.span,
                hir::Binding(binding.0.shift(amount)).into(),
            ),
            // @Beacon @Question @Bug
            (Binding(binding), Use(substitution, expression)) => {
                if binding.0.is_innermost() {
                    expression.subst(&Shift(0))
                } else {
                    hir::Expr::new(
                        expression.attrs.clone(),
                        expression.span,
                        hir::Binding(binding.0.unshift()).into(),
                    )
                    .subst(substitution)
                }
            }
            // @Beacon @Question @Bug
            (Substed(substed), subst) => substed.expr.subst(&substed.subst).subst(subst),
            (NumLit(_) | TextLit(_), _) => self.clone(),
            (RecLit(_), _) => todo!("subst'ing records"),
            (Proj(_), _) => todo!("subst'ing projections"),
            (IO(_), _) => todo!("subst'ing effects"),
            (App(app), subst) => Expr::new(
                self.attrs.clone(),
                self.span,
                hir::App {
                    callee: Expr::bare(
                        hir::Substed {
                            expr: app.callee.clone(),
                            subst: subst.clone(),
                        }
                        .into(),
                    ),
                    arg: Expr::bare(
                        hir::Substed {
                            expr: app.arg.clone(),
                            subst: subst.clone(),
                        }
                        .into(),
                    ),
                    kind: app.kind,
                }
                .into(),
            ),
            (PiTy(pi_ty), subst) => {
                let domain = Expr::bare(
                    hir::Substed {
                        expr: pi_ty.domain.clone(),
                        subst: subst.clone(),
                    }
                    .into(),
                );

                let codomain = Expr::bare(
                    hir::Substed {
                        expr: pi_ty.codomain.clone(),
                        subst: match &pi_ty.binder {
                            Some(binder) => Use(
                                Box::new(Shift(1).compose(subst.clone())),
                                binder.to_innermost().to_item(),
                            ),
                            None => subst.clone(),
                        },
                    }
                    .into(),
                );

                Expr::new(
                    self.attrs.clone(),
                    self.span,
                    hir::PiTy {
                        kind: pi_ty.kind,
                        binder: pi_ty.binder,
                        domain,
                        codomain,
                    }
                    .into(),
                )
            }
            (LamLit(lambda), subst) => {
                let domain = lambda.domain.clone().map(|ty| {
                    Expr::bare(
                        hir::Substed {
                            expr: ty,
                            subst: subst.clone(),
                        }
                        .into(),
                    )
                });

                let codomain = lambda.codomain.clone().map(|ty| {
                    Expr::bare(
                        hir::Substed {
                            expr: ty,
                            subst: match lambda.binder {
                                Some(binder) => Use(
                                    Box::new(Shift(1).compose(subst.clone())),
                                    binder.to_innermost().to_item(),
                                ),
                                None => subst.clone(),
                            },
                        }
                        .into(),
                    )
                });

                let body = Expr::bare(
                    hir::Substed {
                        expr: lambda.body.clone(),
                        subst: match lambda.binder {
                            Some(binder) => Use(
                                Box::new(Shift(1).compose(subst.clone())),
                                binder.to_innermost().to_item(),
                            ),
                            None => subst.clone(),
                        },
                    }
                    .into(),
                );

                Expr::new(
                    self.attrs.clone(),
                    self.span,
                    hir::LamLit {
                        binder: lambda.binder,
                        domain,
                        codomain,
                        body,
                        kind: lambda.kind,
                    }
                    .into(),
                )
            }
            (CaseAnalysis(analysis), subst) => Expr::new(
                self.attrs.clone(),
                self.span,
                hir::CaseAnalysis {
                    cases: analysis
                        .cases
                        .iter()
                        .map(|case| hir::Case {
                            pat: case.pat.clone(),
                            body: Expr::bare(
                                hir::Substed {
                                    expr: case.body.clone(),
                                    subst: subst.clone(),
                                }
                                .into(),
                            ),
                        })
                        .collect(),
                    scrutinee: Expr::bare(
                        hir::Substed {
                            expr: analysis.scrutinee.clone(),
                            subst: subst.clone(),
                        }
                        .into(),
                    ),
                }
                .into(),
            ),
            (IntrApp(app), subst) => Expr::new(
                self.attrs.clone(),
                self.span,
                hir::IntrApp {
                    callee: app.callee,
                    args: app
                        .args
                        .iter()
                        .map(|arg| {
                            Expr::new(
                                arg.attrs.clone(),
                                arg.span,
                                hir::Substed {
                                    expr: arg.clone(),
                                    subst: subst.clone(),
                                }
                                .into(),
                            )
                        })
                        .collect(),
                }
                .into(),
            ),
            (&Error(error), _) => PossiblyErroneous::error(error),
        }
    }
}

trait Compose {
    fn compose(self, other: Self) -> Self;
}

impl Compose for hir::Subst {
    fn compose(self, other: Self) -> Self {
        use hir::Subst::*;

        match (self, other) {
            (subst, Shift(0)) => subst,
            (Use(subst, _), Shift(amount)) => subst.compose(Shift(amount - 1)),
            (Shift(amount0), Shift(amount1)) => Shift(amount0 + amount1),
            (subst0, Use(subst1, expr)) => Use(
                Box::new(subst0.clone().compose(*subst1)),
                Expr::bare(
                    hir::Substed {
                        subst: subst0,
                        expr,
                    }
                    .into(),
                ),
            ),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Form {
    Normal,
    #[allow(dead_code)]
    WeakHeadNormal,
}

/// Evaluation context.
// @Task a recursion_depth: usize, @Note if we do that,
// remove Copy and have a custom Clone impl incrementing the value
#[derive(Clone, Copy)]
pub(crate) struct Context<'a> {
    pub(crate) scope: &'a FuncScope<'a>,
    pub(crate) form: Form,
}

impl<'a> Context<'a> {
    /// Temporarily, for convenience and until we fix some bugs, the form
    /// is [`Form::Normal`] by default.
    pub(crate) fn new(scope: &'a FuncScope<'_>) -> Self {
        Self {
            scope,
            // @Temporary
            form: Form::Normal,
            // form: Form::WeakHeadNormal,
        }
    }
}

impl<'a> Context<'a> {
    pub(crate) fn with_scope(&self, scope: &'a FuncScope<'_>) -> Self {
        Self { scope, ..*self }
    }
}

/// A definition.
#[derive(Clone)] // @Question expensive attributes clone?
pub(crate) struct Def {
    pub(crate) attrs: Attrs,
    pub(crate) bare: BareDef,
}

#[derive(Clone)]
pub(crate) enum BareDef {
    Fn {
        binder: Ident,
        ty: Expr,
        value: Option<Expr>,
    },
    Data {
        binder: Ident,
        ty: Expr,
    },
    Ctor {
        binder: Ident,
        ty: Expr,
        owner_data_ty: Ident,
    },
    /// An intrinstic function.
    IntrFn {
        binder: Ident,
        ty: Expr,
    },
}

// only used to report "cyclic" types (currently treated as a bug)
impl Display for Def {
    fn write(&self, sess: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BareDef::*;

        match &self.bare {
            Fn { binder, ty, value } => {
                let mut compound = f.debug_struct("Value");
                compound
                    .field("binder", binder)
                    .field("type", &debugged(|f| ty.write(sess, f)));
                match value {
                    Some(value) => compound.field("value", &debugged(|f| value.write(sess, f))),
                    None => compound.field("value", &"⟨none⟩"),
                }
                .finish()
            }
            Data { binder, ty } => f
                .debug_struct("Data")
                .field("binder", binder)
                .field("type", &debugged(|f| ty.write(sess, f)))
                .finish(),
            Ctor {
                binder,
                ty,
                owner_data_ty: data,
            } => f
                .debug_struct("Constructor")
                .field("binder", binder)
                .field("type", &debugged(|f| ty.write(sess, f)))
                .field("data", data)
                .finish(),
            IntrFn { binder, ty } => f
                .debug_struct("IntrinsicFunction")
                .field("binder", binder)
                .field("type", &debugged(|f| ty.write(sess, f)))
                .finish(),
        }
    }
}

/// The scope of bindings inside of a function.
pub(crate) enum FuncScope<'a> {
    Module,
    FuncParam {
        parent: &'a Self,
        ty: &'a Expr,
    },
    PatBinders {
        parent: &'a Self,
        // @Note idk
        types: Vec<&'a Expr>,
    },
}

impl<'a> FuncScope<'a> {
    pub(crate) fn extend_with_param(&'a self, ty: &'a Expr) -> Self {
        Self::FuncParam { parent: self, ty }
    }

    pub(crate) fn extend_with_pat_binders(&'a self, types: Vec<&'a Expr>) -> Self {
        Self::PatBinders {
            parent: self,
            types,
        }
    }

    pub(super) fn look_up_ty(&self, index: DeBruijnIdx) -> Expr {
        self.look_up_ty_with_depth(index, 0)
    }

    fn look_up_ty_with_depth(&self, index: DeBruijnIdx, depth: usize) -> Expr {
        match self {
            Self::FuncParam { parent, ty } => {
                if depth == index.0 {
                    Expr::bare(
                        hir::Substed {
                            subst: Shift(depth + 1),
                            expr: (*ty).clone(),
                        }
                        .into(),
                    )
                } else {
                    parent.look_up_ty_with_depth(index, depth + 1)
                }
            }
            Self::PatBinders { parent, types } => {
                match types
                    .iter()
                    .rev()
                    .zip(depth..)
                    .find(|(_, depth)| *depth == index.0)
                {
                    Some((ty, depth)) => Expr::bare(
                        hir::Substed {
                            // @Task verify this shift
                            subst: Shift(depth + 1),
                            expr: (*ty).clone(),
                        }
                        .into(),
                    ),
                    None => parent.look_up_ty_with_depth(index, depth + types.len()),
                }
            }
            Self::Module => unreachable!(),
        }
    }
}
