//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

use std::{collections::HashMap, fmt, rc::Rc};

use crate::hir::{self, Expression, Identifier, RefreshState};
use crate::parser::Explicitness;

// pub struct Module {
//     // pub name: Variable,
//     pub declarations: Vec<Declaration>,
//     // used for recursion: @Question Vec<Variable>? bc `'let '_: T = v`?
//     binders: Vec<String>,
//     #[cfg(debug_assertions)]
//     registered_binders: bool,
// }

// impl Module {
//     pub fn new(declarations: Vec<Declaration>) -> Self {
//         Self {
//             binders: Vec::with_capacity(declarations.len()),
//             declarations,
//             #[cfg(debug_assertions)]
//             registered_binders: false,
//         }
//     }

//     // used for recursion
//     // @Question move into constructor?
//     // fn register_binders(&mut self) -> Result<()> {
//     //     #[cfg(debug_assertions)]
//     //     assert!(!self.registered_binders);

//     //     for declaration in &self.declarations {
//     //         match declaration {
//     //             Declaration::Data {
//     //                 binder,
//     //                 constructors,
//     //                 ..
//     //             } => {
//     //                 let binder = binder.view_str().unwrap().to_string();
//     //                 Self::register_binder(&mut self.binders, binder)?;
//     //                 for constructor in constructors {
//     //                     Self::register_binder(
//     //                         &mut self.binders,
//     //                         constructor.binder.view_str().unwrap().to_string(),
//     //                     )?;
//     //                 }
//     //             }
//     //             Declaration::Let { binder, .. } => {
//     //                 Self::register_binder(
//     //                     &mut self.binders,
//     //                     binder.view_str().unwrap().to_string(),
//     //                 )?;
//     //             }
//     //             // is a temp on Declaration
//     //             Declaration::ExprStmt(..) => unreachable!(),
//     //         }
//     //     }

//     //     #[cfg(debug_assertions)]
//     //     {
//     //         self.registered_binders = true;
//     //     }
//     //     Ok(())
//     // }

//     fn register_binder(binders: &mut Vec<String>, binder: String) -> Result<()> {
//         if binders.contains(&binder) {
//             // @Task more information
//             Err(Error::AlreadyDefined)
//         } else {
//             binders.push(binder);
//             Ok(())
//         }
//     }

//     // @Question good naming scheme?
//     // @Note evaluate needs to take a reference to self.binders @Task to allow for recursion
//     // @Note currently `evaluate` means registering bindings in the context and also if
//     // we encounter the "declaration" expr_stmt, we evaluate it (a sort-of main function)
//     // @Note in future, the repl might evaluate expressions as `'let '_: 'hole 'infer = expr`
//     // or we just expose a better evaluate function
//     // @Note once we implement D/E-modes into repl, the signature will change to
//     // fn(&self, context: Context, state: State<'_>) -> Result<()> or
//     // if we switch to immutable contexts (which includes State):
//     // fn(&self, context: ImmContext) -> Result<ImmContext>
//     pub fn evaluate(&self, context: MutCtx, state: State<'_>) -> Result<Option<(Expr, Expr)>> {
//         Ok(self
//             .declarations
//             .iter()
//             .map(|declaration| declaration.evaluate(context.clone(), state))
//             .last()
//             .transpose()?
//             .flatten())
//     }
// }

// impl Declaration {
//     // @Temporary glue code @Question or not temporary?
//     // @Note loss of information: implicits
//     // @Beacon @Beacon @Beacon @Bug we mis-transpile from hir:
//     // we don't get Eq correctly: Even if span is different, they could refer to
//     // the same name, we need `source`!!!
//     // @Beacon @Task remove
//     // pub fn from_hir(declaration: hir::Declaration) -> Self {
//     //     match declaration {
//     //         hir::Declaration::Let {
//     //             binder,
//     //             type_annotation,
//     //             expression,
//     //         } => Self::Let {
//     //             binder: Variable::Identifier(binder),
//     //             type_: Expr::from_hir(type_annotation),
//     //             expr: Expr::from_hir(expression),
//     //         },
//     //         hir::Declaration::Data {
//     //             binder,
//     //             type_annotation,
//     //             constructors,
//     //         } => unimplemented!(),
//     //         _ => unimplemented!(),
//     //     }
//     // }

//     // used for recursion
//     // @Note currently disallows declarations where the binding is recursive in its type signature
//     // the only example that currently makes sense to me is `'let A: A = 'Type` but it's not useful
//     // at all and only exist because of the special `the Type' Type'`.
//     // Are there any expressions that a well-typed if they recurse in their type??
//     // name: "recursion in type annotation"
//     // @Bug unusable
//     pub fn register(&self, context: MutCtx, state: State<'_>) -> Result<()> {
//         unreachable!()
//         // Ok(match self {
//         //     Declaration::Let { binder, type_, .. } => {
//         //         // @Task verify type_
//         //         context.insert_unresolved_binding(binder, type_)?;
//         //     }
//         //     Declaration::Data { binder, type_, constructors } => {
//         //         // @Task verify type_
//         //         context.insert_unresolved_binding(binder, type_)?;
//         //         // @Question should we register those????
//         //         // I think, we don't want to allow "recursion in type annotation"
//         //         //for constructor in constructors {
//         //         //    context.insert_unresolved_binding(constructor.binder, constructor);
//         //         //}
//         //     }
//         //     Declaration::ExprStmt(..) => unreachable!(),
//         // })
//     }

//     // @Task make pure returning new Context
//     // @Task change name to register2 b.c. we are a lazy lang, we should only do type_checking here
//     pub fn evaluate(&self, context: MutCtx, state: State<'_>) -> Result<Option<(Expr, Expr)>> {
//         Ok(match self {
//             Declaration::Let {
//                 binder,
//                 type_,
//                 expr,
//             } => {
//                 // @Note because we cannot shadow in this context yet
//                 if context.clone().contains(&binder) {
//                     return Err(Error::AlreadyDefined);
//                 }
//                 // @Task @Beacon @Bug check equality infered and given type
//                 // @Update @Question ^^^^^^ what did I mean by this message above?
//                 let infered_type = expr.infer_type(context.clone(), state)?;
//                 let value = expr.normalize(context.clone(), state)?;
//                 context.insert_binding(binder.clone(), infered_type, value);
//                 None
//             }
//             Declaration::Data {
//                 binder: adt_binder,
//                 type_,
//                 constructors,
//             } => {
//                 // @Temporary
//                 if context.clone().contains(&adt_binder) {
//                     return Err(Error::AlreadyDefined);
//                 }
//                 type_.check_is_type(context.clone(), state)?;
//                 context
//                     .clone()
//                     .insert_adt(adt_binder.clone(), type_.clone());

//                 for Constructor { binder, type_ } in constructors {
//                     type_.check_is_type(context.clone(), state)?;
//                     // @Task instance check etc
//                     // @Task check if constructor already exists
//                     context
//                         .clone()
//                         .insert_constructor(binder.clone(), type_.clone(), adt_binder);
//                 }

//                 None
//             }
//         })
//     }
// }

// @Task create Value type
// @Task move below impl-block
// pub enum _Value {
//     // NeutralVariable(Variable)
// // Type,
// // Pi,
// // Lambda, // NeutralTerm
// // NeutralApplication(Abstraction)
// //
// }

// @Task replace Environment with a single binding
// @Task optimize body (abstraction_*-calls)
fn substitute(expression: &Expression, env: Environment, state: RefreshState<'_>) -> Expression {
    match expression {
        Expression::Identifier(identifier) => env
            .get(identifier)
            .cloned()
            .unwrap_or_else(|| expression.clone()),
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_substitute(binder.as_ref().unwrap(), parameter, expression, env, state);
            Expression::PiLiteral {
                binder: Some(binder),
                parameter: Box::new(parameter),
                expression: Box::new(expression),
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_substitute(binder, parameter.as_ref().unwrap(), expression, env, state);
            Expression::LambdaLiteral {
                binder,
                parameter: Some(Box::new(parameter)),
                expression: Box::new(expression),
                type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression,
            argument,
            explicitness: _,
        } => Expression::Application {
            expression: Box::new(substitute(expression, env.clone(), state)),
            argument: Box::new(substitute(argument, env, state)),
            // @Temporary
            explicitness: Explicitness::Explicit,
        },
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    }
}

pub fn infer_type(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<hir::Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .lookup_type(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?,
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral { .. } => Expression::TypeLiteral,
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            // @Task
            explicitness: _,
            type_annotation: _,
        } => {
            // @Bug unhandled @Temporary unwrap
            let parameter: &Expression = parameter.as_ref().unwrap();
            check_is_type(&parameter, context.clone(), state)?;
            let expression = infer_type(
                expression,
                context.extend_with_neutral_binding(binder.clone(), parameter.clone()),
                state,
            )?;
            Expression::PiLiteral {
                binder: Some(binder.clone()),
                parameter: Box::new(parameter.clone()),
                expression: Box::new(expression),
                // @Task
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::Application {
            expression: function,
            argument,
            // @Task
            explicitness: _,
        } => {
            let (pi_binder, pi_parameter, pi_expression) =
                infer_pi(function, context.clone(), state)?;
            let argument_type = infer_type(argument, context.clone(), state)?;
            check_equal(&pi_parameter, &argument_type, context, state)?;
            substitute(
                &pi_expression,
                {
                    let mut env = HashMap::new();
                    env.insert(pi_binder, argument.as_ref().clone());
                    Rc::new(env)
                },
                state,
            )
        },
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

// @Question better name available?
// @Note @Bug we don't have a direct equivalent of Abstraction in hir
// @Task translate Abstraction into hir (maybe we don't need this function anymore?)
// @Temporary signature
fn infer_pi(
    expression: &Expression,
    ctx: MutCtx,
    state: RefreshState<'_>,
) -> Result<(Identifier, Expression, Expression)> {
    let ty = infer_type(expression, ctx.clone(), state)?;
    match normalize(&ty, ctx, state)? {
        // @Bug unchecked @Temporary unwrap
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => Ok((binder.unwrap(), *parameter, *expression)),
        expression => Err(Error::FunctionExpected(expression)),
    }
}

// @Question take expression by value?
pub fn normalize(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<Expression> {
    Ok(match expression {
        Expression::Identifier(identifier) => context
            .clone()
            .lookup_value(identifier)
            .ok_or_else(|| Error::UndefinedBinding(identifier.clone()))?
            .map(|expression| normalize(&expression, context, state))
            .unwrap_or_else(|| Ok(expression.clone()))?,
        Expression::Application {
            expression: expr1,
            argument: expr2,
            // @Task
            explicitness: _,
        } => {
            let expr2 = normalize(expr2, context.clone(), state)?;
            match normalize(expr1, context, state)? {
                Expression::LambdaLiteral {
                    binder,
                    parameter,
                    expression,
                    type_annotation: _,
                    explicitness: _,
                } => substitute(
                    &expression,
                    {
                        let mut env = HashMap::new();
                        env.insert(binder, expr2);
                        Rc::new(env)
                    },
                    state,
                ),
                expr1 => Expression::Application {
                    expression: Box::new(expr1),
                    argument: Box::new(expr2),
                    // @Temporary
                    explicitness: Explicitness::Explicit,
                },
            }
        }
        Expression::TypeLiteral => Expression::TypeLiteral,
        Expression::PiLiteral {
            binder,
            parameter,
            expression,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_normalize(binder.as_ref().unwrap(), parameter, expression, context, state)?;
            Expression::PiLiteral {
                binder: Some(binder),
                parameter: Box::new(parameter),
                expression: Box::new(expression),
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        }
        Expression::LambdaLiteral {
            binder,
            parameter,
            expression,
            type_annotation: _,
            explicitness: _,
        } => {
            // @Bug unchecked @Temporary unwrap
            let (binder, parameter, expression) =
                abstraction_normalize(binder, parameter.as_ref().unwrap(), expression, context, state)?;
            Expression::LambdaLiteral {
                binder,
                parameter: Some(Box::new(parameter)),
                expression: Box::new(expression),
                type_annotation: None,
                // @Temporary
                explicitness: Explicitness::Explicit,
            }
        },
        Expression::Hole(_) => unimplemented!(),
        Expression::UseIn => unimplemented!(),
        Expression::Case => unimplemented!(),
    })
}

pub fn check_is_type(
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    let type_ = infer_type(expression, context.clone(), state)?;
    check_equal(&type_, &Expression::TypeLiteral, context, state)?;
    Ok(())
}

fn check_equal(
    left: &Expression,
    right: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<()> {
    if !normalizing_equal(left, right, context, state)? {
        Err(Error::ExpressionsNotEqual(left.clone(), right.clone()))
    } else {
        Ok(())
    }
}

fn normalizing_equal(
    left: &Expression,
    right: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<bool> {
    Ok(equal(
        &normalize(left, context.clone(), state)?,
        &normalize(right, context.clone(), state)?,
        context,
        state,
    ))
}

fn equal(expr1: &Expression, expr2: &Expression, context: MutCtx, state: RefreshState<'_>) -> bool {
    match (expr1, expr2) {
        (Expression::Identifier(left), Expression::Identifier(right)) => left == right,
        (
            Expression::Application {
                expression: left_expression,
                argument: left_argument,
                explicitness: _,
            },
            Expression::Application {
                expression: right_expression,
                argument: right_argument,
                explicitness: _,
            },
        ) => {
            equal(left_expression, right_expression, context.clone(), state)
                && equal(left_argument, right_argument, context, state)
        }
        (Expression::TypeLiteral, Expression::TypeLiteral) => true,
        (
            Expression::PiLiteral {
                binder: binder1,
                parameter: parameter1,
                expression: expression1,
                explicitness: _,
            },
            Expression::PiLiteral {
                binder: binder2,
                parameter: parameter2,
                expression: expression2,
                explicitness: _,
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                binder1.as_ref().unwrap(),
                parameter1,
                expression1,
                binder2.as_ref().unwrap(),
                parameter2,
                expression2,
                context,
                state,
            )
        }
        (
            Expression::LambdaLiteral {
                binder: binder1,
                parameter: parameter1,
                expression: expression1,
                type_annotation: _,
                explicitness: _,
            },
            Expression::LambdaLiteral {
                binder: binder2,
                parameter: parameter2,
                expression: expression2,
                type_annotation: _,
                explicitness: _,
            },
        ) => {
            // @Bug unchecked @Temporary unwrap
            abstraction_equal(
                binder1,
                parameter1.as_ref().unwrap(),
                expression1,
                binder2,
                parameter2.as_ref().unwrap(),
                expression2,
                context,
                state,
            )
        }
        _ => false,
    }
}

// @Task impl; move it to future Value
// @Note @Temporary panics if self not a type
// @Note panics if a binder does not exist
// fn inhabited(&self, context: MutCtx) -> Result<bool> {
//     Ok(match self {
//         Expr::Type => true,
//         Expr::Pi(Abstraction { input, output, .. }) => {
//             // @Bug crashes on e.g. `(-> A # A)` because we need to extend
//             // context
//             output.inhabited(context.clone())? || !input.inhabited(context)?
//         }
//         // @Note because we treat Self=Expr as Self=Value, this is a neutral Var
//         // so its value (repr) is None. It cannot be a parameter, so ist must be
//         // an ADT i think
//         Expr::Var(binding) => {
//             if context.clone().adt(binding) {
//                 let constructors = context.clone().constructors(binding);
//                 // @Note sad: bc of error handling, we cannot use `all` but
//                 // only `try_fold`
//                 !constructors.is_empty()
//                     && constructors
//                         .into_iter()
//                         .try_fold(true, |acc, constructor| {
//                             Ok(acc
//                                 && context
//                                     .clone()
//                                     .lookup_type(&constructor)
//                                     .unwrap()
//                                     .inhabited(context.clone())?)
//                         })?
//             } else {
//                 // constructor @Bug not always a constructor but also neutral variable
//                 // @Note not extensible, hacky
//                 return Err(Error::ExpressionsNotEqual(
//                     Expr::Type,
//                     context.lookup_type(binding).unwrap(),
//                 ));
//             }
//         }
//         // @Bug hit: neutral application, e.g. `Maybe` (after lookup)
//         x => {
//             // @Temporary
//             eprintln!("{:?}", x);
//             unreachable!()
//         }
//     })
// }

// @Task impl; move to Value
// fn instance() {}

// @Temporary signature
fn abstraction_substitute(
    binder: &Identifier,
    parameter: &Expression,
    expression: &Expression,
    env: Environment,
    state: RefreshState<'_>,
) -> (Identifier, Expression, Expression) {
    let binder = binder.refresh(state);
    (
        binder.clone(), // @Note redundant
        substitute(parameter, env.clone(), state),
        substitute(
            expression,
            {
                let mut env = env.as_ref().clone();
                env.insert(binder.clone(), Expression::Identifier(binder));
                Rc::new(env)
            },
            state,
        ),
    )
}

// @Temporary signature
fn abstraction_normalize(
    binder: &Identifier,
    parameter: &Expression,
    expression: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> Result<(Identifier, Expression, Expression)> {
    let parameter = normalize(parameter, context.clone(), state)?;
    Ok((
        binder.clone(),    // @Note redundant
        parameter.clone(), // @Note redundant
        normalize(
            expression,
            context.extend_with_neutral_binding(binder.clone(), parameter),
            state,
        )?,
    ))
}

// @Temporary signature
fn abstraction_equal(
    binder1: &Identifier,
    parameter1: &Expression,
    expression1: &Expression,
    binder2: &Identifier,
    parameter2: &Expression,
    expression2: &Expression,
    context: MutCtx,
    state: RefreshState<'_>,
) -> bool {
    equal(parameter1, parameter2, context.clone(), state)
        && equal(
            &expression1,
            &substitute(
                expression2,
                {
                    let mut env = HashMap::new();
                    env.insert(binder2.clone(), Expression::Identifier(binder1.clone()));
                    Rc::new(env)
                },
                state,
            ),
            context,
            state,
        )
}

pub use context::MutCtx;

mod context {
    use super::{Expression, Identifier};
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    // @Note expressions aren't the only entities: modules are too, gonna be added later
    // @Note repr gonna change once we allow overloading (expression,module)
    #[derive(Clone, Debug)]
    enum Entity {
        // used for recursion
        UnresolvedExpression { type_: Expression },
        // @Note technically, an ADT is a neutral expression, too @Question better naming?
        // @Note formal parameters
        NeutralExpression { type_: Expression },
        ResolvedExpression { type_: Expression, expr: Expression },
        // @Question additional payload? constructors?
        AlgebraicDataType { type_: Expression },
    }

    impl Entity {
        // @Note returns Option because UnresolvedExpression does not have a known type_
        // in our current model
        fn retrieve_type(&self) -> &Expression {
            match self {
                Entity::UnresolvedExpression { type_ } => type_,
                Entity::NeutralExpression { type_ } => type_,
                Entity::ResolvedExpression { type_, .. } => type_,
                Entity::AlgebraicDataType { type_, .. } => type_,
            }
        }

        fn retrieve_value(&self) -> Option<&Expression> {
            if let Entity::ResolvedExpression { expr, .. } = self {
                Some(expr)
            } else {
                None
            }
        }
    }

    // @Note currently, ADTs and its constructors cannot be shadowed, but this will change.
    // Once this happens, the mutating methods on Context will be removed and the methods
    // immutably extending the Context also need to update ADTContext i.e. returning
    // (Context, ADTContext) or even better storing the latter inside the former.
    // But for now, variables refering to ADTs and constructors are global/globally unique.
    // @Note name-change from Context to MutCtx because we might want to transition to ImmCtx (later Context again)
    #[derive(Default, Clone, Debug)]
    pub struct MutCtx {
        // @Note I know that should merge the two Rc'ed RcCells into one
        // @Task @Question add binders: Vec<Variable>?
        bindings: Rc<RefCell<HashMap<Identifier, Entity>>>,
        adts: Rc<RefCell<HashMap<Identifier, Vec<Identifier>>>>,
    }

    impl MutCtx {
        pub fn contains(self, binding: &Identifier) -> bool {
            self.bindings.borrow().contains_key(binding)
        }

        pub fn adt(self, binding: &Identifier) -> bool {
            self.adts.borrow().contains_key(binding)
        }

        // @Note assumes binding is indeed an ADT @Note panics
        // @Note ugly: clones ... I just want to introspect the value
        // ... CPS would work here
        pub fn constructors(self, binding: &Identifier) -> Vec<Identifier> {
            self.adts.borrow()[binding].clone()
        }

        pub fn lookup_type(self, binding: &Identifier) -> Option<Expression> {
            self.bindings
                .borrow()
                .get(binding)
                .map(Entity::retrieve_type)
                .cloned()
        }

        pub fn lookup_value(self, binding: &Identifier) -> Option<Option<Expression>> {
            self.bindings
                .borrow()
                .get(binding)
                .map(|entity| entity.retrieve_value().cloned())
        }

        #[must_use]
        pub fn extend_with_binding(
            self,
            binding: Identifier,
            type_: Expression,
            value: Expression,
        ) -> Self {
            let mut map = self.bindings.as_ref().borrow().clone();
            map.insert(binding, Entity::ResolvedExpression { type_, expr: value });
            MutCtx {
                bindings: Rc::new(RefCell::new(map)),
                adts: self.adts,
            }
        }

        #[must_use]
        pub fn extend_with_neutral_binding(self, binding: Identifier, type_: Expression) -> Self {
            let mut map = self.bindings.as_ref().borrow().clone();
            map.insert(binding, Entity::NeutralExpression { type_ });
            MutCtx {
                bindings: Rc::new(RefCell::new(map)),
                adts: self.adts,
            }
        }

        /*
        // @Note important: see note above Context before impl'ing this
        pub fn extend_with_adt(self, binder: Variable: type_: Expr, xxx) -> Self {

        }*/

        // @Task remove: We should only use extend_with_bindings even in the REPL
        pub fn insert_binding(self, binding: Identifier, type_: Expression, value: Expression) {
            self.bindings
                .borrow_mut()
                .insert(binding, Entity::ResolvedExpression { type_, expr: value });
        }

        // @Task remove
        pub fn insert_neutral_binding(self, binding: Identifier, type_: Expression) {
            self.bindings
                .borrow_mut()
                .insert(binding, Entity::NeutralExpression { type_ });
        }

        // @Task remove
        pub fn insert_adt(self, binding: Identifier, type_: Expression) {
            self.clone().insert_neutral_binding(binding.clone(), type_);
            self.adts.borrow_mut().insert(binding, Vec::new());
        }

        // @Task remove
        // @Note panics if adt does not exist
        pub fn insert_constructor(self, binding: Identifier, type_: Expression, adt: &Identifier) {
            self.clone().insert_neutral_binding(binding.clone(), type_);
            self.adts.borrow_mut().get_mut(adt).unwrap().push(binding);
        }
    }
}

pub type State<'a> = &'a mut u32;

// @Note currently, the whole code base only constructs Environments with one entry
// Possibly, a later blog post expands on this, otherwise @Task replace with tuple
type Environment = Rc<HashMap<Identifier, Expression>>;

pub fn initial() -> (MutCtx, u32) {
    (Default::default(), 0)
}

type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    UndefinedBinding(Identifier),
    FunctionExpected(Expression),
    ExpressionsNotEqual(Expression, Expression),
    AlreadyDefined,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::UndefinedBinding(binding) => write!(f, "undefined binding `{}`", binding),
            Error::FunctionExpected(actual) => write!(f, "expected function, got `{}`", actual),
            Error::ExpressionsNotEqual(expected, actual) => {
                write!(f, "expected `{}` got `{}`", expected, actual)
            }
            Error::AlreadyDefined => write!(f, "already defined"),
        }
    }
}
