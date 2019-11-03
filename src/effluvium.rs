//! First type-checker plus backend (tree-walk interpreter).
//!
//! The code in here is all temporary and needs to be reworked quite a bit!

use std::{collections::HashMap, fmt, rc::Rc};

use crate::hir;

pub struct Module {
    // pub name: Variable,
    pub declarations: Vec<Declaration>,
    // used for recursion: @Question Vec<Variable>? bc `'let '_: T = v`?
    binders: Vec<String>,
    #[cfg(debug_assertions)]
    registered_binders: bool,
}

impl Module {
    pub fn new(declarations: Vec<Declaration>) -> Self {
        Self {
            binders: Vec::with_capacity(declarations.len()),
            declarations,
            #[cfg(debug_assertions)]
            registered_binders: false,
        }
    }

    // used for recursion
    // @Question move into constructor?
    // fn register_binders(&mut self) -> Result<()> {
    //     #[cfg(debug_assertions)]
    //     assert!(!self.registered_binders);

    //     for declaration in &self.declarations {
    //         match declaration {
    //             Declaration::Data {
    //                 binder,
    //                 constructors,
    //                 ..
    //             } => {
    //                 let binder = binder.view_str().unwrap().to_string();
    //                 Self::register_binder(&mut self.binders, binder)?;
    //                 for constructor in constructors {
    //                     Self::register_binder(
    //                         &mut self.binders,
    //                         constructor.binder.view_str().unwrap().to_string(),
    //                     )?;
    //                 }
    //             }
    //             Declaration::Let { binder, .. } => {
    //                 Self::register_binder(
    //                     &mut self.binders,
    //                     binder.view_str().unwrap().to_string(),
    //                 )?;
    //             }
    //             // is a temp on Declaration
    //             Declaration::ExprStmt(..) => unreachable!(),
    //         }
    //     }

    //     #[cfg(debug_assertions)]
    //     {
    //         self.registered_binders = true;
    //     }
    //     Ok(())
    // }

    fn register_binder(binders: &mut Vec<String>, binder: String) -> Result<()> {
        if binders.contains(&binder) {
            // @Task more information
            Err(Error::AlreadyDefined)
        } else {
            binders.push(binder);
            Ok(())
        }
    }

    // @Question good naming scheme?
    // @Note evaluate needs to take a reference to self.binders @Task to allow for recursion
    // @Note currently `evaluate` means registering bindings in the context and also if
    // we encounter the "declaration" expr_stmt, we evaluate it (a sort-of main function)
    // @Note in future, the repl might evaluate expressions as `'let '_: 'hole 'infer = expr`
    // or we just expose a better evaluate function
    // @Note once we implement D/E-modes into repl, the signature will change to
    // fn(&self, context: Context, state: State<'_>) -> Result<()> or
    // if we switch to immutable contexts (which includes State):
    // fn(&self, context: ImmContext) -> Result<ImmContext>
    pub fn evaluate(&self, context: MutCtx, state: State<'_>) -> Result<Option<(Expr, Expr)>> {
        Ok(self
            .declarations
            .iter()
            .map(|declaration| declaration.evaluate(context.clone(), state))
            .last()
            .transpose()?
            .flatten())
    }
}

#[derive(Debug)]
pub enum Declaration {
    // @Question `expr` instead of `value`?
    Let {
        binder: Variable,
        type_: Expr,
        expr: Expr,
    },
    Data {
        binder: Variable,
        type_: Expr,
        constructors: Vec<Constructor>,
    },
    // @Temporary
    ExprStmt(Expr),
}

#[derive(Debug)]
pub struct Constructor {
    pub binder: Variable,
    pub type_: Expr,
}

impl Declaration {
    // @Temporary glue code @Question or not temporary?
    // @Note loss of information: implicits
    // @Beacon @Beacon @Beacon @Bug we mis-transpile from hir:
    // we don't get Eq correctly: Even if span is different, they could refer to
    // the same name, we need `source`!!!
    pub fn from_hir(declaration: hir::Declaration) -> Self {
        match declaration {
            hir::Declaration::Let {
                binder,
                type_annotation,
                expression,
            } => Self::Let {
                binder: Variable::Identifier(binder),
                type_: Expr::from_hir(type_annotation),
                expr: Expr::from_hir(expression),
            },
            hir::Declaration::Data {
                binder,
                type_annotation,
                constructors,
            } => unimplemented!(),
            _ => unimplemented!(),
        }
    }

    // used for recursion
    // @Note currently disallows declarations where the binding is recursive in its type signature
    // the only example that currently makes sense to me is `'let A: A = 'Type` but it's not useful
    // at all and only exist because of the special `the Type' Type'`.
    // Are there any expressions that a well-typed if they recurse in their type??
    // name: "recursion in type annotation"
    // @Bug unusable
    pub fn register(&self, context: MutCtx, state: State<'_>) -> Result<()> {
        unreachable!()
        // Ok(match self {
        //     Declaration::Let { binder, type_, .. } => {
        //         // @Task verify type_
        //         context.insert_unresolved_binding(binder, type_)?;
        //     }
        //     Declaration::Data { binder, type_, constructors } => {
        //         // @Task verify type_
        //         context.insert_unresolved_binding(binder, type_)?;
        //         // @Question should we register those????
        //         // I think, we don't want to allow "recursion in type annotation"
        //         //for constructor in constructors {
        //         //    context.insert_unresolved_binding(constructor.binder, constructor);
        //         //}
        //     }
        //     Declaration::ExprStmt(..) => unreachable!(),
        // })
    }

    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon 
    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon 
    // @Bug @Bug @Bug still has strange equality errors (e.g. `À` not found even though
    // it should be in scope) @Question broken Eq/Hash implementations?
    // @Task make pure returning new Context
    // @Task change name to register2 b.c. we are a lazy lang, we should only do type_checking here
    pub fn evaluate(&self, context: MutCtx, state: State<'_>) -> Result<Option<(Expr, Expr)>> {
        Ok(match self {
            Declaration::Let {
                binder,
                type_,
                expr,
            } => {
                // @Note because we cannot shadow in this context yet
                if context.clone().contains(&binder) {
                    return Err(Error::AlreadyDefined);
                }
                // @Task @Beacon @Bug check equality infered and given type
                let infered_type = expr.infer_type(context.clone(), state)?;
                let value = expr.normalize(context.clone(), state)?;
                context.insert_binding(binder.clone(), infered_type, value);
                None
            }
            Declaration::Data {
                binder: adt_binder,
                type_,
                constructors,
            } => {
                // @Temporary
                if context.clone().contains(&adt_binder) {
                    return Err(Error::AlreadyDefined);
                }
                type_.check_is_type(context.clone(), state)?;
                context
                    .clone()
                    .insert_adt(adt_binder.clone(), type_.clone());

                for Constructor { binder, type_ } in constructors {
                    type_.check_is_type(context.clone(), state)?;
                    // @Task instance check etc
                    // @Task check if constructor already exists
                    context
                        .clone()
                        .insert_constructor(binder.clone(), type_.clone(), adt_binder);
                }

                None
            }
            Declaration::ExprStmt(expr) => {
                let type_ = expr.infer_type(context.clone(), state)?;
                let value = expr.normalize(context, state)?;
                Some((value, type_))
            }
        })
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Var(Variable),
    Type,
    Pi(Abstraction),
    Lambda(Abstraction),
    App(Box<Expr>, Box<Expr>),
}

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

impl Expr {
    // @Temporary glue code @Note very expensive
    pub fn from_hir(expression: hir::Expression) -> Self {
        match expression {
            // @Note big information loss (explicitness)
            hir::Expression::PiLiteral {
                binder,
                parameter,
                expression,
                explicitness: _explicitness,
            } => Self::Pi(Abstraction {
                binder: binder.map_or(Variable::Dummy, Variable::Identifier),
                input: Box::new(Self::from_hir(*parameter)),
                output: Box::new(Self::from_hir(*expression)),
            }),
            // @Note big information loss (explicitness)
            hir::Expression::Application {
                expression,
                argument,
                explicitness: _explicitness,
            } => Self::App(
                Box::new(Self::from_hir(*expression)),
                Box::new(Self::from_hir(*argument)),
            ),
            hir::Expression::TypeLiteral => Self::Type,
            hir::Expression::Identifier(identifier) => Self::Var(Variable::Identifier(identifier)),
            hir::Expression::Hole(identifier) => unimplemented!(),
            // @Note big information loss (type_annotation, explicitness)
            hir::Expression::LambdaLiteral {
                binder,
                parameter,
                explicitness: _explicitness,
                type_annotation: _type_annotation,
                expression,
            } => {
                Self::Lambda(Abstraction {
                    binder: Variable::Identifier(binder),
                    // @Bug unannotated parameters not handled
                    input: Box::new(Self::from_hir(*parameter.unwrap())),
                    output: Box::new(Self::from_hir(*expression)),
                })
            }
            _ => unimplemented!(),
        }
    }

    fn subst(&self, env: Environment, state: State<'_>) -> Self {
        match self {
            Expr::Var(binding) => env.get(binding).cloned().unwrap_or_else(|| self.clone()),
            Expr::Type => Expr::Type,
            Expr::Pi(abstraction) => Expr::Pi(abstraction.subst(env, state)),
            Expr::Lambda(abstraction) => Expr::Lambda(abstraction.subst(env, state)),
            Expr::App(function, argument) => Expr::App(
                Box::new(function.subst(env.clone(), state)),
                Box::new(argument.subst(env, state)),
            ),
        }
    }

    pub fn infer_type(&self, ctx: MutCtx, state: State<'_>) -> Result<Self> {
        Ok(match self {
            Expr::Var(binding) => ctx
                .lookup_type(binding)
                .ok_or_else(|| Error::UndefinedBinding(binding.clone()))?,
            Expr::Type => Expr::Type,
            Expr::Pi(_) => Expr::Type,
            Expr::Lambda(Abstraction {
                binder,
                input,
                output,
            }) => {
                input.check_is_type(ctx.clone(), state)?;
                let output = output.infer_type(
                    ctx.extend_with_neutral_binding(binder.clone(), input.as_ref().clone()),
                    state,
                )?;
                Expr::Pi(Abstraction {
                    binder: binder.clone(),
                    input: input.clone(),
                    output: Box::new(output),
                })
            }
            Expr::App(function, argument) => {
                let abstraction = function.infer_pi(ctx.clone(), state)?;
                let argument_type = argument.infer_type(ctx.clone(), state)?;
                abstraction.input.check_equal(&argument_type, ctx, state)?;
                abstraction.output.subst(
                    {
                        let mut env = HashMap::new();
                        env.insert(abstraction.binder, argument.as_ref().clone());
                        Rc::new(env)
                    },
                    state,
                )
            }
        })
    }

    fn infer_pi(&self, ctx: MutCtx, state: State<'_>) -> Result<Abstraction> {
        let ty = self.infer_type(ctx.clone(), state)?;
        match ty.normalize(ctx, state)? {
            Expr::Pi(abstraction) => Ok(abstraction),
            expr => Err(Error::FunctionExpected(expr)),
        }
    }

    pub fn normalize(&self, ctx: MutCtx, state: State<'_>) -> Result<Expr> {
        Ok(match self {
            Expr::Var(binding) => ctx
                .clone()
                .lookup_value(binding)
                .ok_or_else(|| Error::UndefinedBinding(binding.clone()))?
                .map(|expr| expr.normalize(ctx, state))
                .unwrap_or_else(|| Ok(self.clone()))?,
            Expr::App(expr1, expr2) => {
                let expr2 = expr2.normalize(ctx.clone(), state)?;
                match expr1.normalize(ctx, state)? {
                    Expr::Lambda(abstraction) => abstraction.output.subst(
                        {
                            let mut env = HashMap::new();
                            env.insert(abstraction.binder, expr2);
                            Rc::new(env)
                        },
                        state,
                    ),
                    expr1 => Expr::App(Box::new(expr1), Box::new(expr2)),
                }
            }
            Expr::Type => Expr::Type,
            Expr::Pi(abstraction) => Expr::Pi(abstraction.normalize(ctx, state)?),
            Expr::Lambda(abstraction) => Expr::Lambda(abstraction.normalize(ctx, state)?),
        })
    }

    pub fn check_is_type(&self, ctx: MutCtx, state: State<'_>) -> Result<()> {
        let type_ = self.infer_type(ctx.clone(), state)?;
        type_.check_equal(&Expr::Type, ctx, state)?;
        Ok(())
    }

    fn check_equal(&self, other: &Self, ctx: MutCtx, state: State<'_>) -> Result<()> {
        if !self.equal(other, ctx, state)? {
            Err(Error::ExpressionsNotEqual(self.clone(), other.clone()))
        } else {
            Ok(())
        }
    }

    fn equal(&self, other: &Self, ctx: MutCtx, state: State<'_>) -> Result<bool> {
        Ok(equal(
            &self.normalize(ctx.clone(), state)?,
            &other.normalize(ctx.clone(), state)?,
            ctx,
            state,
        ))
    }

    // @Task impl; move it to future Value
    // @Note @Temporary panics if self not a type
    // @Note panics if a binder does not exist
    fn inhabited(&self, context: MutCtx) -> Result<bool> {
        Ok(match self {
            Expr::Type => true,
            Expr::Pi(Abstraction { input, output, .. }) => {
                // @Bug crashes on e.g. `(-> A # A)` because we need to extend
                // context
                output.inhabited(context.clone())? || !input.inhabited(context)?
            }
            // @Note because we treat Self=Expr as Self=Value, this is a neutral Var
            // so its value (repr) is None. It cannot be a parameter, so ist must be
            // an ADT i think
            Expr::Var(binding) => {
                if context.clone().adt(binding) {
                    let constructors = context.clone().constructors(binding);
                    // @Note sad: bc of error handling, we cannot use `all` but
                    // only `try_fold`
                    !constructors.is_empty()
                        && constructors
                            .into_iter()
                            .try_fold(true, |acc, constructor| {
                                Ok(acc
                                    && context
                                        .clone()
                                        .lookup_type(&constructor)
                                        .unwrap()
                                        .inhabited(context.clone())?)
                            })?
                } else {
                    // constructor @Bug not always a constructor but also neutral variable
                    // @Note not extensible, hacky
                    return Err(Error::ExpressionsNotEqual(
                        Expr::Type,
                        context.lookup_type(binding).unwrap(),
                    ));
                }
            }
            // @Bug hit: neutral application, e.g. `Maybe` (after lookup)
            x => {
                // @Temporary
                eprintln!("{:?}", x);
                unreachable!()
            }
        })
    }

    // @Task impl; move to Value
    // fn instance() {}
}

fn equal(expr1: &Expr, expr2: &Expr, ctx: MutCtx, state: State<'_>) -> bool {
    match (expr1, expr2) {
        (Expr::Var(binding1), Expr::Var(binding2)) => binding1 == binding2,
        (Expr::App(function1, argument1), Expr::App(function2, argument2)) => {
            equal(function1, function2, ctx.clone(), state)
                && equal(argument1, argument2, ctx, state)
        }
        (Expr::Type, Expr::Type) => true,
        (Expr::Pi(abstr1), Expr::Pi(abstr2)) => abstr1.equal(abstr2, ctx, state),
        (Expr::Lambda(abstr1), Expr::Lambda(abstr2)) => abstr1.equal(abstr2, ctx, state),
        // @Bug annotations not handled but see note above
        _ => false,
    }
}

// @Task
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(binding) => write!(f, "{}", binding),
            Expr::Type => f.write_str("#"),
            Expr::Pi(abstraction) => write!(f, "(-> {})", abstraction),
            Expr::Lambda(_) => f.write_str("\\"),
            // @Note should not be printable
            Expr::App(function, argument) => write!(f, "({} {})", function, argument),
        }
    }
}

#[derive(Clone, Debug)]
pub struct Abstraction {
    pub binder: Variable,
    pub input: Box<Expr>,
    pub output: Box<Expr>,
}

impl Abstraction {
    fn subst(&self, env: Environment, state: State<'_>) -> Self {
        let binder = self.binder.refresh(state);
        Self {
            binder: binder.clone(),
            input: Box::new(self.input.subst(env.clone(), state)),
            output: Box::new(self.output.subst(
                {
                    let mut env = env.as_ref().clone();
                    env.insert(self.binder.clone(), Expr::Var(binder));
                    Rc::new(env)
                },
                state,
            )),
        }
    }

    fn normalize(&self, ctx: MutCtx, state: State<'_>) -> Result<Self> {
        let input = self.input.normalize(ctx.clone(), state)?;
        Ok(Self {
            binder: self.binder.clone(),
            input: Box::new(input.clone()),
            output: Box::new(self.output.normalize(
                ctx.extend_with_neutral_binding(self.binder.clone(), input),
                state,
            )?),
        })
    }

    fn equal(&self, other: &Self, ctx: MutCtx, state: State<'_>) -> bool {
        equal(&self.input, &other.input, ctx.clone(), state)
            && equal(
                &self.output,
                &other.output.subst(
                    {
                        let mut env = HashMap::new();
                        env.insert(other.binder.clone(), Expr::Var(self.binder.clone()));
                        Rc::new(env)
                    },
                    state,
                ),
                ctx,
                state,
            )
    }
}

impl fmt::Display for Abstraction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} {}", self.binder, self.input, self.output)
    }
}

/// A variable.
///
/// Identifiers are equal if their atoms are equal no matter what their spans are.
#[derive(Eq, Clone, Debug)]
pub enum Variable {
    Dummy,
    Identifier(crate::parser::Identifier),
    GeneratedIdentifier(crate::parser::Identifier, u32),
}

impl PartialEq for Variable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // @Question should this be equal to itself or not?
            (Self::Dummy, Self::Dummy) => true,
            (Self::Identifier(left), Self::Identifier(right)) => left.atom == right.atom,
            (
                Self::GeneratedIdentifier(left_identifier, left_version),
                Self::GeneratedIdentifier(right_identifier, right_version),
            ) => left_identifier.atom == right_identifier.atom && left_version == right_version,
            _ => false,
        }
    }
}

use std::hash::{Hash, Hasher};

impl Hash for Variable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Dummy => 0u64.hash(state),
            Self::Identifier(identifier) => {
                1u64.hash(state);
                identifier.hash(state);
            }
            Self::GeneratedIdentifier(identifier, version) => {
                2u64.hash(state);
                identifier.hash(state);
                version.hash(state);
            }
        }
    }
}

impl Variable {
    fn refresh(&self, state: State<'_>) -> Self {
        // @Bug overflowable
        *state += 1;
        match self {
            // @Temporary use more abstractions
            Variable::Dummy => Variable::GeneratedIdentifier(
                crate::parser::Identifier {
                    atom: crate::lexer::Atom::from(""),
                    span: 0..=0,
                },
                *state,
            ),
            Variable::Identifier(identifier) | Variable::GeneratedIdentifier(identifier, _) => {
                return Variable::GeneratedIdentifier(identifier.clone(), *state);
            }
        }
    }

    // @Temporary
    // fn view_str(&self) -> Option<&str> {
    //     if let Variable::String(string) = self {
    //         Some(string)
    //     } else {
    //         None
    //     }
    // }
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::Dummy => f.write_str("$"),
            Variable::Identifier(identifier) => write!(f, "{}", identifier),
            Variable::GeneratedIdentifier(identifier, version) => {
                write!(f, "{}${}", identifier, version)
            }
        }
    }
}

pub use context::MutCtx;

mod context {
    use super::{Expr, Variable};
    use std::{cell::RefCell, collections::HashMap, rc::Rc};

    // @Note expressions aren't the only entities: modules are too, gonna be added later
    // @Note repr gonna change once we allow overloading (expression,module)
    #[derive(Clone, Debug)]
    enum Entity {
        // used for recursion
        UnresolvedExpression { type_: Expr },
        // @Note technically, an ADT is a neutral expression, too @Question better naming?
        // @Note formal parameters
        NeutralExpression { type_: Expr },
        ResolvedExpression { type_: Expr, expr: Expr },
        // @Question additional payload? constructors?
        AlgebraicDataType { type_: Expr },
    }

    impl Entity {
        // @Note returns Option because UnresolvedExpression does not have a known type_
        // in our current model
        fn retrieve_type(&self) -> &Expr {
            match self {
                Entity::UnresolvedExpression { type_ } => type_,
                Entity::NeutralExpression { type_ } => type_,
                Entity::ResolvedExpression { type_, .. } => type_,
                Entity::AlgebraicDataType { type_, .. } => type_,
            }
        }

        fn retrieve_value(&self) -> Option<&Expr> {
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
        bindings: Rc<RefCell<HashMap<Variable, Entity>>>,
        adts: Rc<RefCell<HashMap<Variable, Vec<Variable>>>>,
    }

    impl MutCtx {
        pub fn contains(self, binding: &Variable) -> bool {
            self.bindings.borrow().contains_key(binding)
        }

        pub fn adt(self, binding: &Variable) -> bool {
            self.adts.borrow().contains_key(binding)
        }

        // @Note assumes binding is indeed an ADT @Note panics
        // @Note ugly: clones ... I just want to introspect the value
        // ... CPS would work here
        pub fn constructors(self, binding: &Variable) -> Vec<Variable> {
            self.adts.borrow()[binding].clone()
        }

        pub fn lookup_type(self, binding: &Variable) -> Option<Expr> {
            self.bindings
                .borrow()
                .get(binding)
                .map(Entity::retrieve_type)
                .cloned()
        }

        pub fn lookup_value(self, binding: &Variable) -> Option<Option<Expr>> {
            self.bindings
                .borrow()
                .get(binding)
                .map(|entity| entity.retrieve_value().cloned())
        }

        pub fn extend_with_binding(self, binding: Variable, type_: Expr, value: Expr) -> Self {
            let mut map = self.bindings.as_ref().borrow().clone();
            map.insert(binding, Entity::ResolvedExpression { type_, expr: value });
            MutCtx {
                bindings: Rc::new(RefCell::new(map)),
                adts: self.adts,
            }
        }

        pub fn extend_with_neutral_binding(self, binding: Variable, type_: Expr) -> Self {
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
        pub fn insert_binding(self, binding: Variable, type_: Expr, value: Expr) {
            self.bindings
                .borrow_mut()
                .insert(binding, Entity::ResolvedExpression { type_, expr: value });
        }

        // @Task remove
        pub fn insert_neutral_binding(self, binding: Variable, type_: Expr) {
            self.bindings
                .borrow_mut()
                .insert(binding, Entity::NeutralExpression { type_ });
        }

        // @Task remove
        pub fn insert_adt(self, binding: Variable, type_: Expr) {
            self.clone().insert_neutral_binding(binding.clone(), type_);
            self.adts.borrow_mut().insert(binding, Vec::new());
        }

        // @Task remove
        // @Note panics if adt does not exist
        pub fn insert_constructor(self, binding: Variable, type_: Expr, adt: &Variable) {
            self.clone().insert_neutral_binding(binding.clone(), type_);
            self.adts.borrow_mut().get_mut(adt).unwrap().push(binding);
        }
    }
}

pub type State<'a> = &'a mut u32;

// @Note currently, the whole code base only constructs Environments with one entry
// Possibly, a later blog post expands on this, otherwise @Task replace with tuple
type Environment = Rc<HashMap<Variable, Expr>>;

pub fn initial() -> (MutCtx, u32) {
    (Default::default(), 0)
}

type Result<T> = std::result::Result<T, Error>;

pub enum Error {
    UndefinedBinding(Variable),
    FunctionExpected(Expr),
    ExpressionsNotEqual(Expr, Expr),
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
