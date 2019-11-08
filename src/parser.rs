mod error;

use crate::error::Span;
use crate::lexer::{self, Atom};
use std::convert::{TryFrom, TryInto};
use std::fmt;

pub use error::Error;
use error::{ErrorKind, Result};

// @Beacon @Temporary
const NOWHERE: Span = Span::new(0, 0);

// @Temporary pub
#[derive(Clone)]
pub struct Context<'i> {
    tokens: &'i [lexer::SourceToken],
    index: usize,
}

impl<'i> Context<'i> {
    pub fn new(tokens: &'i [lexer::SourceToken]) -> Self {
        Self { tokens, index: 0 }
    }

    fn reflect<T>(&mut self, parser: fn(&mut Context<'i>) -> Result<T>) -> Result<T> {
        let mut context = self.clone();
        parser(&mut context).map(|value| {
            *self = context;
            value
        })
    }

    // @Note unused exept in consume
    fn expect(&self, token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.token()?;
        if token.kind() == token_kind {
            Ok(token)
        } else {
            // @Task also add (list of) expected token(s)
            Err(Error {
                kind: ErrorKind::UnexpectedToken(token),
                span: NOWHERE,
            })
        }
    }

    // @Note ugly: special-casing??
    fn expect_end_of_input(&self) -> Result<()> {
        if self.index < self.tokens.len() {
            Err(Error {
                kind: ErrorKind::ExpectedEndOfInput,
                span: NOWHERE,
            })
        } else {
            Ok(())
        }
    }

    // @Question don't just accept tokens but anything of a certain trait which tokens implement,
    // optional tokens, either this or that token (with special token EOI), parser::Identifier,...
    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::SourceToken> {
        let token = self.expect(token_kind)?;
        self.accept();
        Ok(token)
    }

    // @Note ugly: special-casing identifiers... need to generalize? via trait?
    fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(lexer::TokenKind::Identifier)
            .map(|token| token.try_into().unwrap())
    }

    fn accept(&mut self) {
        self.index += 1;
    }

    fn token(&self) -> Result<lexer::SourceToken> {
        self.tokens.get(self.index).cloned().ok_or(Error {
            kind: ErrorKind::UnexpectedEndOfInput,
            span: NOWHERE,
        })
    }
}

#[derive(Debug)] // @Temporary
pub enum Declaration {
    Let {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        expression: Expression,
        span: Span,
    },
    Data {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        constructors: Vec<Constructor>,
        span: Span,
    },
    // @Temporary missing a lot of information
    Module {
        declarations: Vec<Declaration>,
        span: Span,
    },
    // @Task
    Use {
        span: Span,
    },
    // @Task
    Foreign {
        span: Span,
    },
}

impl Declaration {
    pub fn span(&self) -> Span {
        match self {
            Self::Let { span, .. } => *span,
            Self::Data { span, .. } => *span,
            Self::Module { span, .. } => *span,
            Self::Use { span, .. } => *span,
            Self::Foreign { span, .. } => *span,
        }
    }
}

// @Temporary
pub fn parse_file_module_no_header(context: &mut Context<'_>) -> Result<Declaration> {
    let mut declarations = Vec::new();

    while let Ok(declaration) = context
        .consume(lexer::TokenKind::LineBreak)
        .map(|_| None)
        .or_else(|_| context.reflect(parse_declaration).map(Some))
    {
        if let Some(declaration) = declaration {
            declarations.push(declaration);
        }
    }

    context.expect_end_of_input()?;

    Ok(Declaration::Module {
        declarations,
        span: NOWHERE,
    })
}

pub fn parse_declaration(context: &mut Context<'_>) -> Result<Declaration> {
    pub fn parse_data_declaration(context: &mut Context<'_>) -> Result<Declaration> {
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Data))?;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        let type_annotation = parse_type_annotation(context)?;
        context.consume(lexer::TokenKind::Equals)?;
        context.consume(lexer::TokenKind::LineBreak)?;

        let mut constructors = Vec::new();

        if context.consume(lexer::TokenKind::Indentation).is_ok() {
            while let Ok(constructor) = context.reflect(parse_constructor) {
                constructors.push(constructor);
            }
            // @Question or EOI?
            context.consume(lexer::TokenKind::Dedentation)?;
        }

        Ok(Declaration::Data {
            binder,
            parameters,
            type_annotation,
            constructors,
            span: NOWHERE,
        })
    }

    fn parse_let_declaration(context: &mut Context<'_>) -> Result<Declaration> {
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Let))?;
        let binder = context.consume_identifier()?;
        let parameters = context.reflect(parse_annotated_parameters)?;

        let type_annotation = context.reflect(parse_type_annotation)?;
        context.consume(lexer::TokenKind::Equals)?;
        let expression = context.reflect(parse_expression)?;
        expect_delimiter(context).map(|_| context.accept())?;

        Ok(Declaration::Let {
            binder,
            parameters,
            type_annotation,
            expression,
            span: NOWHERE,
        })
    }

    // @Task
    fn _parse_use_declaration() -> Result<Declaration> {
        unimplemented!()
    }

    // @Task
    fn _parse_foreign_declaration() -> Result<Declaration> {
        unimplemented!()
    }

    // @Task
    fn _parse_module_declaration() -> Result<Declaration> {
        unimplemented!()
    }

    // @Temporary missing use, module and foreign declarations
    context
        .reflect(parse_data_declaration)
        .or_else(|_| parse_let_declaration(context))
}

#[derive(Debug, Clone)]
pub enum Expression {
    // @Task rename to PiTypeLiteral { parameter, domain, codomain, explicitness }
    PiLiteral {
        binder: Option<Identifier>,
        parameter: Box<Expression>,
        expression: Box<Expression>,
        explicitness: Explicitness,
        span: Span,
    },
    Application {
        expression: Box<Expression>,
        argument: Box<Expression>,
        explicitness: Explicitness,
        span: Span,
    },
    TypeLiteral {
        span: Span,
    },
    NatTypeLiteral {
        span: Span,
    },
    NatLiteral {
        value: lexer::Nat,
        span: Span,
    },
    Identifier {
        inner: Identifier,
    },
    Hole {
        tag: Identifier,
        span: Span,
    },
    // @Task rename to LambdaLiteral { parameters, body_type_annotation, body }
    LambdaLiteral {
        parameters: Parameters,
        type_annotation: Option<Box<Expression>>,
        expression: Box<Expression>,
        span: Span,
    },
    // @Task rename the field expression to something more descriptive
    LetIn {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Box<Expression>>,
        // @Task improve upon naming
        expression: Box<Expression>,
        scope: Box<Expression>,
        span: Span,
    },
    // @Task
    UseIn {
        span: Span,
    },
    // @Task
    Case {
        span: Span,
    },
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Self::PiLiteral { span, .. } => *span,
            Self::Application { span, .. } => *span,
            Self::TypeLiteral { span } => *span,
            Self::NatTypeLiteral { span } => *span,
            Self::NatLiteral { span, .. } => *span,
            Self::Identifier { inner } => inner.span,
            Self::Hole { span, .. } => *span,
            Self::LambdaLiteral { span, .. } => *span,
            Self::LetIn { span, .. } => *span,
            Self::UseIn { span, .. } => *span,
            Self::Case { span, .. } => *span,
        }
    }
}

// @Note @Bug indentation logic not implemented (e.g. indent+let-in)
// Expression ::= Let_In | Lambda_Literal | Pi_Literal_Or_Lower
pub fn parse_expression(context: &mut Context<'_>) -> Result<Expression> {
    // Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    fn parse_let_in(context: &mut Context<'_>) -> Result<Expression> {
        let let_keyword = context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Let))?;
        let binder = context.consume_identifier()?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok().map(Box::new);
        context.consume(lexer::TokenKind::Equals)?;
        let expression = Box::new(context.reflect(parse_expression)?);
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::In))?;
        let scope = Box::new(context.reflect(parse_expression)?);
        Ok(Expression::LetIn {
            binder,
            parameters,
            type_annotation,
            expression,
            span: let_keyword.span.merge(scope.span()),
            scope,
        })
    }

    // Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    fn parse_lambda_literal(context: &mut Context<'_>) -> Result<Expression> {
        let backslash = context.consume(lexer::TokenKind::Backslash)?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok().map(Box::new);
        context.consume(lexer::TokenKind::WideArrow)?;
        let expression = Box::new(context.reflect(parse_expression)?);

        Ok(Expression::LambdaLiteral {
            parameters,
            type_annotation,
            span: backslash.span.merge(expression.span()),
            expression,
        })
    }

    // @Task update grammar
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_literal_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let (explicitness, binder, parameter) = context
            .reflect(|context| {
                context.consume(lexer::TokenKind::OpeningRoundBracket)?;

                let explicitness = consume_explicitness_comma(context);
                let binder = context.consume_identifier()?;
                let parameter = parse_type_annotation(context)?;

                context.consume(lexer::TokenKind::ClosingRoundBracket)?;

                Ok((explicitness, Some(binder), parameter))
            })
            .or_else(|_| {
                // @Question do we need reflect here?
                context
                    .reflect(parse_application_or_lower)
                    // .reflect(parse_semicolon_application_or_lower)
                    .map(|parameter| (Explicitness::Explicit, None, parameter))
            })?;

        Ok(match context.consume(lexer::TokenKind::ThinArrow) {
            Ok(_) => Expression::PiLiteral {
                expression: Box::new(context.reflect(parse_pi_literal_or_lower)?),
                binder,
                parameter: Box::new(parameter),
                explicitness,
                span: NOWHERE,
            },
            Err(_) if binder.is_none() => parameter,
            Err(error) => return Err(error),
        })
    }

    // @Beacon @Beacon @Beacon @Note semicolon application
    // Is right-associative and apparently (Haskell) has the same precedence
    // as `->` which is also right-assoc
    // Interaction between application, `;` and `->` looks like:
    // Alpha Beta -> Gamma Delta ;;; (Alpha Beta) -> (Gamma Delta)
    // Alpha; Beta -> Gamma; Delta ;;; (Alpha) (Beta -> ((Gamma) (Delta)))
    // @Task grammar
    // fn parse_semicolon_application_or_lower(context: &mut Context<'_>) -> Result<Expression> {
    //     let expression = parse_application_or_lower(context)?;

    //     Ok(if context.consume(lexer::TokenKind::Semicolon).is_ok() {
    //         Expression::Application {
    //             expression: Box::new(expression),
    //             argument: Box::new(context.reflect(parse_semicolon_application_or_lower)?),
    //             explicitness: Explicitness::Explicit,
    //         }
    //     } else {
    //         expression
    //     })
    // }

    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon
    // @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon
    // @Task implement semicolon application
    // @Note the logic should probably take place inside parse_application_or_lower
    // @Note: The precedence of semicolon application and pi literals IS THE SAME
    // we don't need the "or_lower"-logic here but simply an `or_else`
    // @Question where exactly to impl??

    // @Task application with semicolon
    // Application_Or_Lower %left% ::= Lower_Expression (Lower_Expression | "(" "," Expression ")")*
    fn parse_application_or_lower(context: &mut Context<'_>) -> Result<Expression> {
        let mut expression = context.reflect(parse_lower_expression)?;
        while let Ok((argument, explicitness)) = context
            .reflect(|context| Ok((parse_lower_expression(context)?, Explicitness::Explicit)))
            .or_else(|_| -> Result<_> {
                context.consume(lexer::TokenKind::OpeningRoundBracket)?;
                context.consume(lexer::TokenKind::Comma)?;
                let expression = parse_expression(context)?;
                context.consume(lexer::TokenKind::ClosingRoundBracket)?;
                Ok((expression, Explicitness::Implicit))
            })
        {
            // @Bug @Temporary span
            expression = Expression::Application {
                expression: Box::new(expression),
                argument: Box::new(argument),
                explicitness,
                span: NOWHERE,
            };
        }
        Ok(expression)
    }

    // Lower_Expression ::=
    //     Type_Literal | Nat_Literal | Identifier | Hole | Bracketed_Expression
    fn parse_lower_expression(context: &mut Context<'_>) -> Result<Expression> {
        parse_type_literal(context)
            .or_else(|_| parse_nat_type_literal(context))
            .or_else(|_| parse_nat_literal(context))
            .or_else(|_| parse_identifier(context))
            .or_else(|_| context.reflect(parse_hole))
            .or_else(|_| parse_bracketed_expression(context))
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(context: &mut Context<'_>) -> Result<Expression> {
        context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Type))
            .map(|token| Expression::TypeLiteral { span: token.span })
    }

    fn parse_nat_type_literal(context: &mut Context<'_>) -> Result<Expression> {
        context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Nat))
            .map(|token| Expression::NatTypeLiteral { span: token.span })
    }

    fn parse_nat_literal(context: &mut Context<'_>) -> Result<Expression> {
        // @Note ugly match+unreachable, directly relates to consume/consume_identifier-issue mentioned above
        context
            .consume(lexer::TokenKind::NatLiteral)
            .map(|token| Expression::NatLiteral {
                value: match token.inner {
                    lexer::Token::NatLiteral(value) => value,
                    _ => unreachable!(),
                },
                span: token.span,
            })
    }

    // Identifier ::= %identifier%
    fn parse_identifier(context: &mut Context<'_>) -> Result<Expression> {
        context
            .consume_identifier()
            .map(|inner| Expression::Identifier { inner })
    }

    // Hole ::= "'hole" %identifier%
    fn parse_hole(context: &mut Context<'_>) -> Result<Expression> {
        let keyword_hole = context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Hole))?;
        let tag = context.consume_identifier()?;
        Ok(Expression::Hole {
            span: keyword_hole.span.merge(tag.span),
            tag,
        })
    }

    // Bracketed_Expression ::= "(" Expression ")"
    fn parse_bracketed_expression(context: &mut Context<'_>) -> Result<Expression> {
        context.consume(lexer::TokenKind::OpeningRoundBracket)?;
        let expression = context.reflect(parse_expression)?;
        context.consume(lexer::TokenKind::ClosingRoundBracket)?;
        Ok(expression)
    }

    // @Task verify
    context
        .reflect(parse_let_in)
        .or_else(|_| context.reflect(parse_lambda_literal))
        .or_else(|_| parse_pi_literal_or_lower(context))
}

// @Question add span information??
#[derive(Debug)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: AnnotatedParameters,
    pub type_annotation: Expression,
}

// @Task very future: unnameable constructor `'_`
fn parse_constructor(context: &mut Context<'_>) -> Result<Constructor> {
    let binder = context.consume_identifier()?;
    let parameters = parse_annotated_parameters(context)?;
    let type_annotation = parse_type_annotation(context)?;
    // @Question what about EOI?
    context.consume(lexer::TokenKind::LineBreak)?;

    Ok(Constructor {
        binder,
        parameters,
        type_annotation,
    })
}

// @Task add span information @Question or shouldn't we?
// @Task inline pattern-match
#[derive(Debug)]
pub struct AnnotatedParameterGroup {
    pub parameters: Vec<Identifier>,
    pub type_annotation: Expression,
    pub explicitness: Explicitness,
}

// Annotated_Parameter_Group ::= "(" ","? Unfenced_Annotated_Parameter_Group ")"
// Unfenced_Annotated_Parameter_Group ::= Identifier+ Type_Annotation
fn parse_annotated_parameter_group(context: &mut Context<'_>) -> Result<AnnotatedParameterGroup> {
    context.consume(lexer::TokenKind::OpeningRoundBracket)?;
    let explicitness = consume_explicitness_comma(context);
    let mut parameters = Vec::new();

    parameters.push(context.consume_identifier()?);

    // @Note @Bug probably drops errors as well @Note shouldn't: consume
    // reflects by itself @Task verify
    while let Ok(parameter) = context.consume_identifier() {
        parameters.push(parameter);
    }

    let type_annotation = context.reflect(parse_type_annotation)?;
    context.consume(lexer::TokenKind::ClosingRoundBracket)?;

    Ok(AnnotatedParameterGroup {
        parameters,
        type_annotation,
        explicitness,
    })
}

pub type AnnotatedParameters = Vec<AnnotatedParameterGroup>;

// Annotated_Parameters ::= Annotated_Parameter_Group*
fn parse_annotated_parameters(context: &mut Context<'_>) -> Result<AnnotatedParameters> {
    let mut parameters = Vec::new();

    // @Bug drops errors @Note updated code, check for bug @Task
    while let Ok(parameter_group) = context.reflect(parse_annotated_parameter_group) {
        parameters.push(parameter_group)
    }

    Ok(parameters)
}

// @Question add span information?
// @Task inline pattern-match
#[derive(Debug, Clone)] // @Temporary clone
pub struct ParameterGroup {
    pub parameters: Vec<Identifier>,
    pub type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
}

// @Task grammar snippet above function decl
fn parse_parameter_group(context: &mut Context<'_>) -> Result<ParameterGroup> {
    fn parse_optionally_annotated_parameter_group(
        context: &mut Context<'_>,
    ) -> Result<ParameterGroup> {
        context.consume(lexer::TokenKind::OpeningRoundBracket)?;
        let explicitness = consume_explicitness_comma(context);
        let mut parameters = Vec::new();

        parameters.push(context.consume_identifier()?);

        while let Ok(parameter) = context.consume_identifier() {
            parameters.push(parameter);
        }

        let type_annotation = parse_type_annotation(context).ok();

        context.consume(lexer::TokenKind::ClosingRoundBracket)?;

        Ok(ParameterGroup {
            parameters,
            type_annotation,
            explicitness,
        })
    }

    context
        .consume_identifier()
        .map(|identifier| ParameterGroup {
            parameters: vec![identifier],
            type_annotation: None,
            explicitness: Explicitness::Explicit,
        })
        .or_else(|_| parse_optionally_annotated_parameter_group(context))
}

pub type Parameters = Vec<ParameterGroup>;

// Parameters ::= Parameter_Group*
fn parse_parameters(context: &mut Context<'_>) -> Result<Parameters> {
    let mut parameters = Vec::new();

    while let Ok(parameter_group) = context.reflect(parse_parameter_group) {
        parameters.push(parameter_group)
    }

    Ok(parameters)
}

// Type_Annotation ::= ":" Expression
fn parse_type_annotation(context: &mut Context<'_>) -> Result<Expression> {
    context.consume(lexer::TokenKind::Colon)?;
    context.reflect(parse_expression)
}

// fn parse_possibly_indented @Task

fn _parse_indented<T>(
    _context: &mut Context<'_>,
    _inner: fn(&mut Context<'_>) -> Result<T>,
) -> Result<T> {
    unimplemented!() // @Task
}

fn consume_explicitness_comma(context: &mut Context<'_>) -> Explicitness {
    match context.consume(lexer::TokenKind::Comma) {
        Ok(_) => Explicitness::Implicit,
        Err(_) => Explicitness::Explicit,
    }
}

/// expect either a line break or the end of input
fn expect_delimiter(context: &Context<'_>) -> Result<()> {
    if let Ok(token) = context.token() {
        if token.kind() != lexer::TokenKind::LineBreak {
            return Err(Error {
                kind: ErrorKind::UnexpectedToken(token),
                span: NOWHERE,
            });
        }
    }
    Ok(())
}

// @Note is going to become more complex in the future (or is going to be replaced)
// when we implement to dotted identifiers, blanks (`'_`) and symbols
// @Note and possibly also generated identifiers (see effluvium::Variable) except if
// we use a variation on debruijn-indeces
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub atom: Atom,
    pub span: Span,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.atom)
    }
}

// @Note only ever used in consume_identifier @Question over-engineered?
impl TryFrom<lexer::SourceToken> for Identifier {
    type Error = ();

    fn try_from(token: lexer::SourceToken) -> Result<Self, Self::Error> {
        match token.inner {
            lexer::Token::Identifier(atom) => Ok(Self {
                atom,
                span: token.span,
            }),
            _ => Err(()),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Implicit => f.write_str(","),
            Self::Explicit => f.write_str(""),
        }
    }
}
