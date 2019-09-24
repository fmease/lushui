use crate::error::{DisplayWithSource, Span};
use crate::lexer;
use std::convert::{TryFrom, TryInto};
use std::fmt;

// @Task remove debug `pub` from some parsing functions (maybe)
// @Task figure out which `Context::reflect`s are necessary and which
// can be replaced by normal function calls

type Result<T, E = Error> = std::result::Result<T, E>;

// @Task pub parse_file_module

// @Temp pub
#[derive(Clone)]
pub struct Context<'i> {
    tokens: &'i [lexer::Token],
    index: usize,
}

impl<'i> Context<'i> {
    pub fn new(tokens: &'i [lexer::Token]) -> Self {
        Self { tokens, index: 0 }
    }

    fn reflect<T>(&mut self, parser: fn(&mut Context) -> Result<T>) -> Result<T> {
        let mut context = self.clone();
        parser(&mut context).map(|value| {
            *self = context;
            value
        })
    }

    // @Note unused exept in consume
    fn expect(&self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
        let token = self.token()?;
        if token.kind == token_kind {
            Ok(token)
        } else {
            // @Task also add (list of) expected token(s)
            // @Bug @Temp dummy span
            Err(Error {
                kind: ErrorKind::UnexpectedToken(token),
                span: 0..=0,
            })
        }
    }

    fn consume(&mut self, token_kind: lexer::TokenKind) -> Result<lexer::Token> {
        let token = self.expect(token_kind)?;
        self.accept();
        Ok(token)
    }

    fn consume_identifier(&mut self) -> Result<Identifier> {
        self.consume(lexer::TokenKind::Identifier)
            .map(|token| token.try_into().unwrap())
    }

    fn accept(&mut self) {
        self.index += 1;
    }

    fn token(&self) -> Result<lexer::Token> {
        // @Bug @Temp dummy span
        self.tokens.get(self.index).cloned().ok_or(Error {
            kind: ErrorKind::UnexpectedEndOfInput,
            span: 0..=0,
        })
    }
}

#[derive(Debug)] // @Temp
pub enum Declaration {
    Let {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        expression: Expression,
    },
    Data {
        binder: Identifier,
        parameters: AnnotatedParameters,
        type_annotation: Expression,
        constructors: Vec<Constructor>,
    },
    Module,  // @Task @Question file vs inline module
    Use,     // @Task
    Foreign, // @Task
}

pub fn parse_declaration(_context: &mut Context) -> Result<Declaration> {
    fn parse_data_declaration(context: &mut Context) -> Result<Declaration> {
        eprintln!("parsing data declaration..");
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Data))?;
        let binder = context.consume_identifier()?;
        let parameters = parse_annotated_parameters(context)?;
        dbg!(&parameters);
        let type_annotation = parse_type_annotation(context)?;
        dbg!(&type_annotation);
        context.consume(lexer::TokenKind::LineBreak)?;
        let mut constructors = Vec::new();

        // @Bug @Task use reflect
        if context.consume(lexer::TokenKind::Indentation).is_ok() {
            while let Ok(constructor) = parse_constructor(context) {
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
        })
    }

    fn parse_let_declaration(context: &mut Context) -> Result<Declaration> {
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
        })
    }

    // @Task
    fn parse_use_declaration() -> Result<Declaration> {
        unimplemented!()
    }

    // @Task
    fn parse_foreign_declaration() -> Result<Declaration> {
        unimplemented!()
    }

    unimplemented!() // @Task
}

#[derive(Debug, Clone)]
pub enum Expression {
    PiLiteral {
        binder: Option<Identifier>,
        parameter: Box<Expression>,
        expression: Box<Expression>,
        explicitness: Explicitness,
    },
    Application {
        expression: Box<Expression>,
        argument: Box<Expression>,
        explicitness: Explicitness,
    },
    TypeLiteral,
    Identifier(Identifier),
    Hole(Identifier),
    LambdaLiteral {
        parameters: Parameters,
        type_annotation: Option<Box<Expression>>,
        expression: Box<Expression>,
    },
    LetIn {
        binder: Identifier,
        parameters: Parameters,
        type_annotation: Option<Box<Expression>>,
        // @Task improve upon naming
        expression: Box<Expression>,
        scope: Box<Expression>,
    },
    UseIn, // @Task
    Case,  // @Task
}

// @Note @Bug indentation logic not implemented (e.g. indent+let-in)
// Expression ::= Let_In | Lambda_Literal | Pi_Literal_Or_Lower
pub fn parse_expression(context: &mut Context) -> Result<Expression> {
    // Let_In ::= "'let" Identifier Parameters Type_Annotation? "=" Expression "'in" Expression
    fn parse_let_in(context: &mut Context) -> Result<Expression> {
        dbg!();
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Let))?;
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
            scope,
        })
    }

    // Lambda_Literal ::= "\" Parameters Type_Annotation? "=>" Expression
    fn parse_lambda_literal(context: &mut Context) -> Result<Expression> {
        context.consume(lexer::TokenKind::Backslash)?;
        let parameters = context.reflect(parse_parameters)?;
        let type_annotation = context.reflect(parse_type_annotation).ok().map(Box::new);
        context.consume(lexer::TokenKind::WideArrow)?;

        Ok(Expression::LambdaLiteral {
            parameters,
            type_annotation,
            expression: Box::new(context.reflect(parse_expression)?),
        })
    }

    // @Task integrate with expression logic
    // pub fn parse_pi_literal(context: &mut Context) -> Result<Expression> {
    //     eprintln!("@@@@@ pi type literal");
    //     let (binder, parameter, implicit) = if let Ok(open_brace) = parse_opening_bracket(context) {
    //         // @Note @Task does not need to be that strict: `(Int) -> Int` should be valid as well
    //         // (rule: parenthesized expression)
    //         (
    //             Some(context.consume_identifier()?),
    //             parse_type_annotation(context)?,
    //             context
    //                 .consume(open_brace.kind.invert_bracket())?
    //                 .kind
    //                 .is_curly_bracket(),
    //         )
    //     } else {
    //         // @Bug @Temp replace with parse_expression1???
    //         (None, parse_expression(context)?, false)
    //     };
    //     dbg!(&binder, &parameter, &implicit);

    //     context.consume(lexer::TokenKind::ThinArrow)?;

    //     // @Question parse_expression or parse_expression0?
    //     let result = parse_expression(context)?;

    //     Ok(Expression::PiLiteral {
    //         binder,
    //         parameter: Box::new(parameter),
    //         implicit,
    //         result: Box::new(result),
    //     })
    // }

    // @Task with binder (implicit and explicit parameter)
    // Pi_Literal_Or_Lower %right% ::= Application_Or_Lower (-> Pi_Literal_Or_Lower)*
    fn parse_pi_literal_or_lower(context: &mut Context) -> Result<Expression> {
        let parameter = context.reflect(parse_application_or_lower)?;
        Ok(if context.expect(lexer::TokenKind::ThinArrow).is_ok() {
            context.accept();
            let expression = context.reflect(parse_pi_literal_or_lower)?;
            Expression::PiLiteral {
                binder: None,
                parameter: Box::new(parameter),
                expression: Box::new(expression),
                explicitness: Explicitness::Explicit,
            }
        } else {
            parameter
        })
    }

    // @Task explicit implicit arguments
    // @Task application with semicolon
    // Application_Or_Lower %left% ::= Lower_Expression*
    fn parse_application_or_lower(context: &mut Context) -> Result<Expression> {
        let mut expression = context.reflect(parse_lower_expression)?;
        while let Ok(argument) = context.reflect(parse_lower_expression) {
            expression = Expression::Application {
                expression: Box::new(expression),
                argument: Box::new(argument),
                explicitness: Explicitness::Explicit,
            };
        }
        Ok(expression)
    }

    // Lower_Expression ::=
    //     Type_Literal | Identifier | Hole | Bracketed_Expression
    fn parse_lower_expression(context: &mut Context) -> Result<Expression> {
        parse_type_literal(context)
            .or_else(|_| parse_identifier(context))
            .or_else(|_| context.reflect(parse_hole))
            .or_else(|_| parse_bracketed_expression(context))
    }

    // Type_Literal ::= %type literal%
    fn parse_type_literal(context: &mut Context) -> Result<Expression> {
        context
            .consume(lexer::TokenKind::Keyword(lexer::Keyword::Type))
            .map(|_| Expression::TypeLiteral)
    }

    // Identifier ::= %identifier%
    fn parse_identifier(context: &mut Context) -> Result<Expression> {
        context.consume_identifier().map(Expression::Identifier)
    }

    // Hole ::= %hole%
    fn parse_hole(context: &mut Context) -> Result<Expression> {
        context.consume(lexer::TokenKind::Keyword(lexer::Keyword::Hole))?;
        Ok(Expression::Hole(context.consume_identifier()?))
    }

    // Bracketed_Expression ::= "(" Expression ")"
    fn parse_bracketed_expression(context: &mut Context) -> Result<Expression> {
        context.consume(lexer::TokenKind::Bracket(lexer::Bracket::OpeningRound))?;
        let expression = context.reflect(parse_expression)?;
        context.consume(lexer::TokenKind::Bracket(lexer::Bracket::ClosingRound))?;
        Ok(expression)
    }

    // @Task verify
    context
        .reflect(parse_let_in)
        .or_else(|_| context.reflect(parse_lambda_literal))
        .or_else(|_| parse_pi_literal_or_lower(context))
}
#[derive(Debug)]
pub struct Constructor {
    pub binder: Identifier,
    pub parameters: AnnotatedParameters,
    pub type_annotation: Expression,
}

// @Task very future: unnameable constructor `'_`
fn parse_constructor(context: &mut Context) -> Result<Constructor> {
    eprintln!(">>>> ctor");
    let binder = context.consume_identifier()?;
    let parameters = parse_annotated_parameters(context)?;
    dbg!(&binder, &parameters);
    let type_annotation = parse_type_annotation(context)?;
    eprintln!("###");
    dbg!(&type_annotation);
    context.consume(lexer::TokenKind::LineBreak)?;

    Ok(Constructor {
        binder,
        parameters,
        type_annotation,
    })
}

// @Task inline pattern-match
#[derive(Debug)]
pub struct AnnotatedParameterGroup {
    pub parameters: Vec<Identifier>,
    pub type_annotation: Expression,
    pub explicitness: Explicitness,
}

// Annotated_Parameter_Group ::=
//     "(" Unfenced_Annotated_Parameter_Group ")" |
//     "{" Unfenced_Annotated_Parameter_Group "}"
// Unfenced_Annotated_Parameter_Group ::= Identifier+ Type_Annotation
fn parse_annotated_parameter_group(context: &mut Context) -> Result<AnnotatedParameterGroup> {
    let openening_bracket = parse_opening_bracket(context)?;
    // @Note boilerplate
    let openening_bracket = match openening_bracket.kind {
        lexer::TokenKind::Bracket(bracket) => bracket,
        _ => unreachable!(),
    };
    let mut parameters = Vec::new();

    parameters.push(context.consume_identifier()?);

    // @Note @Bug probably drops errors as well @Note shouldn't: consume
    // reflects by itself @Task verify
    while let Ok(parameter) = context.consume_identifier() {
        parameters.push(parameter);
    }

    let type_annotation = context.reflect(parse_type_annotation)?;
    context.consume(lexer::TokenKind::Bracket(openening_bracket.invert()))?;

    Ok(AnnotatedParameterGroup {
        parameters,
        type_annotation,
        explicitness: openening_bracket.into(),
    })
}

pub type AnnotatedParameters = Vec<AnnotatedParameterGroup>;

// Annotated_Parameters ::= Annotated_Parameter_Group*
fn parse_annotated_parameters(context: &mut Context) -> Result<AnnotatedParameters> {
    let mut parameters = Vec::new();

    // @Bug drops errors @Note updated code, check for bug @Task
    while let Ok(parameter_group) = context.reflect(parse_annotated_parameter_group) {
        parameters.push(parameter_group)
    }

    Ok(parameters)
}

// @Task inline pattern-match
#[derive(Debug, Clone)] // @Temp clone
pub struct ParameterGroup {
    pub parameters: Vec<Identifier>,
    pub type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
}

fn parse_parameter_group(context: &mut Context) -> Result<ParameterGroup> {
    fn parse_optionally_annotated_parameter_group(context: &mut Context) -> Result<ParameterGroup> {
        let opening_bracket = parse_opening_bracket(context)?;
        // @Note boilerplate
        let opening_bracket = match opening_bracket.kind {
            lexer::TokenKind::Bracket(bracket) => bracket,
            _ => unreachable!(),
        };
        let mut parameters = Vec::new();

        parameters.push(context.consume_identifier()?);

        while let Ok(parameter) = context.consume_identifier() {
            parameters.push(parameter);
        }

        let type_annotation = parse_type_annotation(context).ok();

        context.consume(lexer::TokenKind::Bracket(opening_bracket.invert()))?;

        Ok(ParameterGroup {
            parameters,
            type_annotation,
            explicitness: opening_bracket.into(),
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
fn parse_parameters(context: &mut Context) -> Result<Parameters> {
    let mut parameters = Vec::new();

    while let Ok(parameter_group) = context.reflect(parse_parameter_group) {
        parameters.push(parameter_group)
    }

    Ok(parameters)
}

// Type_Annotation ::= ":" Expression
fn parse_type_annotation(context: &mut Context) -> Result<Expression> {
    context.consume(lexer::TokenKind::Colon)?;
    context.reflect(parse_expression)
}

// fn parse_possibly_indented @Task

fn _parse_indented<T>(_context: &mut Context, _inner: fn(&mut Context) -> Result<T>) -> Result<T> {
    unimplemented!() // @Task
}

fn parse_opening_bracket(context: &mut Context) -> Result<lexer::Token> {
    context
        .consume(lexer::TokenKind::Bracket(lexer::Bracket::OpeningRound))
        .or_else(|_| context.consume(lexer::TokenKind::Bracket(lexer::Bracket::OpeningCurly)))
}

/// expect either a line break or the end of input
fn expect_delimiter(context: &Context) -> Result<()> {
    if let Ok(token) = context.token() {
        if token.kind != lexer::TokenKind::LineBreak {
            return Err(Error {
                kind: ErrorKind::UnexpectedToken(token),
                span: 0..=0,
            });
        }
    }
    Ok(())
}

// @Note is going to become more complex in the future (or is going to be replaced)
// when we implement to dotted identifiers, blanks (`'_`) and symbols
#[derive(Debug, Clone)]
pub struct Identifier {
    span: Span,
}

impl DisplayWithSource for Identifier {
    fn display_with_source(&self, source: &str) -> String {
        self.span.display_with_source(source)
    }
}

impl TryFrom<lexer::Token> for Identifier {
    type Error = ();

    fn try_from(token: lexer::Token) -> Result<Self, Self::Error> {
        if let lexer::TokenKind::Identifier = token.kind {
            Ok(Self { span: token.span })
        } else {
            Err(())
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Explicitness {
    Implicit,
    Explicit,
}

impl Explicitness {
    pub fn brackets(self) -> (char, char) {
        match self {
            Self::Implicit => ('{', '}'),
            Self::Explicit => ('(', ')'),
        }
    }
}

impl From<lexer::Bracket> for Explicitness {
    fn from(bracket: lexer::Bracket) -> Self {
        use lexer::Bracket;
        match bracket {
            Bracket::OpeningRound | Bracket::ClosingRound => Self::Explicit,
            Bracket::OpeningCurly | Bracket::ClosingCurly => Self::Implicit,
        }
    }
}

// @Task store span information
#[derive(Debug)] // @Temp
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)] // @Temp
pub enum ErrorKind {
    UnexpectedEndOfInput,
    UnexpectedToken(lexer::Token),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            // @Temp
            Self::UnexpectedToken(token) => write!(f, "unexpected token {:?}", token.kind),
        }
    }
}
