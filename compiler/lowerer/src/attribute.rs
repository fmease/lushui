use crate::{Lowerer, Options};
use ast::Path;
use diagnostics::{
    error::{Health, Outcome, Result},
    reporter::ErasedReportedError,
    Diagnostic, ErrorCode, Reporter,
};
use lo_ast::{
    attribute::{Predicate, Query, Special, Target},
    AttributeName, Attributes, BareAttribute,
};
use session::Session;
use span::{Span, Spanned, Spanning};
use utility::default;
use utility::{Atom, Conjunction, ListingExt, QuoteExt};

impl Lowerer<'_> {
    /// Lower attributes.
    // @Task filter out documentation attributes if !options.keep_documentation_comments
    pub(crate) fn lower_attributes<T: Target>(
        &mut self,
        unchecked_attributes: &[ast::Attribute],
        target: &T,
        context: T::Context,
    ) -> Attributes {
        use lo_ast::Attribute;

        let actual_targets = target.targets(context);
        let mut conforming_attributes = Attributes::default();

        for attribute in unchecked_attributes {
            let attribute = match Attribute::parse(attribute, &self.options, self.session) {
                Ok(attribute) => attribute,
                Err(error) => {
                    self.health.taint(error);
                    continue;
                }
            };

            // search for non-conforming attributes
            {
                let expected_targets = attribute.bare.targets();
                if !expected_targets.contains(actual_targets) {
                    // @Question wording: "cannot be ascribed to"?
                    Diagnostic::error()
                        .code(ErrorCode::E013)
                        .message(format!(
                            "attribute ‘{}’ is ascribed to {}",
                            attribute.bare.name(),
                            target.name(context)
                        ))
                        .span(&attribute, "misplaced attribute")
                        .label(target, "incompatible item")
                        .note(format!(
                            "attribute ‘{}’ can only be ascribed to {}",
                            attribute.bare.name(),
                            expected_targets.description(),
                        ))
                        .handle(&mut *self);
                    continue;
                }
            }

            conforming_attributes.0.push(attribute);
        }

        self.check_attribute_synergy(&conforming_attributes)
    }

    pub(crate) fn check_attribute_synergy(
        &mut self,
        conforming_attributes: &Attributes,
    ) -> Attributes {
        let mut attributes = Attributes::default();

        // search for conflicting or duplicate attributes
        for attribute in &conforming_attributes.0 {
            if attribute.bare.can_be_applied_multiple_times() {
                attributes.0.push(attribute.clone());
                continue;
            }

            let is_homonymous =
                Predicate(|some_attribute| some_attribute.name() == attribute.bare.name());

            let homonymous_attributes: Vec<_> =
                conforming_attributes.filter(is_homonymous).collect();

            if !attributes.has(is_homonymous) {
                attributes.0.push(attribute.clone());
            }

            if let [first, _second, ..] = &*homonymous_attributes {
                Diagnostic::error()
                    .code(ErrorCode::E006)
                    .message(format!("multiple ‘{}’ attributes", first.bare.name()))
                    .spans(homonymous_attributes, "duplicate or conflicting attribute")
                    .handle(&mut *self);
            }
        }

        // no further checks necessary if empty
        if attributes.0.is_empty() {
            return attributes;
        }

        {
            use AttributeName::*;
            self.check_mutual_exclusivity(Intrinsic.or(Known), &attributes);
            self.check_mutual_exclusivity(Moving.or(Abstract), &attributes);
        }

        for attribute in attributes.filter(Predicate(|attribute| !attribute.is_implemented())) {
            Diagnostic::error()
                .message(format!(
                    "the attribute ‘{}’ is not supported yet",
                    attribute.bare.name()
                ))
                .unlabeled_span(attribute)
                .handle(&mut *self);
        }

        // @Task replace this concept with a feature system
        if !self.options.internal_features_enabled {
            for attribute in attributes.filter(Predicate(BareAttribute::is_internal)) {
                Diagnostic::error()
                    .code(ErrorCode::E038)
                    .message(format!(
                        "the attribute ‘{}’ is an internal feature",
                        attribute.bare.name()
                    ))
                    .unlabeled_span(attribute)
                    .handle(&mut *self);
            }
        }

        attributes
    }

    fn check_mutual_exclusivity<Q: Query>(&mut self, query: Q, attributes: &Attributes) {
        let attributes = attributes.filter(query).collect::<Vec<_>>();

        if attributes.len() > 1 {
            let listing = attributes
                .iter()
                .map(|attribute| attribute.bare.name().to_str().quote())
                .list(Conjunction::And);

            Diagnostic::error()
                .code(ErrorCode::E014)
                .message(format!("attributes {listing} are mutually exclusive"))
                .spans(attributes, "conflicting attribute")
                .handle(self);
        }
    }
}

// @Task dedup! (also found in the resolver/literal)
const NAT32_INTERVAL_REPRESENTATION: &str = "[0, 2^32-1]";

trait AttributeExt: Sized {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self>;
}

impl AttributeExt for lo_ast::Attribute {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self> {
        Ok(Self::new(
            attribute.span,
            BareAttribute::parse(attribute, options, session)?,
        ))
    }
}

trait BareAttributeExt: Sized {
    fn parse(attribute: &ast::Attribute, options: &Options, session: &Session<'_>) -> Result<Self>;
}

impl BareAttributeExt for lo_ast::BareAttribute {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    fn parse(
        // @Task take by value and create parsing helpers on ast::Attribute and ast::Attributes
        attribute: &ast::Attribute,
        options: &Options,
        session: &Session<'_>,
    ) -> Result<Self> {
        let ast::BareAttribute::Regular { binder, arguments } = &attribute.bare else {
            return Ok(Self::Doc {
                content: if options.keep_documentation_comments {
                    session
                        .shared_map()
                        .snippet(attribute.span)
                        .trim_start_matches(";;")
                        .to_owned()
                        .into()
                } else {
                    default()
                },
            });
        };

        let arguments: &mut &[_] = &mut &**arguments;

        fn optional_argument<'a>(
            arguments: &mut &'a [ast::AttributeArgument],
        ) -> Option<&'a ast::AttributeArgument> {
            arguments.first().map(|argument| {
                *arguments = &arguments[1..];
                argument
            })
        }

        // @Task improve API
        fn argument<'a>(
            arguments: &mut &'a [ast::AttributeArgument],
            span: Span,
            reporter: &Reporter,
        ) -> Result<&'a ast::AttributeArgument, AttributeParsingError> {
            let argument = arguments.first().ok_or_else(|| {
                // @Task add more information about the arity and the argument types
                AttributeParsingError::Erased(
                    Diagnostic::error()
                        .code(ErrorCode::E019)
                        .message("too few attribute arguments provided")
                        .unlabeled_span(span)
                        .report(reporter),
                )
            })?;
            *arguments = &arguments[1..];
            Ok(argument)
        }

        let result = (|| {
            use AttributeName::*;

            let name = AttributeName::parse(binder.bare())
                .ok_or(AttributeParsingError::UndefinedAttribute(*binder))?;

            Ok(match name {
                Abstract => Self::Abstract,
                Allow | Deny | Forbid | Warn => {
                    let lint = lo_ast::attribute::Lint::parse(
                        argument(arguments, attribute.span, session.reporter())?
                            .path(Some(Atom::LINT), session.reporter())?
                            .clone(),
                        session.reporter(),
                    )?;

                    match name {
                        Allow => Self::Allow { lint },
                        Deny => Self::Deny { lint },
                        Forbid => Self::Forbid { lint },
                        Warn => Self::Warn { lint },
                        _ => unreachable!(),
                    }
                }
                Context => Self::Context,
                Deprecated => Self::Deprecated(lo_ast::attribute::Deprecated {
                    reason: optional_argument(arguments)
                        .map(|argument| {
                            argument.text_literal(Some(Atom::REASON), session.reporter())
                        })
                        .transpose()?,
                    // @Task parse version
                    since: None,
                    // @Task parse version
                    removal: None,
                    replacement: optional_argument(arguments)
                        .map(|argument| {
                            argument.text_literal(Some(Atom::REPLACEMENT), session.reporter())
                        })
                        .transpose()?,
                }),
                Doc => Self::Doc {
                    content: if options.keep_documentation_comments {
                        argument(arguments, attribute.span, session.reporter())?
                            .text_literal(None, session.reporter())?
                            .to_str()
                            .into()
                    } else {
                        *arguments = &arguments[1..];
                        default()
                    },
                },
                Intrinsic => {
                    let name = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::NAME), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Intrinsic(Special { name })
                }
                If => {
                    return Err(AttributeParsingError::Erased(
                        Diagnostic::error()
                            .message("the attribute ‘if’ is not supported yet")
                            .unlabeled_span(attribute)
                            .report(session.reporter()),
                    ));
                }
                Ignore => Self::Ignore,
                Include => Self::Include,
                Known => {
                    let name = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::NAME), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Known(Special { name })
                }
                Location => {
                    let path = argument(arguments, attribute.span, session.reporter())?
                        .text_literal(Some(Atom::PATH), session.reporter())?;

                    Self::Location { path }
                }
                Moving => Self::Moving,
                Public => {
                    let reach = optional_argument(arguments)
                        .map(|argument| argument.path(Some(Atom::REACH), session.reporter()))
                        .transpose()?
                        .cloned();

                    Self::Public(lo_ast::attribute::Public { reach })
                }
                Record => Self::Record,
                RecursionLimit => {
                    let depth = argument(arguments, attribute.span, session.reporter())?;
                    let depth = depth
                        .number_literal(Some(Atom::DEPTH), session.reporter())?
                        .to_str()
                        .parse::<u32>()
                        .map_err(|_| {
                            AttributeParsingError::Erased(
                                Diagnostic::error()
                                    .code(ErrorCode::E008)
                                    .message(format!(
                                        "attribute argument does not fit integer interval \
                                        {NAT32_INTERVAL_REPRESENTATION}",
                                    ))
                                    .unlabeled_span(depth)
                                    .report(session.reporter()),
                            )
                        })?;

                    Self::RecursionLimit { depth }
                }
                Static => Self::Static,
                Statistics => Self::Statistics,
                Test => Self::Test,
                Trait => Self::Trait,
                Unsafe => Self::Unsafe,
                Unstable => {
                    return Err(AttributeParsingError::Erased(
                        Diagnostic::error()
                            .message("the attribute ‘unstable’ is not supported yet")
                            .unlabeled_span(attribute)
                            .report(session.reporter()),
                    ));
                }
            })
        })();

        let mut health = Health::Untainted;

        if !matches!(result, Err(AttributeParsingError::UndefinedAttribute(_))) {
            // if there are still unparsed arguments it means too many arguments were provided
            // unless the attribute does not exist in the first place since in such case,
            // no argument was parsed either
            if let Some(argument) = arguments.first() {
                let error = Diagnostic::error()
                    .code(ErrorCode::E019)
                    .message("too many attribute arguments provided")
                    .unlabeled_span(argument.span.merge(&arguments.last()))
                    .report(session.reporter());
                health.taint(error);
            }
        }

        result
            .map_err(|error| match error {
                AttributeParsingError::UndefinedAttribute(binder) => Diagnostic::error()
                    .code(ErrorCode::E011)
                    .message(format!("the attribute ‘{binder}’ is not defined"))
                    .unlabeled_span(binder)
                    .report(session.reporter()),
                AttributeParsingError::Erased(error) => error,
            })
            .and_then(|attributes| Outcome::new(attributes, health).into())
    }
}

enum AttributeParsingError {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    UndefinedAttribute(ast::Identifier),
}

// @Beacon hideous accessors! @Task deduplicate!
// @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

trait AttributeArgumentExt {
    fn number_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError>;

    fn text_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError>;

    fn path(&self, name: Option<Atom>, reporter: &Reporter)
        -> Result<&Path, AttributeParsingError>;
}

impl AttributeArgumentExt for ast::AttributeArgument {
    fn number_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            &NumberLiteral(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        NumberLiteral(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            error::invalid_attribute_argument_type(
                                Spanned::new(argument.span, bare.name()),
                                "number literal",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .copied(),
            bare => Err(AttributeParsingError::Erased(
                error::invalid_attribute_argument_type(
                    Spanned::new(self.span, bare.name()),
                    "positional or named number literal",
                )
                .report(reporter),
            )),
        }
    }

    fn text_literal(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<Atom, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            &TextLiteral(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        TextLiteral(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            error::invalid_attribute_argument_type(
                                Spanned::new(argument.span, bare.name()),
                                "text literal",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .copied(),
            bare => Err(AttributeParsingError::Erased(
                error::invalid_attribute_argument_type(
                    Spanned::new(self.span, bare.name()),
                    "positional or named text literal",
                )
                .report(reporter),
            )),
        }
    }

    fn path(
        &self,
        name: Option<Atom>,
        reporter: &Reporter,
    ) -> Result<&Path, AttributeParsingError> {
        use ast::BareAttributeArgument::*;

        match &self.bare {
            Path(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        Path(literal) => Ok(literal),
                        bare => Err(AttributeParsingError::Erased(
                            error::invalid_attribute_argument_type(
                                Spanned::new(argument.span, bare.name()),
                                "path",
                            )
                            .report(reporter),
                        )),
                    },
                    reporter,
                )
                .map(|path| &**path),
            bare => Err(AttributeParsingError::Erased(
                error::invalid_attribute_argument_type(
                    Spanned::new(self.span, bare.name()),
                    "positional or named path",
                )
                .report(reporter),
            )),
        }
    }
}

trait NamedAttributeArgumentExt {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<&T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<&T, AttributeParsingError>;
}

impl NamedAttributeArgumentExt for ast::NamedAttributeArgument {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttributeArgument) -> Result<&T, AttributeParsingError>,
        reporter: &Reporter,
    ) -> Result<&T, AttributeParsingError> {
        match name {
            Some(name) => {
                if self.binder.bare() == name {
                    handle(&self.value)
                } else {
                    Err(AttributeParsingError::Erased(
                        error::unexpected_named_attribute_argument(self.binder, name)
                            .report(reporter),
                    ))
                }
            }
            None => {
                // @Beacon @Task span
                Err(AttributeParsingError::Erased(
                    Diagnostic::error()
                        .message("unexpected named attribute argument")
                        .report(reporter),
                ))
            }
        }
    }
}

trait LintExt: Sized {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError>;
}

impl LintExt for lo_ast::attribute::Lint {
    fn parse(binder: Path, reporter: &Reporter) -> Result<Self, AttributeParsingError> {
        Err(AttributeParsingError::Erased(
            Diagnostic::error()
                .code(ErrorCode::E018)
                .message(format!("the lint ‘{binder}’ is not defined"))
                .unlabeled_span(binder.span())
                .report(reporter),
        ))
    }
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    // @Temporary signature
    pub(super) fn unexpected_named_attribute_argument(
        actual: ast::Identifier,
        expected: Atom,
    ) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E028)
            .message(format!(
                "found named argument ‘{actual}’ but expected ‘{expected}’"
            ))
            .unlabeled_span(actual)
    }

    // @Temporary signature
    pub(super) fn invalid_attribute_argument_type(
        actual: Spanned<&'static str>,
        expected: &'static str,
    ) -> Diagnostic {
        Diagnostic::error()
            .code(ErrorCode::E027)
            .message(format!("found {actual} but expected {expected}"))
            .unlabeled_span(actual)
    }
}
