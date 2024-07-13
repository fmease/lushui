use crate::{Lowerer, Options};
use ast::Path;
use diagnostics::{
    error::{Health, Outcome, Result},
    reporter::ErasedReportedError,
    Diag, ErrorCode, Reporter,
};
use lo_ast::{
    attr::{Predicate, Query, Special, Target},
    AttrName, Attrs, BareAttr,
};
use session::Session;
use span::{Span, Spanned, Spanning};
use utility::default;
use utility::{Atom, Conjunction, ListingExt, QuoteExt};

impl Lowerer<'_> {
    /// Lower attributes.
    // @Task filter out documentation attributes if !options.keep_documentation_comments
    pub(crate) fn lower_attrs<T: Target>(
        &mut self,
        unchecked_attrs: &[ast::Attr],
        target: &T,
        cx: T::Context,
    ) -> Attrs {
        use lo_ast::Attr;

        let actual_targets = target.targets(cx);
        let mut conforming_attrs = Attrs::default();

        for attr in unchecked_attrs {
            let attr = match Attr::parse(attr, &self.opts, self.sess) {
                Ok(attr) => attr,
                Err(error) => {
                    self.health.taint(error);
                    continue;
                }
            };

            // search for non-conforming attributes
            {
                let expected_targets = attr.bare.targets();
                if !expected_targets.contains(actual_targets) {
                    // @Question wording: "cannot be ascribed to"?
                    Diag::error()
                        .code(ErrorCode::E013)
                        .message(format!(
                            "attribute ‘{}’ is ascribed to {}",
                            attr.bare.name(),
                            target.name(cx)
                        ))
                        .span(&attr, "misplaced attribute")
                        .label(target, "incompatible item")
                        .note(format!(
                            "attribute ‘{}’ can only be ascribed to {}",
                            attr.bare.name(),
                            expected_targets.description(),
                        ))
                        .handle(&mut *self);
                    continue;
                }
            }

            conforming_attrs.0.push(attr);
        }

        self.check_attr_synergy(&conforming_attrs)
    }

    pub(crate) fn check_attr_synergy(&mut self, conforming_attrs: &Attrs) -> Attrs {
        let mut attrs = Attrs::default();

        // search for conflicting or duplicate attributes
        for attr in &conforming_attrs.0 {
            if attr.bare.can_be_applied_multiple_times() {
                attrs.0.push(attr.clone());
                continue;
            }

            let is_homonymous = Predicate(|some_attr| some_attr.name() == attr.bare.name());

            let homonymous_attrs: Vec<_> = conforming_attrs.filter(is_homonymous).collect();

            if !attrs.has(is_homonymous) {
                attrs.0.push(attr.clone());
            }

            if let [first, _second, ..] = &*homonymous_attrs {
                Diag::error()
                    .code(ErrorCode::E006)
                    .message(format!("multiple ‘{}’ attributes", first.bare.name()))
                    .spans(homonymous_attrs, "duplicate or conflicting attribute")
                    .handle(&mut *self);
            }
        }

        // No further checks necessary if empty.
        if attrs.0.is_empty() {
            return attrs;
        }

        {
            use AttrName::*;
            self.check_mutual_exclusivity(Intrinsic.or(Known), &attrs);
            self.check_mutual_exclusivity(Moving.or(Abstract), &attrs);
        }

        for attribute in attrs.filter(Predicate(|attribute| !attribute.is_implemented())) {
            Diag::error()
                .message(format!(
                    "the attribute ‘{}’ is not supported yet",
                    attribute.bare.name()
                ))
                .unlabeled_span(attribute)
                .handle(&mut *self);
        }

        // @Task replace this concept with a feature system
        if !self.opts.internal_features_enabled {
            for attr in attrs.filter(Predicate(BareAttr::is_internal)) {
                Diag::error()
                    .code(ErrorCode::E038)
                    .message(format!(
                        "the attribute ‘{}’ is an internal feature",
                        attr.bare.name()
                    ))
                    .unlabeled_span(attr)
                    .handle(&mut *self);
            }
        }

        attrs
    }

    fn check_mutual_exclusivity<Q: Query>(&mut self, query: Q, attrs: &Attrs) {
        let attrs = attrs.filter(query).collect::<Vec<_>>();

        if attrs.len() > 1 {
            let listing = attrs
                .iter()
                .map(|attribute| attribute.bare.name().to_str().quote())
                .list(Conjunction::And);

            Diag::error()
                .code(ErrorCode::E014)
                .message(format!("attributes {listing} are mutually exclusive"))
                .spans(attrs, "conflicting attribute")
                .handle(self);
        }
    }
}

// @Task dedup! (also found in the resolver/literal)
const NAT32_INTERVAL_REPR: &str = "[0, 2^32-1]";

trait AttrExt: Sized {
    fn parse(attr: &ast::Attr, options: &Options, sess: &Session<'_>) -> Result<Self>;
}

impl AttrExt for lo_ast::Attr {
    fn parse(attr: &ast::Attr, options: &Options, sess: &Session<'_>) -> Result<Self> {
        Ok(Self::new(attr.span, BareAttr::parse(attr, options, sess)?))
    }
}

trait BareAttrExt: Sized {
    fn parse(attr: &ast::Attr, options: &Options, sess: &Session<'_>) -> Result<Self>;
}

impl BareAttrExt for lo_ast::BareAttr {
    // @Task allow unordered named attributes e.g. `@(unstable (reason "x") (feature thing))`
    fn parse(
        // @Task take by value and create parsing helpers on ast::Attr and ast::Attrs
        attr: &ast::Attr,
        options: &Options,
        sess: &Session<'_>,
    ) -> Result<Self> {
        let ast::BareAttr::Reg {
            binder,
            args: arguments,
        } = &attr.bare
        else {
            return Ok(Self::Doc {
                content: if options.keep_doc_comments {
                    sess.shared_map()
                        .snippet(attr.span)
                        .trim_start_matches(";;")
                        .to_owned()
                        .into()
                } else {
                    default()
                },
            });
        };

        let args: &mut &[_] = &mut &**arguments;

        fn opt_arg<'a>(args: &mut &'a [ast::AttrArg]) -> Option<&'a ast::AttrArg> {
            args.first().map(|arg| {
                *args = &args[1..];
                arg
            })
        }

        // @Task improve API
        fn arg<'a>(
            args: &mut &'a [ast::AttrArg],
            span: Span,
            rep: &Reporter,
        ) -> Result<&'a ast::AttrArg, AttrParseErr> {
            let argument = args.first().ok_or_else(|| {
                // @Task add more information about the arity and the argument types
                AttrParseErr::Erased(
                    Diag::error()
                        .code(ErrorCode::E019)
                        .message("too few attribute arguments provided")
                        .unlabeled_span(span)
                        .report(rep),
                )
            })?;
            *args = &args[1..];
            Ok(argument)
        }

        let result = (|| {
            use AttrName::*;

            let name =
                AttrName::parse(binder.bare()).ok_or(AttrParseErr::UndefinedAttribute(*binder))?;

            Ok(match name {
                Abstract => Self::Abstract,
                Allow | Deny | Forbid | Warn => {
                    let lint = lo_ast::attr::Lint::parse(
                        arg(args, attr.span, sess.rep())?
                            .path(Some(Atom::LINT), sess.rep())?
                            .clone(),
                        sess.rep(),
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
                Deprecated => Self::Deprecated(lo_ast::attr::Deprecated {
                    reason: opt_arg(args)
                        .map(|argument| argument.text_lit(Some(Atom::REASON), sess.rep()))
                        .transpose()?,
                    // @Task parse version
                    since: None,
                    // @Task parse version
                    removal: None,
                    replacement: opt_arg(args)
                        .map(|argument| argument.text_lit(Some(Atom::REPLACEMENT), sess.rep()))
                        .transpose()?,
                }),
                Doc => Self::Doc {
                    content: if options.keep_doc_comments {
                        arg(args, attr.span, sess.rep())?
                            .text_lit(None, sess.rep())?
                            .to_str()
                            .into()
                    } else {
                        *args = &args[1..];
                        default()
                    },
                },
                Intrinsic => {
                    let name = opt_arg(args)
                        .map(|argument| argument.path(Some(Atom::NAME), sess.rep()))
                        .transpose()?
                        .cloned();

                    Self::Intrinsic(Special { name })
                }
                If => {
                    return Err(AttrParseErr::Erased(
                        Diag::error()
                            .message("the attribute ‘if’ is not supported yet")
                            .unlabeled_span(attr)
                            .report(sess.rep()),
                    ));
                }
                Ignore => Self::Ignore,
                Include => Self::Include,
                Known => {
                    let name = opt_arg(args)
                        .map(|argument| argument.path(Some(Atom::NAME), sess.rep()))
                        .transpose()?
                        .cloned();

                    Self::Known(Special { name })
                }
                Location => {
                    let path =
                        arg(args, attr.span, sess.rep())?.text_lit(Some(Atom::PATH), sess.rep())?;

                    Self::Location { path }
                }
                Moving => Self::Moving,
                Public => {
                    let reach = opt_arg(args)
                        .map(|argument| argument.path(Some(Atom::REACH), sess.rep()))
                        .transpose()?
                        .cloned();

                    Self::Public(lo_ast::attr::Public { reach })
                }
                Record => Self::Record,
                RecursionLimit => {
                    let depth = arg(args, attr.span, sess.rep())?;
                    let depth = depth
                        .num_lit(Some(Atom::DEPTH), sess.rep())?
                        .to_str()
                        .parse::<u32>()
                        .map_err(|_| {
                            AttrParseErr::Erased(
                                Diag::error()
                                    .code(ErrorCode::E008)
                                    .message(format!(
                                        "attribute argument does not fit integer interval \
                                        {NAT32_INTERVAL_REPR}",
                                    ))
                                    .unlabeled_span(depth)
                                    .report(sess.rep()),
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
                    return Err(AttrParseErr::Erased(
                        Diag::error()
                            .message("the attribute ‘unstable’ is not supported yet")
                            .unlabeled_span(attr)
                            .report(sess.rep()),
                    ));
                }
            })
        })();

        let mut health = Health::Untainted;

        if !matches!(result, Err(AttrParseErr::UndefinedAttribute(_))) {
            // if there are still unparsed arguments it means too many arguments were provided
            // unless the attribute does not exist in the first place since in such case,
            // no argument was parsed either
            if let Some(argument) = args.first() {
                let error = Diag::error()
                    .code(ErrorCode::E019)
                    .message("too many attribute arguments provided")
                    .unlabeled_span(argument.span.merge(&args.last()))
                    .report(sess.rep());
                health.taint(error);
            }
        }

        result
            .map_err(|error| match error {
                AttrParseErr::UndefinedAttribute(binder) => Diag::error()
                    .code(ErrorCode::E011)
                    .message(format!("the attribute ‘{binder}’ is not defined"))
                    .unlabeled_span(binder)
                    .report(sess.rep()),
                AttrParseErr::Erased(error) => error,
            })
            .and_then(|attributes| Outcome::new(attributes, health).into())
    }
}

enum AttrParseErr {
    /// Some opaque error that was already reported.
    Erased(ErasedReportedError),
    UndefinedAttribute(ast::Ident),
}

// @Beacon hideous accessors! @Task deduplicate!
// @Beacon @Task don't use reporter+Result<_> here but
// a custom error type (w/o ::Unrecoverable)
// and turn that stuff into stuff later

trait AttributeArgumentExt {
    fn num_lit(&self, name: Option<Atom>, rep: &Reporter) -> Result<Atom, AttrParseErr>;

    fn text_lit(&self, name: Option<Atom>, rep: &Reporter) -> Result<Atom, AttrParseErr>;

    fn path(&self, name: Option<Atom>, rep: &Reporter) -> Result<&Path, AttrParseErr>;
}

impl AttributeArgumentExt for ast::AttrArg {
    fn num_lit(&self, name: Option<Atom>, rep: &Reporter) -> Result<Atom, AttrParseErr> {
        use ast::BareAttrArg::*;

        match &self.bare {
            &NumLit(lit) => Ok(lit),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        NumLit(literal) => Ok(literal),
                        bare => Err(AttrParseErr::Erased(
                            error::invalid_attribute_argument_ty(
                                Spanned::new(argument.span, bare.name()),
                                "number literal",
                            )
                            .report(rep),
                        )),
                    },
                    rep,
                )
                .copied(),
            bare => Err(AttrParseErr::Erased(
                error::invalid_attribute_argument_ty(
                    Spanned::new(self.span, bare.name()),
                    "positional or named number literal",
                )
                .report(rep),
            )),
        }
    }

    fn text_lit(&self, name: Option<Atom>, rep: &Reporter) -> Result<Atom, AttrParseErr> {
        use ast::BareAttrArg::*;

        match &self.bare {
            &TextLit(text) => Ok(text),
            Named(named) => named
                .handle(
                    name,
                    |arg| match &arg.bare {
                        TextLit(text) => Ok(text),
                        bare => Err(AttrParseErr::Erased(
                            error::invalid_attribute_argument_ty(
                                Spanned::new(arg.span, bare.name()),
                                "text literal",
                            )
                            .report(rep),
                        )),
                    },
                    rep,
                )
                .copied(),
            bare => Err(AttrParseErr::Erased(
                error::invalid_attribute_argument_ty(
                    Spanned::new(self.span, bare.name()),
                    "positional or named text literal",
                )
                .report(rep),
            )),
        }
    }

    fn path(&self, name: Option<Atom>, rep: &Reporter) -> Result<&Path, AttrParseErr> {
        use ast::BareAttrArg::*;

        match &self.bare {
            Path(literal) => Ok(literal),
            Named(named) => named
                .handle(
                    name,
                    |argument| match &argument.bare {
                        Path(literal) => Ok(literal),
                        bare => Err(AttrParseErr::Erased(
                            error::invalid_attribute_argument_ty(
                                Spanned::new(argument.span, bare.name()),
                                "path",
                            )
                            .report(rep),
                        )),
                    },
                    rep,
                )
                .map(|path| &**path),
            bare => Err(AttrParseErr::Erased(
                error::invalid_attribute_argument_ty(
                    Spanned::new(self.span, bare.name()),
                    "positional or named path",
                )
                .report(rep),
            )),
        }
    }
}

trait NamedAttrArgExt {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttrArg) -> Result<&T, AttrParseErr>,
        rep: &Reporter,
    ) -> Result<&T, AttrParseErr>;
}

impl NamedAttrArgExt for ast::NamedAttrArg {
    fn handle<T>(
        &self,
        name: Option<Atom>,
        handle: impl FnOnce(&ast::AttrArg) -> Result<&T, AttrParseErr>,
        rep: &Reporter,
    ) -> Result<&T, AttrParseErr> {
        match name {
            Some(name) => {
                if self.binder.bare() == name {
                    handle(&self.value)
                } else {
                    Err(AttrParseErr::Erased(
                        error::unexpected_named_attribute_argument(self.binder, name).report(rep),
                    ))
                }
            }
            None => {
                // @Beacon @Task span
                Err(AttrParseErr::Erased(
                    Diag::error()
                        .message("unexpected named attribute argument")
                        .report(rep),
                ))
            }
        }
    }
}

trait LintExt: Sized {
    fn parse(binder: Path, rep: &Reporter) -> Result<Self, AttrParseErr>;
}

impl LintExt for lo_ast::attr::Lint {
    fn parse(binder: Path, rep: &Reporter) -> Result<Self, AttrParseErr> {
        Err(AttrParseErr::Erased(
            Diag::error()
                .code(ErrorCode::E018)
                .message(format!("the lint ‘{binder}’ is not defined"))
                .unlabeled_span(binder.span())
                .report(rep),
        ))
    }
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    // @Temporary signature
    pub(super) fn unexpected_named_attribute_argument(actual: ast::Ident, expected: Atom) -> Diag {
        Diag::error()
            .code(ErrorCode::E028)
            .message(format!(
                "found named argument ‘{actual}’ but expected ‘{expected}’"
            ))
            .unlabeled_span(actual)
    }

    // @Temporary signature
    pub(super) fn invalid_attribute_argument_ty(
        actual: Spanned<&'static str>,
        expected: &'static str,
    ) -> Diag {
        Diag::error()
            .code(ErrorCode::E027)
            .message(format!("found {actual} but expected {expected}"))
            .unlabeled_span(actual)
    }
}
