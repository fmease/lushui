use super::BuildQueue;
use derivation::{Elements, FromStr, Str};
use diagnostics::{
    error::{Health, Outcome, Result},
    reporter::ErasedReportedError,
    Diag, ErrorCode, Reporter,
};
use recnot::{convert, Record, RecordWalker, WithTextContentSpanExt};
use session::{package::Version, unit::CompTy};

use lexer::word::Word;
use span::{SourceMap, Spanned, SrcFileIdx, WeaklySpanned};
use std::{fmt, path::PathBuf, str::FromStr};
use utility::{try_all, AndThenMapExt, Conjunction, HashMap, ListingExt, QuoteExt};

pub(super) struct PackageManifest {
    pub(super) profile: PackageProfile,
    pub(super) components: Option<Spanned<Components>>,
}

impl PackageManifest {
    pub(super) fn parse(file: SrcFileIdx, queue: &BuildQueue) -> Result<Self> {
        let manifest = recnot::parse(file, &queue.map, &queue.rep)?;
        let manifest = recnot::convert(manifest, &queue.rep)?;
        let mut manifest = RecordWalker::new(manifest, &queue.rep);

        let name = manifest
            .take("name")
            .map(|name| name.with_text_content_span(&queue.shared_map()))
            .and_then(|name| parse_name(name, NameKind::Package, &queue.rep));

        let version = manifest.take("version");
        let description = manifest.take_optional("description");

        let components = manifest
            .take_optional("components")
            .and_then_map(|components| {
                parse_components(components, &queue.shared_map(), &queue.rep)
            });

        manifest.exhaust()?;

        try_all! {
            name, version, description, components;
            return Err(ErasedReportedError::new_unchecked())
        };

        Ok(PackageManifest {
            profile: PackageProfile {
                name,
                version: version.map(Version),
                description,
            },
            components,
        })
    }
}

fn parse_name(
    Spanned { bare: name, span }: Spanned<String>,
    kind: NameKind,
    rep: &Reporter,
) -> Result<Spanned<Word>> {
    Word::parse(name.clone())
        .map(|name| Spanned::new(span, name))
        .map_err(|()| {
            // @Task DRY, @Question is the common code justified? package v component
            Diag::error()
                .code(ErrorCode::E036)
                .message(format!("the {kind} name ‘{name}’ is not a valid word"))
                .unlabeled_span(span)
                .report(rep)
        })
}

#[derive(Clone, Copy)]
enum NameKind {
    Package,
    Component,
}

impl fmt::Display for NameKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::Package => "package",
            Self::Component => "component",
        })
    }
}

fn parse_components(
    untyped_components: Spanned<recnot::Record>,
    map: &SourceMap,
    rep: &Reporter,
) -> Result<Spanned<Components>> {
    let mut components = Components::default();
    let mut health = Health::Untainted;

    for (name, untyped_component) in untyped_components.bare {
        let component = match convert(untyped_component, rep) {
            Ok(component) => component,
            Err(error) => {
                health.taint(error);
                continue;
            }
        };
        let mut component = RecordWalker::new(component, rep);

        let name = parse_name(name.strong(), NameKind::Component, rep).map(Spanned::weak);

        let public = component.take_optional("public");

        let ty = component
            .take::<String>("type")
            .map(|ty| ty.with_text_content_span(map))
            .and_then(|ty| parse_component_ty(ty, rep));

        let path = component
            .take::<String>("path")
            .map(|path| path.with_text_content_span(map));

        let dependencies = component
            .take_optional("dependencies")
            .and_then_map(|dependencies| parse_dependencies(dependencies, map, rep));

        let span = component.exhaust();

        try_all! {
            name, public, ty, path, dependencies, span;
            health.taint(ErasedReportedError::new_unchecked()); // @Task don't call new_unchecked here!
            continue
        };

        components.insert(
            name,
            Spanned::new(
                span,
                ComponentManifest {
                    ty,
                    public,
                    path: path.map(Into::into),
                    dependencies,
                },
            ),
        );
    }

    Outcome::new(Spanned::new(untyped_components.span, components), health).into()
}

fn parse_component_ty(
    Spanned!(span, ty): Spanned<String>,
    rep: &Reporter,
) -> Result<Spanned<CompTy>, ErasedReportedError> {
    CompTy::from_str(&ty)
        .map(|ty| Spanned::new(span, ty))
        .map_err(|()| {
            // @Task don't list all component types unconditionally:
            //       if the invalid type is_similar to a valid one, give a more
            //       fine-tuned suggestion
            Diag::error()
                .message(format!("‘{ty}’ is not a valid component type"))
                .unlabeled_span(span)
                .note(format!(
                    "valid component types are {}",
                    CompTy::elements()
                        .map(|element| element.name())
                        .map(QuoteExt::quote)
                        .list(Conjunction::And)
                ))
                .report(rep)
        })
}

fn parse_dependencies(
    untyped_dependencies: Spanned<Record>,
    map: &SourceMap,
    rep: &Reporter,
) -> Result<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>> {
    let mut health = Health::Untainted;
    let mut dependencies = HashMap::default();

    for (component_exonym, declaration) in untyped_dependencies.bare {
        let component_exonym = component_exonym.strong().with_text_content_span(map);
        let exonym = parse_name(component_exonym, NameKind::Component, rep).map(Spanned::weak);

        let declaration = match convert(declaration, rep) {
            Ok(declaration) => declaration,
            Err(error) => {
                health.taint(error);
                continue;
            }
        };
        let mut declaration = RecordWalker::new(declaration, rep);

        let component_endonym = declaration.take_optional("component").and_then_map(|name| {
            parse_name(name.with_text_content_span(map), NameKind::Component, rep)
        });

        let provider = declaration
            .take_optional::<String>("provider")
            .and_then_map(|name| {
                let Spanned!(span, name) = name.with_text_content_span(map);
                DependencyProvider::from_str(&name)
                    .map(|name| Spanned::new(span, name))
                    .map_err(|()| {
                        // @Task code
                        // @Task don't list all providers unconditionally:
                        //       if the invalid provider is_similar to a valid one, give a more
                        //       fine-tuned suggestion
                        Diag::error()
                            .message(format!("‘{name}’ is not a valid dependency provider"))
                            .unlabeled_span(span)
                            .note(format!(
                                "valid dependency providers are {}",
                                DependencyProvider::elements()
                                    .map(QuoteExt::quote)
                                    .list(Conjunction::And)
                            ))
                            .report(rep)
                    })
            });

        let version = declaration.take_optional::<String>("version");
        let path = declaration.take_optional::<String>("path");
        let package = declaration.take_optional("package").and_then_map(|name| {
            parse_name(name.with_text_content_span(map), NameKind::Package, rep)
        });
        let public = declaration.take_optional("public");

        let span = declaration.exhaust();

        try_all! {
            exonym, component_endonym, provider, version, path, package, public, span;
            health.taint(ErasedReportedError::new_unchecked()); // @Task don't call new_unchecked here!
            continue
        };

        dependencies.insert(
            exonym,
            Spanned::new(
                span,
                DependencyDeclaration {
                    component: component_endonym,
                    provider,
                    version: version.map(|version| version.map(VersionRequirement)),
                    path: path.map(|path| path.map(Into::into).with_text_content_span(map)),
                    package,
                    public,
                },
            ),
        );
    }

    Outcome::new(
        Spanned::new(untyped_dependencies.span, dependencies),
        health,
    )
    .into()
}

pub(super) struct PackageProfile {
    pub(super) name: Spanned<Word>,
    pub(super) version: Spanned<Version>,
    pub(super) description: Option<Spanned<String>>,
}

pub(super) type Components = HashMap<WeaklySpanned<Word>, Spanned<ComponentManifest>>;

pub(super) struct ComponentManifest {
    pub(super) ty: Spanned<CompTy>,
    pub(super) public: Option<Spanned<bool>>,
    /// The path to the component root (relative to the package).
    pub(super) path: Spanned<PathBuf>,
    pub(super) dependencies:
        Option<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>>,
}

#[derive(Clone, Debug)]
pub(super) struct DependencyDeclaration {
    pub(super) component: Option<Spanned<Word>>,
    pub(super) provider: Option<Spanned<DependencyProvider>>,
    #[allow(dead_code)]
    pub(super) version: Option<Spanned<VersionRequirement>>,
    pub(super) path: Option<Spanned<PathBuf>>,
    pub(super) package: Option<Spanned<Word>>,
    pub(super) public: Option<Spanned<bool>>,
}

#[derive(Clone, Copy, Debug, Elements, FromStr, Str, PartialEq, Eq)]
#[format(dash_case)]
pub(super) enum DependencyProvider {
    Package,
    Filesystem,
    Distribution,
    Git,
    Registry,
}

impl fmt::Display for DependencyProvider {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Clone, Debug)]
pub(super) struct VersionRequirement(#[allow(dead_code)] pub(crate) String);
