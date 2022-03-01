use crate::{
    component::ComponentType,
    diagnostics::{reporter::ErrorReported, Code, Diagnostic, Reporter},
    error::{AndThenMapExt, Health, OkIfUntaintedExt, Result},
    metadata::{self, convert, key_content_span, Map, MapWalker, Value},
    span::{SourceFileIndex, SourceMap, SourceMapCell, Spanned, WeaklySpanned},
    syntax::Word,
    utility::{try_all, Conjunction, HashMap, QuoteExt, UnorderedListingExt},
};
use derivation::{Elements, FromStr, Str};
use std::{fmt, path::PathBuf, str::FromStr};

// @Task implement notion of "templates" ("common stanzas" in Cabal speak)

// @Note missing span of PackageManifest itself
pub struct PackageManifest {
    pub(crate) profile: PackageProfile,
    pub(crate) components: Option<Spanned<Vec<ComponentManifest>>>,
}

impl PackageManifest {
    pub const FILE_NAME: &'static str = "package.metadata";

    pub(crate) fn parse(
        file: SourceFileIndex,
        map: SourceMapCell,
        reporter: &Reporter,
    ) -> Result<Self> {
        let manifest = metadata::parse(file, map.clone(), reporter)?;

        let manifest = metadata::convert(manifest, reporter)?;
        let mut manifest = MapWalker::new(manifest, reporter);

        let name = manifest
            .take("name")
            .map(trim_quotes)
            .and_then(|name| parse_name(name, NameKind::Package, reporter));

        let version = manifest.take("version");
        let description = manifest.take_optional("description");

        let components = manifest
            .take_optional("components")
            .and_then_map(|components| parse_components(components, &map.borrow(), reporter));

        manifest.exhaust()?;

        // @Task try to get rid of the unchecked call
        try_all! {
            name, version, description,
            components;
            return Err(ErrorReported::new_unchecked())
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
    Spanned { value: name, span }: Spanned<String>,
    kind: NameKind,
    reporter: &Reporter,
) -> Result<Spanned<Word>> {
    Word::parse(name.clone())
        .map(|name| Spanned::new(span, name))
        .map_err(|_| {
            // @Task DRY, @Question is the common code justified? package v component
            Diagnostic::error()
                .code(Code::E036)
                .message(format!("the {kind} name `{name}` is not a valid word",))
                .primary_span(span)
                .report(reporter)
        })
}

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
    untyped_components: Spanned<Vec<Value>>,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Spanned<Vec<ComponentManifest>>> {
    let health = &mut Health::Untainted;
    let mut components = Vec::new();

    for untyped_component in untyped_components.value {
        let Ok(component) = convert(untyped_component, reporter) else {
            health.taint();
            continue;
        };
        let mut component = MapWalker::new(component, reporter);

        let type_ =
            component
                .take::<String>("type")
                .map(trim_quotes)
                .and_then(|Spanned!(type_, span)| {
                    ComponentType::from_str(&type_)
                        .map(|type_| Spanned::new(span, type_))
                        .map_err(|_| {
                            Diagnostic::error()
                                .message(format!("`{type_}` is not a valid component type"))
                                .primary_span(span)
                                .note(format!(
                                    "valid component types are {}",
                                    ComponentType::elements()
                                        .map(|element| element.name())
                                        .map(QuoteExt::quote)
                                        .list(Conjunction::And)
                                ))
                                .report(reporter)
                        })
                });

        let name = component
            .take_optional("name")
            .and_then_map(|name| parse_name(trim_quotes(name), NameKind::Component, reporter));

        let path = component.take::<String>("path").map(trim_quotes);

        let dependencies = component
            .take_optional("dependencies")
            .and_then_map(|dependencies| parse_dependencies(dependencies, map, reporter));

        let exhaustion = component.exhaust();

        try_all! {
            type_, name, path, dependencies, exhaustion;
            return Err(ErrorReported::new_unchecked())
        };

        components.push(ComponentManifest {
            name,
            path: path.map(Into::into),
            type_,
            dependencies,
        });
    }

    Result::ok_if_untainted(Spanned::new(untyped_components.span, components), *health)
}

fn parse_dependencies(
    untyped_dependencies: Spanned<Map>,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>> {
    let health = &mut Health::Untainted;
    let mut dependencies = HashMap::default();

    for (exonym, declaration) in untyped_dependencies.value {
        let exonym = exonym.strong().map_span(|span| key_content_span(span, map));
        let exonym = parse_name(exonym, NameKind::Component, reporter).map(Spanned::weak);

        let Ok(declaration) = convert(declaration, reporter) else {
            health.taint();
            continue;
        };
        let mut declaration = MapWalker::new(declaration, reporter);

        let endonym = declaration
            .take_optional("name")
            .and_then_map(|name| parse_name(trim_quotes(name), NameKind::Component, reporter));

        let package = declaration
            .take_optional("package")
            .and_then_map(|name| parse_name(trim_quotes(name), NameKind::Package, reporter));

        let provider = declaration
            .take_optional::<String>("provider")
            .and_then_map(|name| {
                let Spanned!(name, span) = trim_quotes(name);
                Provider::from_str(&name)
                    .map(|name| Spanned::new(span, name))
                    .map_err(|_| {
                        // @Task code
                        Diagnostic::error()
                            .message(format!("`{name}` is not a valid dependency provider"))
                            .primary_span(span)
                            .note(format!(
                                "valid providers are {}",
                                Provider::elements()
                                    .map(QuoteExt::quote)
                                    .list(Conjunction::And)
                            ))
                            .report(reporter)
                    })
            });

        let version = declaration.take_optional::<String>("version");
        let path = declaration.take_optional::<String>("path");
        let span = declaration.exhaust();

        try_all! {
            exonym, endonym, package, provider, version, path, span;
            health.taint(); continue
        };

        dependencies.insert(
            exonym,
            Spanned::new(
                span,
                DependencyDeclaration {
                    name: endonym,
                    package,
                    provider,
                    version: version.map(|version| version.map(VersionRequirement)),
                    path: path.map(|path| trim_quotes(path.map(Into::into))),
                },
            ),
        );
    }

    Result::ok_if_untainted(
        Spanned::new(untyped_dependencies.span, dependencies),
        *health,
    )
}

fn trim_quotes<T>(value: Spanned<T>) -> Spanned<T> {
    value.map_span(|span| span.trim(1))
}

pub(crate) struct PackageProfile {
    pub(crate) name: Spanned<Word>,
    pub(crate) version: Spanned<Version>,
    pub(crate) description: Option<Spanned<String>>,
}

pub(crate) struct ComponentManifest {
    pub(crate) name: Option<Spanned<Word>>,
    pub(crate) path: Spanned<PathBuf>,
    pub(crate) type_: Spanned<ComponentType>,
    pub(crate) dependencies:
        Option<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>>,
}

#[derive(Clone, Debug)]
pub(crate) struct DependencyDeclaration {
    pub(crate) name: Option<Spanned<Word>>,
    #[allow(dead_code)] // @Temporary
    pub(crate) package: Option<Spanned<Word>>,
    pub(crate) provider: Option<Spanned<Provider>>,
    #[allow(dead_code)]
    pub(crate) version: Option<Spanned<VersionRequirement>>,
    pub(crate) path: Option<Spanned<PathBuf>>,
}

#[derive(Clone, Copy, Debug, Elements, FromStr, Str)]
#[format(dash_case)]
pub(crate) enum Provider {
    Package,
    Filesystem,
    Distribution,
    Git,
    Registry,
}

impl fmt::Display for Provider {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug)]
pub(crate) struct Version(pub(crate) String);

#[derive(Clone, Debug)]
pub(crate) struct VersionRequirement(pub(crate) String);
