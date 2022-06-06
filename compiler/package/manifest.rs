use super::BuildQueue;
use crate::{
    component::ComponentType,
    diagnostics::{reporter::ErasedReportedError, Code, Diagnostic, Reporter},
    error::{AndThenMapExt, Health, OkIfUntaintedExt, Result},
    metadata::{self, convert, Record, RecordWalker, Value, WithTextContentSpanExt},
    span::{SourceFileIndex, SourceMap, Span, Spanned, WeaklySpanned},
    syntax::Word,
    utility::{try_all, Conjunction, HashMap, ListingExt, QuoteExt, SmallVec},
};
use derivation::{Elements, FromStr, Str};
use smallvec::smallvec;
use std::{default::default, fmt, path::PathBuf, str::FromStr};

pub const FILE_NAME: &str = "package.metadata";

pub(super) struct PackageManifest {
    pub(super) profile: PackageProfile,
    pub(super) components: Option<Spanned<Components>>,
}

impl PackageManifest {
    pub(super) fn parse(file: SourceFileIndex, queue: &BuildQueue) -> Result<Self> {
        let manifest = metadata::parse(file, &queue.map, &queue.reporter)?;
        let manifest = metadata::convert(manifest, &queue.reporter)?;
        let mut manifest = RecordWalker::new(manifest, &queue.reporter);

        let name = manifest
            .take("name")
            .map(|name| name.with_text_content_span(&queue.shared_map()))
            .and_then(|name| parse_name(name, NameKind::Package, &queue.reporter));

        let version = manifest.take("version");
        let description = manifest.take_optional("description");

        let components = manifest
            .take_optional("components")
            .and_then_map(|components| {
                parse_components(
                    components,
                    name.as_ref().ok().map(|name| &name.bare),
                    &queue.shared_map(),
                    &queue.reporter,
                )
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
    reporter: &Reporter,
) -> Result<Spanned<Word>> {
    Word::parse(name.clone())
        .map(|name| Spanned::new(span, name))
        .map_err(|_| {
            // @Task DRY, @Question is the common code justified? package v component
            Diagnostic::error()
                .code(Code::E036)
                .message(format!("the {kind} name ‘{name}’ is not a valid word"))
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
    Spanned!(span, untyped_components): Spanned<Vec<Value>>,
    package: Option<&Word>,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Spanned<Components>> {
    let mut components: HashMap<ComponentKey, Spanned<ComponentManifest>> = default();
    let mut conflicts: HashMap<ComponentKey, SmallVec<Span, 2>> = default();
    let mut health = Health::Untainted;

    for untyped_component in untyped_components {
        let Ok(component) = convert(untyped_component, reporter) else {
            health.taint();
            continue;
        };
        let mut component = RecordWalker::new(component, reporter);

        let name = component.take_optional("name").and_then_map(|name| {
            parse_name(
                name.with_text_content_span(map),
                NameKind::Component,
                reporter,
            )
        });

        let public = component.take_optional("public");

        let type_ = component
            .take::<String>("type")
            .map(|type_| type_.with_text_content_span(map))
            .and_then(|type_| parse_component_type(type_, reporter));

        let path = component
            .take::<String>("path")
            .map(|path| path.with_text_content_span(map));

        let dependencies = component
            .take_optional("dependencies")
            .and_then_map(|dependencies| parse_dependencies(dependencies, map, reporter));

        let span = component.exhaust();

        try_all! {
            name, public, type_, path, dependencies, span;
            health.taint();
            continue
        };

        let key = ComponentKey {
            name: name
                .filter(|name| package.map_or(true, |package| &name.bare != package))
                .map(Spanned::weak),
            type_: type_.weak(),
        };

        if let Some((previous, _)) = components.get_key_value(&key) {
            // Use the span of the name or alternatively the type instead of the whole
            // component manifest since in most cases it will lead to more readable / useful highlights.
            // Highlighting a multi-line component manifest would just yield two lines of curly brackets.
            let span_from_key = |key: &ComponentKey| {
                key.name
                    .as_ref()
                    .map_or_else(|| key.type_.span, |name| name.span)
            };
            let span = span_from_key(&key);
            conflicts
                .entry(key)
                .or_insert_with(|| smallvec![span_from_key(previous)])
                .push(span);
        } else {
            components.insert(
                key,
                Spanned::new(
                    span,
                    ComponentManifest {
                        public,
                        path: path.map(Into::into),
                        dependencies,
                    },
                ),
            );
        }
    }

    if !conflicts.is_empty() {
        for (ComponentKey { name, type_ }, spans) in conflicts {
            Diagnostic::error()
                .message(format!(
                    "the {} is defined multiple times",
                    match name {
                        Some(name) => format!("{type_} component ‘{name}’"),
                        None => format!("primary {type_} component"),
                    }
                ))
                .labeled_primary_spans(spans, "conflicting component")
                .report(reporter);
        }
        health.taint();
    }

    Result::ok_if_untainted(Spanned::new(span, components), health)
}

fn parse_component_type(
    Spanned!(span, type_): Spanned<String>,
    reporter: &Reporter,
) -> Result<Spanned<ComponentType>, ErasedReportedError> {
    ComponentType::from_str(&type_)
        .map(|type_| Spanned::new(span, type_))
        .map_err(|_| {
            // @Task don't list all component types unconditionally:
            //       if the invalid type is_similar to a valid one, give a more
            //       fine-tuned suggestion
            Diagnostic::error()
                .message(format!("‘{type_}’ is not a valid component type"))
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
}

fn parse_dependencies(
    Spanned!(span, untyped_dependencies): Spanned<Record>,
    map: &SourceMap,
    reporter: &Reporter,
) -> Result<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>> {
    let mut health = Health::Untainted;
    let mut dependencies = HashMap::default();

    for (component_exonym, declaration) in untyped_dependencies {
        let component_exonym = component_exonym.strong().with_text_content_span(map);
        let exonym = parse_name(component_exonym, NameKind::Component, reporter).map(Spanned::weak);

        let Ok(declaration) = convert(declaration, reporter) else {
            health.taint();
            continue;
        };
        let mut declaration = RecordWalker::new(declaration, reporter);

        let component_endonym = declaration.take_optional("component").and_then_map(|name| {
            parse_name(
                name.with_text_content_span(map),
                NameKind::Component,
                reporter,
            )
        });

        let package = declaration.take_optional("package").and_then_map(|name| {
            parse_name(
                name.with_text_content_span(map),
                NameKind::Package,
                reporter,
            )
        });

        let provider = declaration
            .take_optional::<String>("provider")
            .and_then_map(|name| {
                let Spanned!(span, name) = name.with_text_content_span(map);
                DependencyProvider::from_str(&name)
                    .map(|name| Spanned::new(span, name))
                    .map_err(|_| {
                        // @Task code
                        // @Task don't list all providers unconditionally:
                        //       if the invalid provider is_similar to a valid one, give a more
                        //       fine-tuned suggestion
                        Diagnostic::error()
                            .message(format!("‘{name}’ is not a valid dependency provider"))
                            .primary_span(span)
                            .note(format!(
                                "valid dependency providers are {}",
                                DependencyProvider::elements()
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
            exonym, component_endonym, package, provider, version, path, span;
            health.taint();
            continue
        };

        dependencies.insert(
            exonym,
            Spanned::new(
                span,
                DependencyDeclaration {
                    component: component_endonym,
                    package,
                    provider,
                    version: version.map(|version| version.map(VersionRequirement)),
                    path: path.map(|path| path.map(Into::into).with_text_content_span(map)),
                },
            ),
        );
    }

    Result::ok_if_untainted(Spanned::new(span, dependencies), health)
}

pub(super) struct PackageProfile {
    pub(super) name: Spanned<Word>,
    pub(super) version: Spanned<Version>,
    pub(super) description: Option<Spanned<String>>,
}

pub(super) type Components = HashMap<ComponentKey, Spanned<ComponentManifest>>;

#[derive(PartialEq, Eq, Hash, Debug)]
pub(super) struct ComponentKey {
    /// The name of the component.
    ///
    /// Never directly equal to the package name. Instead, it is `None`.
    pub(super) name: Option<WeaklySpanned<Word>>,
    pub(super) type_: WeaklySpanned<ComponentType>,
}

pub(super) struct ComponentManifest {
    #[allow(dead_code)] // @Temporary
    pub(super) public: Option<Spanned<bool>>,
    pub(super) path: Spanned<PathBuf>,
    pub(super) dependencies:
        Option<Spanned<HashMap<WeaklySpanned<Word>, Spanned<DependencyDeclaration>>>>,
}

#[derive(Clone, Debug)]
pub(super) struct DependencyDeclaration {
    pub(super) component: Option<Spanned<Word>>,
    #[allow(dead_code)] // @Temporary
    pub(super) package: Option<Spanned<Word>>,
    pub(super) provider: Option<Spanned<DependencyProvider>>,
    #[allow(dead_code)]
    pub(super) version: Option<Spanned<VersionRequirement>>,
    pub(super) path: Option<Spanned<PathBuf>>,
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

#[derive(Debug)]
pub(super) struct Version(pub(crate) String);

#[derive(Clone, Debug)]
pub(super) struct VersionRequirement(pub(crate) String);
