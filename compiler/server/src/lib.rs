//! An LSP language server for the Lushui programming language.
#![feature(let_chains)]
#![allow(unused_crate_dependencies, dead_code, unused_imports, unused_variables)]

use self::diagnostics::DiagnosticExt;
use self::span::{FromPositionExt, ToLocationExt};
use ::diagnostics::{
    Diagnostic, ErrorCode, Reporter, UntaggedDiagnostic, error::Result, reporter::Buffer,
};
use ::span::{ByteIndex, SourceMap, Spanning};
use index_map::IndexMap;
use package::resolve_file;
use resolver::ProgramEntryExt;
use session::Context;
use session::{
    Session,
    component::Component,
    unit::{BuildUnit, ComponentType},
};
use std::{
    collections::BTreeSet,
    mem,
    path::Path,
    sync::{Arc, RwLock},
};
use tower_lsp::{
    Client, jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
        GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializeResult,
        InitializedParams, MessageType, OneOf, ServerCapabilities, ServerInfo,
        TextDocumentSyncKind, TextDocumentSyncOptions, Url,
    },
};
use utility::{ComponentIndex, FormatError, HashMap, PROGRAM_ENTRY, default, path::CanonicalPath};

mod diagnostics;
mod span;

const NAME: &str = "Lushui Language Server";

pub async fn serve(map: Arc<RwLock<SourceMap>>) {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::new(|client| Server::new(map, client));
    tower_lsp::Server::new(stdin, stdout, socket).serve(service).await;
}

struct Server {
    map: Arc<RwLock<SourceMap>>,
    // @Beacon @Task remove this, we don't need this!! we have a frckin SourceMap here!!!
    documents: RwLock<HashMap<Url, Arc<String>>>,
    client: Client,
}

impl Server {
    fn new(map: Arc<RwLock<SourceMap>>, client: Client) -> Self {
        Self { map, documents: default(), client }
    }

    // @Beacon @Temporary
    // @Bug we have to reset the source map for now even though that is utter wastage!!
    // validate_file / check_file etc cannot handle a pre-populated SourceMap (yet)
    //   (one where we dedup by abs file path)
    // @Note if we can somehow support a long-living source map, we might be able to
    // get rid of the document map???
    fn reset_source_map(&self) {
        *self.map.write().unwrap() = default();
    }

    async fn validate_file(&self, uri: Url, version: i32, content: Arc<String>) {
        let scheme = uri.scheme();
        if scheme != "file" {
            self.client
                .log_message(MessageType::ERROR, format!("Unsupported URI scheme ‘{scheme}’"))
                .await;
            return;
        }

        let path = Path::new(uri.path());
        let diagnostics = Buffer::default();

        self.reset_source_map();

        _ = check_file(path, content, &self.map, Reporter::buffer(diagnostics.clone()));

        let diagnostics = mem::take(&mut *diagnostics.lock().unwrap());
        self.report(diagnostics, uri, version).await;
    }

    async fn report(&self, diagnostics: BTreeSet<UntaggedDiagnostic>, uri: Url, version: i32) {
        let mut lsp_diagnostics = Vec::new();

        for diagnostic in diagnostics {
            let diagnostic = diagnostic.into_lsp_type(&self.map.read().unwrap());
            match diagnostic {
                OneOf::Left(diagnostic) => lsp_diagnostics.push(diagnostic),
                OneOf::Right((type_, message)) => self.client.show_message(type_, message).await,
            }
        }

        // @Bug incorrect URI + version in general!
        self.client.publish_diagnostics(uri, lsp_diagnostics, Some(version)).await;
    }
}

// @Task replace the async tower_lsp library with a sync server. async is not worth it
#[tower_lsp::async_trait]
#[allow(clippy::ignored_unit_patterns)] // false positive
impl tower_lsp::LanguageServer for Server {
    async fn initialize(&self, _: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        ..default()
                    }
                    .into(),
                ),
                definition_provider: Some(OneOf::Left(true)),
                ..default()
            },
            server_info: Some(ServerInfo {
                name: NAME.into(),
                // @Task supply version here (do what main.rs does)
                version: None,
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client.log_message(MessageType::INFO, "Language server initialized").await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, parameters: DidOpenTextDocumentParams) {
        let content = Arc::new(parameters.text_document.text);

        self.documents
            .write()
            .unwrap()
            .insert(parameters.text_document.uri.clone(), content.clone());

        // @Question version??
        self.validate_file(parameters.text_document.uri.clone(), 0, content).await;
    }

    async fn did_change(&self, mut parameters: DidChangeTextDocumentParams) {
        // @Bug incredibly fragile!
        let content = Arc::new(parameters.content_changes.swap_remove(0).text);

        self.documents
            .write()
            .unwrap()
            .insert(parameters.text_document.uri.clone(), content.clone());

        self.validate_file(
            parameters.text_document.uri.clone(),
            parameters.text_document.version,
            content,
        )
        .await;
    }

    async fn did_close(&self, parameters: DidCloseTextDocumentParams) {
        // @Task unless it's a workspace(?) (a package), get rid of any diagnostics
        self.client.publish_diagnostics(parameters.text_document.uri, Vec::new(), None).await;
    }

    async fn goto_definition(
        &self,
        parameters: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        Ok((|| {
            let uri = parameters.text_document_position_params.text_document.uri;

            let path = CanonicalPath::new_unchecked(uri.path());
            // @Task error on unsupported URI scheme!
            let content = self.documents.read().unwrap().get(&uri).unwrap().clone();

            self.reset_source_map();

            // @Task keep going even with errors!
            let (context, component_roots) =
                check_file(path, content, &self.map, Reporter::silent()).ok()?;

            let position = parameters.text_document_position_params.position;
            let byte_index = ByteIndex::from_position(position, path, &self.map.read().unwrap());

            // @Task just get the SourceFileIndex with the URL from the SourceMap in the future
            let component =
                self.map.read().unwrap().file_by_path(path).unwrap().component().unwrap();
            let component_root = &component_roots[&component];

            let binding = component_root.find_binding(byte_index);
            let binding = binding?;

            let hir::Index::Declaration(index) = binding.index else {
                // @Task handle parameter & pattern bindings
                return None;
            };

            Some(GotoDefinitionResponse::Scalar(
                context[index].source.span().to_location(&self.map.read().unwrap()),
            ))
        })())
    }
}

// @Beacon @Task instead of a path and the content, take a SourceFileIndex!!!
// @Task keep going even with errors!
fn check_file(
    path: &Path,
    content: Arc<String>,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result<(Context, HashMap<ComponentIndex, hir::Declaration>)> {
    let (units, mut context) =
        resolve_file(path, Some(content), ComponentType::Executable, false, map, reporter)?;

    let mut component_roots = HashMap::default();

    for unit in units.into_values() {
        let index = unit.index;
        let root = context.at(unit, build_unit)?;
        component_roots.insert(index, root);
    }

    Ok((context, component_roots))
}

// @Task keep going even with errors!
#[allow(clippy::needless_pass_by_value)] // by design
fn build_unit(unit: BuildUnit, session: &mut Session<'_>) -> Result<hir::Declaration> {
    // let content = component.content.take();
    // @Beacon @Beacon @Beacon @Temporary
    let content = Some(std::sync::Arc::new(String::new()));
    let path = unit.path.as_deref();

    // @Beacon @Task this shouldm't need to be that ugly!!!

    let file = match content {
        Some(content) => session.map().add(path.bare.to_owned(), content, Some(unit.index)),
        None => {
            session.map().load(path.bare, Some(unit.index)).map_err(|error| {
                use std::fmt::Write;

                let mut message =
                    format!("could not load the {} component ‘{}’", unit.type_, unit.name);

                if let Some(package) = session.package() {
                    write!(message, " in package ‘{}’", session[package].name).unwrap();
                }

                // @Bug this is duplication with main.rs!!
                Diagnostic::error()
                    .message(message)
                    .path(path.bare.to_path_buf())
                    .unlabeled_span(path)
                    .note(error.format())
                    .report(session.reporter())
            })?
        }
    };

    let tokens = syntax::lex(file, session);
    let component_root = syntax::parse_root_module_file(tokens, file, session)?;

    let component_root = lowerer::lower_file(
        component_root,
        lowerer::Options {
            internal_features_enabled: unit.is_core_library(session),
            keep_documentation_comments: false,
        },
        session,
    )?;

    let component_root = resolver::resolve_declarations(component_root, session)?;

    typer::check(&component_root, session)?;

    // @Bug this is duplication with main.rs!!
    if unit.type_ == ComponentType::Executable && session.look_up_program_entry().is_none() {
        return Err(Diagnostic::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{PROGRAM_ENTRY}’ function in its root module",
                unit.name,
            ))
            .unlabeled_span(&session.shared_map()[file])
            .report(session.reporter()));
    }

    Ok(component_root)
}

trait FindBinding {
    fn find_binding(&self, byte_index: ByteIndex) -> Option<hir::Identifier>;
}

#[allow(clippy::match_same_arms)] // @Temporary
impl FindBinding for hir::Declaration {
    fn find_binding(&self, byte_index: ByteIndex) -> Option<hir::Identifier> {
        use hir::BareDeclaration::*;

        // @Question do we need this check?
        if !self.span.contains(byte_index) {
            return None;
        }

        match &self.bare {
            Function(function) => {
                if function.type_.span.contains(byte_index) {
                    function.type_.find_binding(byte_index)
                } else if let Some(expression) = &function.body
                    && expression.span.contains(byte_index)
                {
                    expression.find_binding(byte_index)
                } else {
                    None
                }
            }
            Data(type_) => {
                if type_.type_.span.contains(byte_index) {
                    type_.type_.find_binding(byte_index)
                } else if let Some(constructors) = &type_.constructors {
                    let index = constructors
                        .binary_search_by(|constructor| {
                            byte_index.relate(constructor.span).reverse()
                        })
                        .ok()?;
                    constructors[index].find_binding(byte_index)
                } else {
                    None
                }
            }
            Constructor(constructor) => {
                if constructor.type_.span.contains(byte_index) {
                    constructor.type_.find_binding(byte_index)
                } else {
                    None
                }
            }
            Module(module) => {
                let index = module
                    .declarations
                    .binary_search_by(|declaration| byte_index.relate(declaration.span).reverse())
                    .ok()?;
                module.declarations[index].find_binding(byte_index)
            }
            Use(use_) => {
                // @Question I wonder if that actually works ^^
                // @Note I assume this won't work if we click on path segments that aren't the last segment
                if use_.target.span().contains(byte_index) {
                    Some(use_.target)
                } else if let Some(binder) = use_.binder
                    && binder.span().contains(byte_index)
                {
                    Some(binder)
                } else {
                    None
                }
            }
            Error(_) => None, // @Task
        }
    }
}

#[allow(clippy::match_same_arms)] // @Temporary
impl FindBinding for hir::Expression {
    // @Task don't use contains but a function that returns an Ordering!! so we can
    // know if we should jump to the next thingy
    fn find_binding(&self, byte_index: ByteIndex) -> Option<hir::Identifier> {
        use hir::BareExpression::*;

        // @Question do we need this check?
        if !self.span.contains(byte_index) {
            return None;
        }

        match &self.bare {
            PiType(pi) => {
                if pi.domain.span.contains(byte_index) {
                    pi.domain.find_binding(byte_index)
                } else if pi.codomain.span.contains(byte_index) {
                    pi.codomain.find_binding(byte_index)
                } else {
                    None
                }
            }
            Application(application) => {
                if application.callee.span.contains(byte_index) {
                    application.callee.find_binding(byte_index)
                } else if application.argument.span.contains(byte_index) {
                    application.argument.find_binding(byte_index)
                } else {
                    None
                }
            }
            Number(_) => None, // @Task
            Text(_) => None,   // @Task
            Binding(binding) => {
                if binding.0.span().contains(byte_index) {
                    Some(binding.0)
                } else {
                    None
                }
            }
            Lambda(lambda) => {
                if let Some(type_annotation) = &lambda.domain
                    && type_annotation.span.contains(byte_index)
                {
                    type_annotation.find_binding(byte_index)
                } else if let Some(type_annotation) = &lambda.codomain
                    && type_annotation.span.contains(byte_index)
                {
                    type_annotation.find_binding(byte_index)
                } else if lambda.body.span.contains(byte_index) {
                    lambda.body.find_binding(byte_index)
                } else {
                    None
                }
            }
            CaseAnalysis(analysis) => {
                if analysis.scrutinee.span.contains(byte_index) {
                    analysis.scrutinee.find_binding(byte_index)
                } else {
                    // @Task search patterns and match arms
                    None
                }
            }
            Substituted(_) => None,          // @Task
            IntrinsicApplication(_) => None, // @Task
            Projection(_) => None,           // @Task
            Record(_) => None,               // @Task
            IO(_) => None,                   // @Task
            Error(_) => None,                // @Task
        }
    }
}
