//! The LSP language server.

use crate::{
    component::{Component, ComponentType, Components},
    diagnostics::{reporter::Buffer, Diagnostic, ErrorCode, Reporter},
    error::Result,
    package::resolve_file,
    resolver::{self, PROGRAM_ENTRY_IDENTIFIER},
    session::BuildSession,
    span::SourceMap,
    syntax::{lexer, lowerer, parser},
    typer,
    utility::IOError,
};
use std::{
    default::default,
    mem,
    path::Path,
    sync::{Arc, RwLock},
};
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, InitializeParams, InitializeResult, InitializedParams,
        MessageType, ServerCapabilities, ServerInfo, TextDocumentSyncKind, TextDocumentSyncOptions,
    },
    Client,
};

const NAME: &str = "Lushui Language Server";

pub async fn serve(map: Arc<RwLock<SourceMap>>, reporter: Reporter) {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::new(|client| Server {
        map,
        reporter,
        client,
    });
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}

struct Server {
    map: Arc<RwLock<SourceMap>>,
    #[allow(dead_code)] // @Temporary
    reporter: Reporter,
    client: Client,
}

// @Task replace the async tower_lsp library with a sync server. async is not worth it
#[tower_lsp::async_trait]
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
        self.client
            .log_message(MessageType::INFO, "Language server initialized")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_change(&self, mut parameters: DidChangeTextDocumentParams) {
        let diagnostics = Buffer::default();

        let scheme = parameters.text_document.uri.scheme();
        if scheme != "file" {
            self.client
                .log_message(
                    MessageType::ERROR,
                    format!("Unsupported URI scheme ‘{scheme}’"),
                )
                .await;
            return;
        }

        let path = parameters.text_document.uri.path();

        // @Bug incredibly fragile!
        let content = parameters.content_changes.swap_remove(0).text;

        let _ = check_file(
            Path::new(path),
            content,
            &self.map,
            Reporter::buffer(diagnostics.clone()),
        );

        let diagnostics = mem::take(&mut *diagnostics.lock().unwrap());
        let diagnostics = diagnostics
            .into_iter()
            .map(|diagnostic| diagnostic.into_lsp_diagnostic(&self.map.read().unwrap()))
            .collect();

        // @Bug incorrect URI + version in general!
        self.client
            .publish_diagnostics(
                parameters.text_document.uri,
                diagnostics,
                Some(parameters.text_document.version),
            )
            .await;
    }
}

// @Question this should optimally be async, right??
fn check_file(
    path: &Path,
    content: String,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result {
    let (components, session) = resolve_file(
        path,
        Some(content),
        ComponentType::Executable,
        false,
        map,
        reporter,
    )?;

    build_components(components, session)
}

fn build_components(components: Components, mut session: BuildSession) -> Result {
    for mut component in components.into_values() {
        build_component(&mut component, &mut session)?;
        session.add(component);
    }

    Ok(())
}

fn build_component(component: &mut Component, session: &mut BuildSession) -> Result {
    let content = component.metadata.content.take();
    let path = component.path();

    // @Beacon @Task this shouldm't need to be that ugly!!!
    let file = match content {
        Some(content) => session.map().add(Some(path.bare.to_owned()), content),
        None => {
            session.map().load(path.bare.to_owned()).map_err(|error| {
                // @Bug this is duplication with main.rs!!
                Diagnostic::error()
                    .message(format!(
                        "could not load the {} component ‘{}’ in package ‘{}’",
                        component.type_(),
                        component.name(),
                        component.package(session).name,
                    ))
                    .primary_span(path)
                    .note(IOError(error, path.bare).to_string())
                    .report(session.reporter())
            })?
        }
    };

    let tokens = lexer::lex(file, session)?.value;

    let component_root = parser::parse_root_module_file(&tokens, file, session)?;

    let component_root = lowerer::lower_file(
        component_root,
        lowerer::Options {
            internal_features_enabled: component.is_core_library(session),
            keep_documentation_comments: false,
        },
        session,
    )?;

    let component_root = resolver::resolve_declarations(component_root, component, session)?;

    typer::check(&component_root, component, session)?;

    // @Bug this is duplication with main.rs!!
    if component.is_executable() && component.entry.is_none() {
        return Err(Diagnostic::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{PROGRAM_ENTRY_IDENTIFIER}’ function its root module",
                component.name(),
            ))
            .primary_span(&session.shared_map()[file])
            .report(session.reporter()));
    }

    Ok(())
}
