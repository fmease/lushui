//! An LSP language server for the Lushui programming language.
#![feature(let_chains)]
#![allow(unused_crate_dependencies, dead_code, unused_imports, unused_variables)]

use self::diagnostics::DiagnosticExt;
use self::span::{FromPositionExt, ToLocationExt};
use ::diagnostics::{error::Result, reporter::Buffer, Diag, ErrorCode, Reporter, UntaggedDiag};
use ::span::{ByteIdx, SourceMap, Spanning};
use index_map::IndexMap;
use package::resolve_file;
use resolver::ProgramEntryExt;
use session::Context;
use session::{
    component::Comp,
    unit::{BuildUnit, CompTy},
    Session,
};
use std::{
    collections::BTreeSet,
    mem,
    path::Path,
    sync::{Arc, RwLock},
};
use tower_lsp::{
    jsonrpc,
    lsp_types::{
        DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
        GotoDefinitionParams, GotoDefinitionResponse, InitializeParams, InitializeResult,
        InitializedParams, MessageType, OneOf, ServerCapabilities, ServerInfo,
        TextDocumentSyncKind, TextDocumentSyncOptions, Url,
    },
    Client,
};
use utility::{default, path::CanonicalPath, CompIdx, FormatError, HashMap, PROGRAM_ENTRY};

mod diagnostics;
mod span;

const NAME: &str = "Lushui Language Server";

pub async fn serve(map: Arc<RwLock<SourceMap>>) {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = tower_lsp::LspService::new(|client| Server::new(map, client));
    tower_lsp::Server::new(stdin, stdout, socket)
        .serve(service)
        .await;
}

struct Server {
    map: Arc<RwLock<SourceMap>>,
    // @Beacon @Task remove this, we don't need this!! we have a frckin SourceMap here!!!
    documents: RwLock<HashMap<Url, Arc<String>>>,
    client: Client,
}

impl Server {
    fn new(map: Arc<RwLock<SourceMap>>, client: Client) -> Self {
        Self {
            map,
            documents: default(),
            client,
        }
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
                .log_message(
                    MessageType::ERROR,
                    format!("Unsupported URI scheme ‘{scheme}’"),
                )
                .await;
            return;
        }

        let path = Path::new(uri.path());
        let diags = Buffer::default();

        self.reset_source_map();

        _ = check_file(path, content, &self.map, Reporter::buffer(diags.clone()));

        let diags = mem::take(&mut *diags.lock().unwrap());
        self.report(diags, uri, version).await;
    }

    async fn report(&self, diags: BTreeSet<UntaggedDiag>, uri: Url, version: i32) {
        let mut lsp_diags = Vec::new();

        for diag in diags {
            let diagnostic = diag.into_lsp_ty(&self.map.read().unwrap());
            match diagnostic {
                OneOf::Left(diagnostic) => lsp_diags.push(diagnostic),
                OneOf::Right((ty, message)) => self.client.show_message(ty, message).await,
            }
        }

        // @Bug incorrect URI + version in general!
        self.client
            .publish_diagnostics(uri, lsp_diags, Some(version))
            .await;
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
        self.client
            .log_message(MessageType::INFO, "Language server initialized")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let content = Arc::new(params.text_document.text);

        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri.clone(), content.clone());

        // @Question version??
        self.validate_file(params.text_document.uri.clone(), 0, content)
            .await;
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        // @Bug incredibly fragile!
        let content = Arc::new(params.content_changes.swap_remove(0).text);

        self.documents
            .write()
            .unwrap()
            .insert(params.text_document.uri.clone(), content.clone());

        self.validate_file(
            params.text_document.uri.clone(),
            params.text_document.version,
            content,
        )
        .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        // @Task unless it's a workspace(?) (a package), get rid of any diagnostics
        self.client
            .publish_diagnostics(params.text_document.uri, Vec::new(), None)
            .await;
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        Ok((|| {
            let uri = params.text_document_position_params.text_document.uri;

            let path = CanonicalPath::new_unchecked(uri.path());
            // @Task error on unsupported URI scheme!
            let content = self.documents.read().unwrap().get(&uri).unwrap().clone();

            self.reset_source_map();

            // @Task keep going even with errors!
            let (cx, component_roots) =
                check_file(path, content, &self.map, Reporter::silent()).ok()?;

            let position = params.text_document_position_params.position;
            let byte_index = ByteIdx::from_position(position, path, &self.map.read().unwrap());

            // @Task just get the SrcFileIdx with the URL from the SourceMap in the future
            let comp = self
                .map
                .read()
                .unwrap()
                .file_by_path(path)
                .unwrap()
                .component()
                .unwrap();
            let comp_root = &component_roots[&comp];

            let binding = comp_root.find_binding(byte_index);
            let binding = binding?;

            let hir::Index::Decl(idx) = binding.idx else {
                // @Task handle parameter & pattern bindings
                return None;
            };

            Some(GotoDefinitionResponse::Scalar(
                cx[idx].src.span().to_location(&self.map.read().unwrap()),
            ))
        })())
    }
}

// @Beacon @Task instead of a path and the content, take a SrcFileIdx!!!
// @Task keep going even with errors!
fn check_file(
    path: &Path,
    content: Arc<String>,
    map: &Arc<RwLock<SourceMap>>,
    rep: Reporter,
) -> Result<(Context, HashMap<CompIdx, hir::Decl>)> {
    let (units, mut cx) = resolve_file(path, Some(content), CompTy::Executable, false, map, rep)?;

    let mut comp_roots = HashMap::default();

    for unit in units.into_values() {
        let index = unit.index;
        let root = cx.at(unit, build_unit)?;
        comp_roots.insert(index, root);
    }

    Ok((cx, comp_roots))
}

// @Task keep going even with errors!
#[allow(clippy::needless_pass_by_value)] // by design
fn build_unit(unit: BuildUnit, sess: &mut Session<'_>) -> Result<hir::Decl> {
    // let content = component.content.take();
    // @Beacon @Beacon @Beacon @Temporary
    let content = Some(std::sync::Arc::new(String::new()));
    let path = unit.path.as_deref();

    // @Beacon @Task this shouldm't need to be that ugly!!!

    let file = match content {
        Some(content) => sess
            .map()
            .add(path.bare.to_owned(), content, Some(unit.index)),
        None => {
            sess.map()
                .load(path.bare, Some(unit.index))
                .map_err(|error| {
                    use std::fmt::Write;

                    let mut message =
                        format!("could not load the {} component ‘{}’", unit.ty, unit.name);

                    if let Some(package) = sess.pkg() {
                        write!(message, " in package ‘{}’", sess[package].name).unwrap();
                    }

                    // @Bug this is duplication with main.rs!!
                    Diag::error()
                        .message(message)
                        .path(path.bare.into())
                        .unlabeled_span(path)
                        .note(error.format())
                        .report(sess.rep())
                })?
        }
    };

    let tokens = syntax::lex(file, sess);
    let comp_root = syntax::parse_root_module_file(tokens, file, sess)?;

    let comp_root = lowerer::lower_file(
        comp_root,
        lowerer::Options {
            internal_features_enabled: unit.is_core_lib(sess),
            keep_doc_comments: false,
        },
        sess,
    )?;

    let comp_root = resolver::resolve_decls(comp_root, sess)?;

    typer::check(&comp_root, sess)?;

    // @Bug this is duplication with main.rs!!
    if unit.ty == CompTy::Executable && sess.look_up_program_entry().is_none() {
        return Err(Diag::error()
            .code(ErrorCode::E050)
            .message(format!(
                "the component ‘{}’ does not contain a ‘{PROGRAM_ENTRY}’ function in its root module",
                unit.name,
            ))
            .unlabeled_span(&sess.shared_map()[file])
            .report(sess.rep()));
    }

    Ok(comp_root)
}

trait FindBinding {
    fn find_binding(&self, byte_index: ByteIdx) -> Option<hir::Ident>;
}

#[allow(clippy::match_same_arms)] // @Temporary
impl FindBinding for hir::Decl {
    fn find_binding(&self, byte_idx: ByteIdx) -> Option<hir::Ident> {
        use hir::BareDecl::*;

        // @Question do we need this check?
        if !self.span.contains(byte_idx) {
            return None;
        }

        match &self.bare {
            Func(func) => {
                if func.ty.span.contains(byte_idx) {
                    func.ty.find_binding(byte_idx)
                } else if let Some(expression) = &func.body
                    && expression.span.contains(byte_idx)
                {
                    expression.find_binding(byte_idx)
                } else {
                    None
                }
            }
            DataTy(ty) => {
                if ty.ty.span.contains(byte_idx) {
                    ty.ty.find_binding(byte_idx)
                } else if let Some(constructors) = &ty.ctors {
                    let index = constructors
                        .binary_search_by(|ctor| byte_idx.relate(ctor.span).reverse())
                        .ok()?;
                    constructors[index].find_binding(byte_idx)
                } else {
                    None
                }
            }
            Ctor(ctor) => {
                if ctor.ty.span.contains(byte_idx) {
                    ctor.ty.find_binding(byte_idx)
                } else {
                    None
                }
            }
            Module(module) => {
                let index = module
                    .decls
                    .binary_search_by(|decl| byte_idx.relate(decl.span).reverse())
                    .ok()?;
                module.decls[index].find_binding(byte_idx)
            }
            Use(use_) => {
                // @Question I wonder if that actually works ^^
                // @Note I assume this won't work if we click on path segments that aren't the last segment
                if use_.target.span().contains(byte_idx) {
                    Some(use_.target)
                } else if let Some(binder) = use_.binder
                    && binder.span().contains(byte_idx)
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
impl FindBinding for hir::Expr {
    // @Task don't use contains but a function that returns an Ordering!! so we can
    // know if we should jump to the next thingy
    fn find_binding(&self, byte_index: ByteIdx) -> Option<hir::Ident> {
        use hir::BareExpr::*;

        // @Question do we need this check?
        if !self.span.contains(byte_index) {
            return None;
        }

        match &self.bare {
            PiTy(pi_ty) => {
                if pi_ty.domain.span.contains(byte_index) {
                    pi_ty.domain.find_binding(byte_index)
                } else if pi_ty.codomain.span.contains(byte_index) {
                    pi_ty.codomain.find_binding(byte_index)
                } else {
                    None
                }
            }
            App(app) => {
                if app.callee.span.contains(byte_index) {
                    app.callee.find_binding(byte_index)
                } else if app.arg.span.contains(byte_index) {
                    app.arg.find_binding(byte_index)
                } else {
                    None
                }
            }
            NumLit(_) => None,  // @Task
            TextLit(_) => None, // @Task
            Binding(binding) => {
                if binding.0.span().contains(byte_index) {
                    Some(binding.0)
                } else {
                    None
                }
            }
            LamLit(lambda) => {
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
            Substed(_) => None, // @Task
            IntrApp(_) => None, // @Task
            Proj(_) => None,    // @Task
            RecLit(_) => None,  // @Task
            IO(_) => None,      // @Task
            Error(_) => None,   // @Task
        }
    }
}
