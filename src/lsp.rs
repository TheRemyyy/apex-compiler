//! LSP (Language Server Protocol) implementation for Apex
//!
//! Provides IDE features like:
//! - Autocompletion
//! - Hover information
//! - Go to definition
//! - Diagnostics

use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::ast::{Decl, Program};
use crate::lexer;
use crate::parser::{ParseError, Parser};

/// Document state tracked by the LSP server
#[derive(Debug, Clone)]
struct Document {
    text: String,
    version: i32,
    parsed: Option<Program>,
}

/// LSP Server backend
pub struct Backend {
    client: Client,
    documents: Arc<RwLock<HashMap<Url, Document>>>,
}

impl Backend {
    fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Parse a document and store the AST
    async fn parse_document(&self, uri: &Url) {
        let (text, version) = {
            let docs = self.documents.read().await;
            if let Some(doc) = docs.get(uri) {
                (doc.text.clone(), doc.version)
            } else {
                return;
            }
        };

        let mut diagnostics = Vec::new();
        let parsed = match lexer::tokenize(&text) {
            Ok(tokens) => {
                let mut parser = Parser::new(tokens);
                match parser.parse_program() {
                    Ok(program) => Some(program),
                    Err(err) => {
                        diagnostics.push(self.parse_error_to_diagnostic(&text, &err));
                        None
                    }
                }
            }
            Err(msg) => {
                diagnostics.push(self.lexer_error_to_diagnostic(&text, &msg));
                None
            }
        };

        {
            let mut docs = self.documents.write().await;
            if let Some(doc) = docs.get_mut(uri) {
                doc.parsed = parsed;
            }
        }

        self.client
            .publish_diagnostics(uri.clone(), diagnostics, Some(version))
            .await;
    }

    fn parse_error_to_diagnostic(&self, text: &str, err: &ParseError) -> Diagnostic {
        Diagnostic {
            range: self.span_to_range(text, err.span.clone()),
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("apex-parser".to_string()),
            message: err.message.clone(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn lexer_error_to_diagnostic(&self, text: &str, msg: &str) -> Diagnostic {
        // Expected shape: "Unknown token at <offset>: '<snippet>'"
        let offset = msg
            .split("Unknown token at ")
            .nth(1)
            .and_then(|s| s.split(':').next())
            .and_then(|s| s.trim().parse::<usize>().ok())
            .unwrap_or(0);
        Diagnostic {
            range: self.span_to_range(text, offset..(offset + 1)),
            severity: Some(DiagnosticSeverity::ERROR),
            code: None,
            code_description: None,
            source: Some("apex-lexer".to_string()),
            message: msg.to_string(),
            related_information: None,
            tags: None,
            data: None,
        }
    }

    fn span_to_range(&self, text: &str, span: std::ops::Range<usize>) -> Range {
        Range {
            start: self.offset_to_position(text, span.start),
            end: self.offset_to_position(text, span.end),
        }
    }

    fn offset_to_position(&self, text: &str, target: usize) -> Position {
        let mut line = 0u32;
        let mut col = 0u32;
        let mut last_idx = 0usize;

        for (idx, ch) in text.char_indices() {
            if idx >= target {
                break;
            }
            last_idx = idx;
            if ch == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        if target > last_idx && target <= text.len() {
            // No-op: col is already best-effort for UTF-8 char boundaries.
        }

        Position::new(line, col)
    }

    fn word_at_position(&self, text: &str, pos: Position) -> Option<String> {
        let line = text.lines().nth(pos.line as usize)?;
        let chars: Vec<char> = line.chars().collect();
        if chars.is_empty() {
            return None;
        }
        let mut idx = (pos.character as usize).min(chars.len().saturating_sub(1));
        if !chars[idx].is_alphanumeric() && chars[idx] != '_' && idx > 0 {
            idx -= 1;
        }
        if !chars[idx].is_alphanumeric() && chars[idx] != '_' {
            return None;
        }

        let mut start = idx;
        while start > 0 && (chars[start - 1].is_alphanumeric() || chars[start - 1] == '_') {
            start -= 1;
        }
        let mut end = idx;
        while end + 1 < chars.len() && (chars[end + 1].is_alphanumeric() || chars[end + 1] == '_') {
            end += 1;
        }
        Some(chars[start..=end].iter().collect())
    }

    fn definition_locations(
        &self,
        uri: &Url,
        text: &str,
        program: &Program,
        symbol: &str,
    ) -> Vec<Location> {
        let mut out = Vec::new();
        for decl in &program.declarations {
            match &decl.node {
                Decl::Function(func) if func.name == symbol => {
                    out.push(Location::new(
                        uri.clone(),
                        self.span_to_range(text, decl.span.clone()),
                    ));
                }
                Decl::Class(class) if class.name == symbol => {
                    out.push(Location::new(
                        uri.clone(),
                        self.span_to_range(text, decl.span.clone()),
                    ));
                }
                Decl::Enum(en) if en.name == symbol => {
                    out.push(Location::new(
                        uri.clone(),
                        self.span_to_range(text, decl.span.clone()),
                    ));
                }
                Decl::Interface(inter) if inter.name == symbol => {
                    out.push(Location::new(
                        uri.clone(),
                        self.span_to_range(text, decl.span.clone()),
                    ));
                }
                Decl::Module(module) => {
                    if module.name == symbol {
                        out.push(Location::new(
                            uri.clone(),
                            self.span_to_range(text, decl.span.clone()),
                        ));
                    }
                    for inner in &module.declarations {
                        if let Decl::Function(func) = &inner.node {
                            if func.name == symbol {
                                out.push(Location::new(
                                    uri.clone(),
                                    self.span_to_range(text, inner.span.clone()),
                                ));
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        out
    }

    /// Get completion items for a position
    fn get_completions(&self, doc: &Document, _pos: Position) -> Vec<CompletionItem> {
        let mut items = Vec::new();

        // Keywords
        let keywords = vec![
            "function",
            "class",
            "interface",
            "enum",
            "module",
            "if",
            "else",
            "while",
            "for",
            "in",
            "return",
            "break",
            "continue",
            "match",
            "mut",
            "let",
            "import",
            "package",
            "async",
            "await",
            "public",
            "private",
            "protected",
            "constructor",
            "destructor",
        ];

        for kw in keywords {
            items.push(CompletionItem {
                label: kw.to_string(),
                kind: Some(CompletionItemKind::KEYWORD),
                detail: Some("keyword".to_string()),
                ..Default::default()
            });
        }

        // Types
        let types = vec![
            "Integer", "Float", "Boolean", "String", "Char", "None", "Option", "Result", "List",
            "Map", "Set", "Box", "Rc", "Arc", "Task",
        ];

        for ty in types {
            items.push(CompletionItem {
                label: ty.to_string(),
                kind: Some(CompletionItemKind::TYPE_PARAMETER),
                detail: Some("type".to_string()),
                ..Default::default()
            });
        }

        // Functions from AST
        if let Some(program) = &doc.parsed {
            for decl in &program.declarations {
                if let Decl::Function(func) = &decl.node {
                    items.push(CompletionItem {
                        label: func.name.clone(),
                        kind: Some(CompletionItemKind::FUNCTION),
                        detail: Some(format!("function: {}", func.name)),
                        ..Default::default()
                    });
                }
            }
        }

        items
    }

    /// Get hover information
    fn get_hover(&self, doc: &Document, pos: Position) -> Option<Hover> {
        // Simple word-based hover
        let line = doc.text.lines().nth(pos.line as usize)?;

        // Keywords documentation
        let keywords_docs: HashMap<&str, &str> = [
            ("function", "Define a function\n\n```apex\nfunction name(params): ReturnType {\n  // body\n}\n```"),
            ("class", "Define a class\n\n```apex\nclass Name {\n  field: Type;\n  function method(): Type { }\n}\n```"),
            ("if", "Conditional statement\n\n```apex\nif (condition) {\n  // then branch\n} else {\n  // else branch\n}\n```"),
            ("while", "While loop\n\n```apex\nwhile (condition) {\n  // body\n}\n```"),
            ("for", "For loop\n\n```apex\nfor (i in 0..10) {\n  // body\n}\n```"),
            ("match", "Pattern matching\n\n```apex\nmatch value {\n  Pattern => { },\n  _ => { },\n}\n```"),
            ("mut", "Mutable variable declaration\n\n```apex\nmut x: Integer = 10;\n```"),
            ("let", "Variable declaration\n\n```apex\nlet x: Integer = 10;\n```"),
            ("import", "Import from another module\n\n```apex\nimport utils.math.*;\n```"),
            ("package", "Declare package namespace\n\n```apex\npackage my.module;\n```"),
            ("async", "Async function or block\n\n```apex\nasync function foo(): Task<String> { }\n```"),
            ("await", "Await an async operation\n\n```apex\nlet result = await asyncFunction();\n```"),
            ("return", "Return from function\n\n```apex\nreturn value;\n```"),
        ].iter().cloned().collect();

        for (kw, doc) in keywords_docs {
            if line.contains(kw) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: doc.to_string(),
                    }),
                    range: None,
                });
            }
        }

        None
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "apex-lsp".to_string(),
                version: Some("1.3.1".to_string()),
            }),
            offset_encoding: None,
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    resolve_provider: Some(false),
                    trigger_characters: Some(vec![".".to_string(), "(".to_string()]),
                    work_done_progress_options: Default::default(),
                    all_commit_characters: None,
                    completion_item: None,
                }),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Apex LSP server initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        let version = params.text_document.version;

        let mut docs = self.documents.write().await;
        docs.insert(
            uri.clone(),
            Document {
                text: text.clone(),
                version,
                parsed: None,
            },
        );
        drop(docs);

        self.parse_document(&uri).await;

        self.client
            .log_message(MessageType::INFO, format!("Opened document: {}", uri))
            .await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;

        if let Some(change) = params.content_changes.into_iter().next() {
            let mut docs = self.documents.write().await;
            if let Some(doc) = docs.get_mut(&uri) {
                doc.text = change.text;
                doc.version = params.text_document.version;
            }
            drop(docs);

            self.parse_document(&uri).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.write().await;
        docs.remove(&params.text_document.uri);
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let pos = params.text_document_position.position;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(&uri) {
            let items = self.get_completions(doc, pos);
            return Ok(Some(CompletionResponse::Array(items)));
        }

        Ok(None)
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(&uri) {
            return Ok(self.get_hover(doc, pos));
        }

        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let pos = params.text_document_position_params.position;

        let docs = self.documents.read().await;
        if let Some(doc) = docs.get(&uri) {
            if let Some(program) = &doc.parsed {
                if let Some(symbol) = self.word_at_position(&doc.text, pos) {
                    let locations = self.definition_locations(&uri, &doc.text, program, &symbol);
                    if !locations.is_empty() {
                        return Ok(Some(GotoDefinitionResponse::Array(locations)));
                    }
                }
            }
        }

        Ok(None)
    }
}

/// Run the LSP server
pub async fn run_lsp_server() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(Backend::new);
    Server::new(stdin, stdout, socket).serve(service).await;
}
