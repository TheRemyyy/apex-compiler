//! Import checker - verifies that all used functions are imported

use crate::ast::*;
use crate::stdlib::StdLib;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Error when using function without importing it
#[derive(Debug, Clone)]
pub struct ImportError {
    pub function_name: String,
    pub defined_in: String,
    pub used_in: String,
    #[allow(dead_code)]
    pub span: Span,
    pub suggestion: Option<String>,
}

impl ImportError {
    pub fn format(&self) -> String {
        let mut result = format!(
            "Function '{}' is defined in '{}' but not imported in '{}'\n  \
             Hint: Add 'import {}.{};' to the top of your file",
            self.function_name, self.defined_in, self.used_in, self.defined_in, self.function_name
        );

        if let Some(suggestion) = &self.suggestion {
            result.push_str(&format!("\n  Or did you mean: '{}'?", suggestion));
        }

        result
    }
}

/// Calculate Levenshtein distance between two strings
#[allow(clippy::needless_range_loop)]
fn levenshtein_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let len_a = a_chars.len();
    let len_b = b_chars.len();

    if len_a == 0 {
        return len_b;
    }
    if len_b == 0 {
        return len_a;
    }

    let mut prev: Vec<usize> = (0..=len_b).collect();
    let mut curr: Vec<usize> = vec![0; len_b + 1];

    for (i, ca) in a_chars.iter().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b_chars.iter().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        std::mem::swap(&mut prev, &mut curr);
    }

    prev[len_b]
}

/// Find the closest matching string from candidates
fn did_you_mean(name: &str, candidates: &[String]) -> Option<String> {
    let mut best_match: Option<(String, usize)> = None;

    for candidate in candidates {
        let distance = levenshtein_distance(name, candidate);
        // Only suggest if distance is reasonable (<= 3 and less than half the length)
        let threshold = (name.len() / 2).max(3);
        if distance <= threshold {
            if let Some((_, best_distance)) = &best_match {
                if distance < *best_distance {
                    best_match = Some((candidate.clone(), distance));
                }
            } else {
                best_match = Some((candidate.clone(), distance));
            }
        }
    }

    best_match.map(|(s, _)| s)
}

/// Tracks which functions are defined in which files/namespaces
pub struct ImportChecker<'a> {
    /// function_name -> namespace (e.g., "factorial" -> "utils.math")
    function_namespaces: Arc<HashMap<String, String>>,
    /// Current file namespace
    current_namespace: String,
    /// Imported functions in current file (just the name, e.g., "factorial")
    imported_functions: HashSet<String>,
    /// All imports (for wildcard resolution)
    #[allow(dead_code)]
    wildcard_imports: Vec<String>, // e.g., ["utils.math", "utils.strings"]
    /// Standard library registry
    stdlib: &'a StdLib,
    /// Available function names for suggestions
    available_functions: Vec<String>,
    /// Collected errors
    errors: Vec<ImportError>,
}

impl<'a> ImportChecker<'a> {
    pub fn new(
        function_namespaces: Arc<HashMap<String, String>>,
        current_namespace: String,
        imports: Vec<ImportDecl>,
        stdlib: &'a StdLib,
    ) -> Self {
        let mut imported_functions = HashSet::new();
        let mut wildcard_imports = Vec::new();

        for import in imports {
            let path = import.path;

            if path.ends_with(".*") {
                // Wildcard import: utils.math.*
                let ns = path.trim_end_matches(".*");
                wildcard_imports.push(ns.to_string());

                // Add all functions from this namespace (user-defined)
                for (func, func_ns) in function_namespaces.iter() {
                    if func_ns == ns {
                        imported_functions.insert(func.clone());
                    }
                }

                // Add all stdlib functions from this namespace
                for (func, func_ns) in stdlib.get_functions() {
                    if func_ns == ns {
                        imported_functions.insert(func.clone());
                    }
                }
            } else if path.contains('.') {
                // Specific import: utils.math.factorial
                let parts: Vec<&str> = path.split('.').collect();
                if let Some(func_name) = parts.last() {
                    imported_functions.insert(func_name.to_string());
                }
            }
        }

        // Collect available function names for suggestions
        let mut available_functions: Vec<String> = function_namespaces.keys().cloned().collect();
        available_functions.extend(stdlib.get_functions().keys().cloned());

        Self {
            function_namespaces,
            current_namespace,
            imported_functions,
            wildcard_imports,
            stdlib,
            available_functions,
            errors: Vec::new(),
        }
    }

    /// Check if a function call is valid (imported or local)
    pub fn check_function_call(&mut self, name: &str, span: Span) {
        // Skip if it's a local function (defined in current file)
        if let Some(ns) = self.function_namespaces.get(name) {
            if ns == &self.current_namespace {
                // Local function - OK
                return;
            }

            // Check if imported
            if !self.imported_functions.contains(name) {
                // Try to find a similar function name
                let suggestion = did_you_mean(name, &self.available_functions);

                self.errors.push(ImportError {
                    function_name: name.to_string(),
                    defined_in: ns.clone(),
                    used_in: self.current_namespace.clone(),
                    span,
                    suggestion,
                });
            }
            return;
        }

        // Check if it's a stdlib function that needs to be imported
        if let Some(ns) = self.stdlib.get_namespace(name) {
            // "builtin" namespace means no import needed
            if ns == "builtin" {
                return;
            }

            // Check if imported (either specific or wildcard)
            if !self.imported_functions.contains(name) && !self.wildcard_imports.contains(ns) {
                // Try to find a similar function name
                let suggestion = did_you_mean(name, &self.available_functions);

                self.errors.push(ImportError {
                    function_name: name.to_string(),
                    defined_in: ns.clone(),
                    used_in: self.current_namespace.clone(),
                    span,
                    suggestion,
                });
            }
        }
        // If not in function_namespaces or stdlib, it might be a builtin (like println) - OK
    }

    /// Check an expression for function calls
    fn check_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Call { callee, args, .. } => {
                // Check if callee is a simple identifier
                if let Expr::Ident(name) = &callee.node {
                    self.check_function_call(name, callee.span.clone());
                } else {
                    // Check callee expression recursively
                    self.check_expr(&callee.node);
                }

                // Check arguments
                for arg in args {
                    self.check_expr(&arg.node);
                }
            }
            Expr::Binary { left, right, .. } => {
                self.check_expr(&left.node);
                self.check_expr(&right.node);
            }
            Expr::Unary { expr, .. } => {
                self.check_expr(&expr.node);
            }
            Expr::Field { object, .. } => {
                self.check_expr(&object.node);
            }
            Expr::Index { object, index } => {
                self.check_expr(&object.node);
                self.check_expr(&index.node);
            }

            Expr::Block(block) => {
                for stmt in block {
                    self.check_stmt(&stmt.node);
                }
            }

            Expr::Match { expr, arms } => {
                self.check_expr(&expr.node);
                for arm in arms {
                    for stmt in &arm.body {
                        self.check_stmt(&stmt.node);
                    }
                }
            }
            Expr::Lambda { body, .. } => {
                self.check_expr(&body.node);
            }
            Expr::Construct { args, .. } => {
                for arg in args {
                    self.check_expr(&arg.node);
                }
            }
            _ => {} // Literals, identifiers (non-call), etc.
        }
    }

    /// Check a statement for function calls
    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.check_expr(&expr.node);
            }
            Stmt::Let { value, .. } => {
                self.check_expr(&value.node);
            }
            Stmt::Return(Some(expr)) => {
                self.check_expr(&expr.node);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(&condition.node);
                for stmt in then_block {
                    self.check_stmt(&stmt.node);
                }
                if let Some(else_stmts) = else_block {
                    for stmt in else_stmts {
                        self.check_stmt(&stmt.node);
                    }
                }
            }
            Stmt::While { condition, body } => {
                self.check_expr(&condition.node);
                for stmt in body {
                    self.check_stmt(&stmt.node);
                }
            }
            Stmt::For { iterable, body, .. } => {
                self.check_expr(&iterable.node);
                for stmt in body {
                    self.check_stmt(&stmt.node);
                }
            }
            Stmt::Match { expr, arms } => {
                self.check_expr(&expr.node);
                for arm in arms {
                    for stmt in &arm.body {
                        self.check_stmt(&stmt.node);
                    }
                }
            }
            _ => {} // Break, Continue, Return(None), etc.
        }
    }

    /// Check entire program for import violations
    pub fn check_program(&mut self, program: &Program) -> Result<(), Vec<ImportError>> {
        for decl in &program.declarations {
            if let Decl::Function(func) = &decl.node {
                for stmt in &func.body {
                    self.check_stmt(&stmt.node);
                }
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }
}

/// Extract all function definitions from a program with their namespace
#[allow(dead_code)]
pub fn extract_function_namespaces(program: &Program, namespace: &str) -> HashMap<String, String> {
    let mut result = HashMap::new();

    for decl in &program.declarations {
        if let Decl::Function(func) = &decl.node {
            result.insert(func.name.clone(), namespace.to_string());
        }
    }

    result
}
