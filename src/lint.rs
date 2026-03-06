use crate::ast::{Decl, Expr, Program, Stmt, Type};
use crate::parser::Parser;
use crate::{lexer, stdlib::StdLib};
use std::collections::{BTreeSet, HashSet};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LintLevel {
    Warning,
}

#[derive(Debug, Clone)]
pub struct LintFinding {
    pub code: &'static str,
    pub level: LintLevel,
    pub message: String,
    pub suggestion: Option<String>,
}

impl LintFinding {
    pub fn format(&self) -> String {
        let level = match self.level {
            LintLevel::Warning => "warning",
        };
        let mut out = format!("[{}] {}: {}", self.code, level, self.message);
        if let Some(suggestion) = &self.suggestion {
            out.push_str(&format!("\n  hint: {}", suggestion));
        }
        out
    }
}

pub struct LintResult {
    pub findings: Vec<LintFinding>,
    pub fixed_source: Option<String>,
}

pub fn lint_source(source: &str, apply_fixes: bool) -> Result<LintResult, String> {
    let tokens = lexer::tokenize(source).map_err(|e| format!("Lexer error: {}", e))?;
    let mut parser = Parser::new(tokens);
    let program = parser
        .parse_program()
        .map_err(|e| format!("Parse error: {}", e.message))?;

    let mut findings = Vec::new();
    findings.extend(check_duplicate_imports(&program));
    findings.extend(check_import_sorting(&program));
    findings.extend(check_unused_specific_imports(&program));

    let fixed_source = if apply_fixes {
        Some(apply_safe_import_fixes(source, &program))
    } else {
        None
    };

    Ok(LintResult {
        findings,
        fixed_source,
    })
}

fn check_duplicate_imports(program: &Program) -> Vec<LintFinding> {
    let mut seen = HashSet::new();
    let mut duplicates = BTreeSet::new();

    for decl in &program.declarations {
        if let Decl::Import(import) = &decl.node {
            if !seen.insert(import.path.clone()) {
                duplicates.insert(import.path.clone());
            }
        }
    }

    duplicates
        .into_iter()
        .map(|path| LintFinding {
            code: "L001",
            level: LintLevel::Warning,
            message: format!("duplicate import '{}'", path),
            suggestion: Some("remove the redundant import".to_string()),
        })
        .collect()
}

fn check_import_sorting(program: &Program) -> Vec<LintFinding> {
    let imports: Vec<String> = program
        .declarations
        .iter()
        .filter_map(|decl| match &decl.node {
            Decl::Import(import) => Some(import.path.clone()),
            _ => None,
        })
        .collect();

    if imports.len() < 2 {
        return Vec::new();
    }

    let mut sorted = imports.clone();
    sorted.sort();
    sorted.dedup();

    if imports == sorted {
        Vec::new()
    } else {
        vec![LintFinding {
            code: "L002",
            level: LintLevel::Warning,
            message: "imports are not sorted and deduplicated".to_string(),
            suggestion: Some("run `apex fix` or sort imports lexicographically".to_string()),
        }]
    }
}

fn check_unused_specific_imports(program: &Program) -> Vec<LintFinding> {
    let mut used_names = HashSet::new();
    collect_used_names(program, &mut used_names);

    let stdlib = StdLib::new();
    let mut findings = Vec::new();
    for decl in &program.declarations {
        let Decl::Import(import) = &decl.node else {
            continue;
        };
        if import.path.ends_with(".*") {
            continue;
        }

        let Some(imported_name) = import.path.rsplit('.').next() else {
            continue;
        };

        if !used_names.contains(imported_name)
            && !stdlib.get_functions().contains_key(imported_name)
        {
            findings.push(LintFinding {
                code: "L003",
                level: LintLevel::Warning,
                message: format!("specific import '{}' appears unused", import.path),
                suggestion: Some(
                    "remove it or switch to a wildcard import only if justified".to_string(),
                ),
            });
        }
    }

    findings
}

fn apply_safe_import_fixes(source: &str, program: &Program) -> String {
    let mut imports = Vec::new();
    let mut body_lines = Vec::new();

    for line in source.lines() {
        let trimmed = line.trim();
        if trimmed.starts_with("import ") && trimmed.ends_with(';') {
            imports.push(trimmed.to_string());
        } else {
            body_lines.push(line);
        }
    }

    if imports.is_empty() {
        return source.to_string();
    }

    let mut package_line = None;
    if let Some(package) = &program.package {
        package_line = Some(format!("package {};", package));
    }

    imports.sort();
    imports.dedup();

    let mut output = String::new();
    if let Some(package_line) = package_line {
        output.push_str(&package_line);
        output.push_str("\n\n");
    }

    for import in &imports {
        output.push_str(import);
        output.push('\n');
    }
    output.push('\n');

    let body = body_lines
        .into_iter()
        .filter(|line| {
            let trimmed = line.trim();
            !trimmed.starts_with("package ") && !trimmed.starts_with("import ")
        })
        .collect::<Vec<_>>()
        .join("\n");
    output.push_str(body.trim_matches('\n'));
    if !output.ends_with('\n') {
        output.push('\n');
    }

    output
}

fn collect_used_names(program: &Program, used: &mut HashSet<String>) {
    for decl in &program.declarations {
        match &decl.node {
            Decl::Function(func) => {
                for param in &func.params {
                    collect_type_names(&param.ty, used);
                }
                collect_type_names(&func.return_type, used);
                for stmt in &func.body {
                    collect_stmt_names(&stmt.node, used);
                }
            }
            Decl::Class(class) => {
                if let Some(base) = &class.extends {
                    used.insert(base.clone());
                }
                for name in &class.implements {
                    used.insert(name.clone());
                }
                for field in &class.fields {
                    collect_type_names(&field.ty, used);
                }
                if let Some(ctor) = &class.constructor {
                    for param in &ctor.params {
                        collect_type_names(&param.ty, used);
                    }
                    for stmt in &ctor.body {
                        collect_stmt_names(&stmt.node, used);
                    }
                }
                if let Some(dtor) = &class.destructor {
                    for stmt in &dtor.body {
                        collect_stmt_names(&stmt.node, used);
                    }
                }
                for method in &class.methods {
                    for param in &method.params {
                        collect_type_names(&param.ty, used);
                    }
                    collect_type_names(&method.return_type, used);
                    for stmt in &method.body {
                        collect_stmt_names(&stmt.node, used);
                    }
                }
            }
            Decl::Enum(en) => {
                for variant in &en.variants {
                    for field in &variant.fields {
                        collect_type_names(&field.ty, used);
                    }
                }
            }
            Decl::Interface(interface) => {
                for name in &interface.extends {
                    used.insert(name.clone());
                }
                for method in &interface.methods {
                    for param in &method.params {
                        collect_type_names(&param.ty, used);
                    }
                    collect_type_names(&method.return_type, used);
                }
            }
            Decl::Module(module) => {
                let nested = Program {
                    package: None,
                    declarations: module.declarations.clone(),
                };
                collect_used_names(&nested, used);
            }
            Decl::Import(_) => {}
        }
    }
}

fn collect_stmt_names(stmt: &Stmt, used: &mut HashSet<String>) {
    match stmt {
        Stmt::Let { ty, value, .. } => {
            collect_type_names(ty, used);
            collect_expr_names(&value.node, used);
        }
        Stmt::Assign { target, value } => {
            collect_expr_names(&target.node, used);
            collect_expr_names(&value.node, used);
        }
        Stmt::Expr(expr) => collect_expr_names(&expr.node, used),
        Stmt::Return(expr) => {
            if let Some(expr) = expr {
                collect_expr_names(&expr.node, used);
            }
        }
        Stmt::If {
            condition,
            then_block,
            else_block,
        } => {
            collect_expr_names(&condition.node, used);
            for stmt in then_block {
                collect_stmt_names(&stmt.node, used);
            }
            if let Some(block) = else_block {
                for stmt in block {
                    collect_stmt_names(&stmt.node, used);
                }
            }
        }
        Stmt::While { condition, body } => {
            collect_expr_names(&condition.node, used);
            for stmt in body {
                collect_stmt_names(&stmt.node, used);
            }
        }
        Stmt::For {
            var_type,
            iterable,
            body,
            ..
        } => {
            if let Some(ty) = var_type {
                collect_type_names(ty, used);
            }
            collect_expr_names(&iterable.node, used);
            for stmt in body {
                collect_stmt_names(&stmt.node, used);
            }
        }
        Stmt::Match { expr, arms } => {
            collect_expr_names(&expr.node, used);
            for arm in arms {
                for stmt in &arm.body {
                    collect_stmt_names(&stmt.node, used);
                }
            }
        }
        Stmt::Break | Stmt::Continue => {}
    }
}

fn collect_expr_names(expr: &Expr, used: &mut HashSet<String>) {
    match expr {
        Expr::Ident(name) => {
            used.insert(name.clone());
        }
        Expr::Call { callee, args } => {
            collect_expr_names(&callee.node, used);
            for arg in args {
                collect_expr_names(&arg.node, used);
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_expr_names(&left.node, used);
            collect_expr_names(&right.node, used);
        }
        Expr::Unary { expr, .. }
        | Expr::Try(expr)
        | Expr::Borrow(expr)
        | Expr::MutBorrow(expr)
        | Expr::Deref(expr)
        | Expr::Await(expr) => collect_expr_names(&expr.node, used),
        Expr::Field { object, field } => {
            collect_expr_names(&object.node, used);
            used.insert(field.clone());
        }
        Expr::Index { object, index } => {
            collect_expr_names(&object.node, used);
            collect_expr_names(&index.node, used);
        }
        Expr::Construct { ty, args } => {
            used.insert(ty.clone());
            for arg in args {
                collect_expr_names(&arg.node, used);
            }
        }
        Expr::Lambda { params, body } => {
            for param in params {
                collect_type_names(&param.ty, used);
            }
            collect_expr_names(&body.node, used);
        }
        Expr::Match { expr, arms } => {
            collect_expr_names(&expr.node, used);
            for arm in arms {
                for stmt in &arm.body {
                    collect_stmt_names(&stmt.node, used);
                }
            }
        }
        Expr::StringInterp(parts) => {
            for part in parts {
                if let crate::ast::StringPart::Expr(expr) = part {
                    collect_expr_names(&expr.node, used);
                }
            }
        }
        Expr::AsyncBlock(block) | Expr::Block(block) => {
            for stmt in block {
                collect_stmt_names(&stmt.node, used);
            }
        }
        Expr::Require { condition, message } => {
            collect_expr_names(&condition.node, used);
            if let Some(message) = message {
                collect_expr_names(&message.node, used);
            }
        }
        Expr::Range { start, end, .. } => {
            if let Some(start) = start {
                collect_expr_names(&start.node, used);
            }
            if let Some(end) = end {
                collect_expr_names(&end.node, used);
            }
        }
        Expr::IfExpr {
            condition,
            then_branch,
            else_branch,
        } => {
            collect_expr_names(&condition.node, used);
            for stmt in then_branch {
                collect_stmt_names(&stmt.node, used);
            }
            if let Some(block) = else_branch {
                for stmt in block {
                    collect_stmt_names(&stmt.node, used);
                }
            }
        }
        Expr::Literal(_) | Expr::This => {}
    }
}

fn collect_type_names(ty: &Type, used: &mut HashSet<String>) {
    match ty {
        Type::Named(name) => {
            used.insert(name.clone());
        }
        Type::Generic(name, args) => {
            used.insert(name.clone());
            for arg in args {
                collect_type_names(arg, used);
            }
        }
        Type::Function(params, ret) => {
            for param in params {
                collect_type_names(param, used);
            }
            collect_type_names(ret, used);
        }
        Type::Option(inner)
        | Type::List(inner)
        | Type::Set(inner)
        | Type::Ref(inner)
        | Type::MutRef(inner)
        | Type::Box(inner)
        | Type::Rc(inner)
        | Type::Arc(inner)
        | Type::Ptr(inner)
        | Type::Task(inner)
        | Type::Range(inner) => collect_type_names(inner, used),
        Type::Result(ok, err) | Type::Map(ok, err) => {
            collect_type_names(ok, used);
            collect_type_names(err, used);
        }
        Type::Integer | Type::Float | Type::Boolean | Type::String | Type::Char | Type::None => {}
    }
}

#[cfg(test)]
mod tests {
    use super::lint_source;

    #[test]
    fn detects_duplicate_and_unsorted_imports() {
        let source = r#"import std.string.*;
import std.io.*;
import std.io.*;

function main(): None {
    println("ok");
    return None;
}
"#;
        let result = lint_source(source, false).expect("lint succeeds");
        assert_eq!(result.findings.len(), 2);
        assert!(result.findings.iter().any(|f| f.code == "L001"));
        assert!(result.findings.iter().any(|f| f.code == "L002"));
    }

    #[test]
    fn fixes_import_order_and_dedupes() {
        let source = r#"import std.string.*;
import std.io.*;
import std.io.*;

function main(): None {
    println("ok");
    return None;
}
"#;
        let result = lint_source(source, true).expect("lint succeeds");
        let fixed = result.fixed_source.expect("fixed source");
        assert!(fixed.starts_with("import std.io.*;\nimport std.string.*;"));
        assert_eq!(fixed.matches("import std.io.*;").count(), 1);
    }

    #[test]
    fn flags_unused_specific_imports() {
        let source = r#"import project.helper;
import std.io.*;

function main(): None {
    println("ok");
    return None;
}
"#;
        let result = lint_source(source, false).expect("lint succeeds");
        assert!(result.findings.iter().any(|f| f.code == "L003"));
    }
}
