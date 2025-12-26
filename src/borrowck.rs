//! Apex Borrow Checker - Ownership and lifetime analysis
//!
//! This module provides:
//! - Move semantics checking
//! - Borrow lifetime tracking
//! - Mutable borrow exclusivity
//! - Use-after-move detection

#![allow(dead_code)]

use crate::ast::*;
use std::collections::HashMap;

/// Borrow checking error
#[derive(Debug, Clone)]
pub struct BorrowError {
    pub message: String,
    pub span: Span,
    pub note: Option<(String, Span)>,
}

impl BorrowError {
    pub fn new(message: impl Into<String>, span: Span) -> Self {
        Self {
            message: message.into(),
            span,
            note: None,
        }
    }

    pub fn with_note(mut self, message: impl Into<String>, span: Span) -> Self {
        self.note = Some((message.into(), span));
        self
    }
}

/// Ownership state of a value
#[derive(Debug, Clone, PartialEq)]
enum OwnershipState {
    /// Value is owned and valid
    Owned,
    /// Value has been moved
    Moved(Span),
    /// Value is borrowed immutably (count of borrows)
    Borrowed(usize),
    /// Value is borrowed mutably
    MutBorrowed(Span),
}

/// Information about a borrow
#[derive(Debug, Clone)]
struct BorrowInfo {
    /// Variable being borrowed
    borrowed_from: String,
    /// Is it a mutable borrow?
    mutable: bool,
    /// Span where borrow was created
    span: Span,
    /// Scope depth where borrow is valid
    scope_depth: usize,
}

/// Variable tracking for borrow checker
#[derive(Debug, Clone)]
struct VarState {
    /// Current ownership state
    state: OwnershipState,
    /// Is this variable mutable?
    mutable: bool,
    /// Where was this variable declared?
    declared_at: Span,
    /// Type of the variable (for drop checking)
    needs_drop: bool,
}

/// Borrow checker state
pub struct BorrowChecker {
    /// Variable states by scope
    scopes: Vec<HashMap<String, VarState>>,
    /// Active borrows
    borrows: Vec<BorrowInfo>,
    /// Global function signatures
    functions: HashMap<String, Vec<ParamMode>>,
    /// Class method signatures
    classes: HashMap<String, ClassBorrowSigs>,
    /// Current scope depth
    scope_depth: usize,
    /// Collected errors
    errors: Vec<BorrowError>,
    /// Variables that need dropping at end of current scope
    drop_queue: Vec<Vec<String>>,
}

struct ClassBorrowSigs {
    methods: HashMap<String, Vec<ParamMode>>,
    constructor: Vec<ParamMode>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            borrows: Vec::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            scope_depth: 0,
            errors: Vec::new(),
            drop_queue: vec![Vec::new()],
        }
    }

    /// Run borrow checking on a program
    pub fn check(&mut self, program: &Program) -> Result<(), Vec<BorrowError>> {
        // First pass: collect signatures
        for decl in &program.declarations {
            self.collect_sig(&decl.node);
        }

        // Second pass: check function bodies
        for decl in &program.declarations {
            self.check_decl(&decl.node, decl.span.clone());
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn collect_sig(&mut self, decl: &Decl) {
        match decl {
            Decl::Function(func) => {
                self.functions.insert(
                    func.name.clone(),
                    func.params.iter().map(|p| p.mode).collect(),
                );
            }
            Decl::Class(class) => {
                let mut methods = HashMap::new();
                for method in &class.methods {
                    methods.insert(
                        method.name.clone(),
                        method.params.iter().map(|p| p.mode).collect(),
                    );
                }
                let constructor = class
                    .constructor
                    .as_ref()
                    .map(|c| c.params.iter().map(|p| p.mode).collect())
                    .unwrap_or_default();

                self.classes.insert(
                    class.name.clone(),
                    ClassBorrowSigs {
                        methods,
                        constructor,
                    },
                );
            }
            Decl::Module(module) => {
                for inner in &module.declarations {
                    self.collect_sig(&inner.node);
                }
            }
            _ => {}
        }
    }

    fn check_decl(&mut self, decl: &Decl, _span: Span) {
        match decl {
            Decl::Function(func) => self.check_function(func),
            Decl::Class(class) => self.check_class(class),
            Decl::Module(module) => {
                for inner in &module.declarations {
                    self.check_decl(&inner.node, inner.span.clone());
                }
            }
            _ => {}
        }
    }

    fn check_function(&mut self, func: &FunctionDecl) {
        self.enter_scope();

        // Add parameters with correct initial state
        for param in &func.params {
            self.declare_var(&param.name, param.mutable, 0..0, self.needs_drop(&param.ty));

            // If it's a borrow parameter, initialize it as borrowed
            match param.mode {
                ParamMode::Borrow => {
                    if let Some(var) = self.get_var_mut(&param.name) {
                        var.state = OwnershipState::Borrowed(1);
                    }
                }
                ParamMode::BorrowMut => {
                    if let Some(var) = self.get_var_mut(&param.name) {
                        var.state = OwnershipState::MutBorrowed(0..0);
                    }
                }
                ParamMode::Owned => {}
            }
        }

        self.check_block(&func.body);
        self.exit_scope();
    }

    fn check_class(&mut self, class: &ClassDecl) {
        // Check constructor
        if let Some(ctor) = &class.constructor {
            self.enter_scope();
            self.declare_var("this", true, 0..0, false);
            for param in &ctor.params {
                self.declare_var(&param.name, param.mutable, 0..0, self.needs_drop(&param.ty));
            }
            self.check_block(&ctor.body);
            self.exit_scope();
        }

        // Check methods
        for method in &class.methods {
            self.enter_scope();
            self.declare_var("this", false, 0..0, false);
            for param in &method.params {
                self.declare_var(&param.name, param.mutable, 0..0, self.needs_drop(&param.ty));

                // Initialize borrow state for parameters
                match param.mode {
                    ParamMode::Borrow => {
                        if let Some(var) = self.get_var_mut(&param.name) {
                            var.state = OwnershipState::Borrowed(1);
                        }
                    }
                    ParamMode::BorrowMut => {
                        if let Some(var) = self.get_var_mut(&param.name) {
                            var.state = OwnershipState::MutBorrowed(0..0);
                        }
                    }
                    ParamMode::Owned => {}
                }
            }
            self.check_block(&method.body);
            self.exit_scope();
        }
    }

    fn check_block(&mut self, block: &Block) {
        self.enter_scope();
        for stmt in block {
            self.check_stmt(&stmt.node, stmt.span.clone());
        }
        self.exit_scope();
    }

    fn check_stmt(&mut self, stmt: &Stmt, span: Span) {
        match stmt {
            Stmt::Let {
                name,
                ty,
                value,
                mutable,
            } => {
                // Check the value expression (may involve moves)
                self.check_expr(&value.node, value.span.clone(), false);

                // Declare the new variable
                self.declare_var(name, *mutable, span, self.needs_drop(ty));
            }

            Stmt::Assign { target, value } => {
                // Check value first
                self.check_expr(&value.node, value.span.clone(), false);

                // Check target is valid for assignment
                self.check_assign_target(&target.node, target.span.clone());
            }

            Stmt::Expr(expr) => {
                self.check_expr(&expr.node, expr.span.clone(), false);
            }

            Stmt::Return(expr) => {
                if let Some(e) = expr {
                    self.check_expr(&e.node, e.span.clone(), false);
                }
            }

            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(&condition.node, condition.span.clone(), false);
                self.check_block(then_block);
                if let Some(else_blk) = else_block {
                    self.check_block(else_blk);
                }
            }

            Stmt::While { condition, body } => {
                self.check_expr(&condition.node, condition.span.clone(), false);
                self.check_block(body);
            }

            Stmt::For {
                var,
                var_type,
                iterable,
                body,
            } => {
                self.check_expr(&iterable.node, iterable.span.clone(), false);
                self.enter_scope();
                let needs_drop = var_type
                    .as_ref()
                    .map(|t| self.needs_drop(t))
                    .unwrap_or(false);
                self.declare_var(var, false, span, needs_drop);
                for stmt in body {
                    self.check_stmt(&stmt.node, stmt.span.clone());
                }
                self.exit_scope();
            }

            Stmt::Match { expr, arms } => {
                self.check_expr(&expr.node, expr.span.clone(), false);
                for arm in arms {
                    self.enter_scope();
                    self.bind_pattern(&arm.pattern, span.clone());
                    for stmt in &arm.body {
                        self.check_stmt(&stmt.node, stmt.span.clone());
                    }
                    self.exit_scope();
                }
            }

            Stmt::Break | Stmt::Continue => {}
        }
    }

    fn check_assign_target(&mut self, target: &Expr, span: Span) {
        match target {
            Expr::Ident(name) => {
                // Check mutability and borrow state
                let (mutable, state) = {
                    if let Some(var) = self.get_var(name) {
                        (var.mutable, var.state.clone())
                    } else {
                        self.errors.push(BorrowError::new(
                            format!("Cannot assign to undeclared variable '{}'", name),
                            span.clone(),
                        ));
                        return;
                    }
                };

                if !mutable {
                    self.errors.push(BorrowError::new(
                        format!("Cannot assign to immutable variable '{}'", name),
                        span.clone(),
                    ));
                }

                match state {
                    OwnershipState::MutBorrowed(borrow_span) => {
                        self.errors.push(
                            BorrowError::new(
                                format!("Cannot assign to '{}' while mutably borrowed", name),
                                span.clone(),
                            )
                            .with_note("Mutable borrow occurred here", borrow_span),
                        );
                    }
                    OwnershipState::Borrowed(count) if count > 0 => {
                        self.errors.push(BorrowError::new(
                            format!("Cannot assign to '{}' while borrowed", name),
                            span.clone(),
                        ));
                    }
                    _ => {}
                }

                // Reset ownership state (old value dropped)
                if let Some(var) = self.get_var_mut(name) {
                    var.state = OwnershipState::Owned;
                }
            }
            Expr::Field { object, field: _ } => {
                self.check_expr(&object.node, object.span.clone(), false);
            }
            Expr::Index { object, index } => {
                self.check_expr(&object.node, object.span.clone(), false);
                self.check_expr(&index.node, index.span.clone(), false);
            }
            Expr::Deref(inner) => {
                // Check that we're dereferencing a mutable reference
                self.check_expr(&inner.node, inner.span.clone(), true);
            }
            _ => {
                self.errors.push(BorrowError::new(
                    "Invalid assignment target".to_string(),
                    span,
                ));
            }
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn check_expr(&mut self, expr: &Expr, span: Span, need_mut: bool) {
        match expr {
            Expr::Ident(name) => {
                // Using a variable - check if it's valid
                let state = self.get_var(name).map(|v| v.state.clone());
                if let Some(OwnershipState::Moved(move_span)) = state {
                    self.errors.push(
                        BorrowError::new(format!("Use of moved value '{}'", name), span.clone())
                            .with_note("Value moved here", move_span),
                    );
                }
            }

            Expr::Binary { left, right, .. } => {
                self.check_expr(&left.node, left.span.clone(), false);
                self.check_expr(&right.node, right.span.clone(), false);
            }

            Expr::Unary { expr: inner, .. } => {
                self.check_expr(&inner.node, inner.span.clone(), false);
            }

            Expr::Call { callee, args } => {
                self.check_expr(&callee.node, callee.span.clone(), false);

                // Get param modes if possible
                let mut param_modes = Vec::new();
                if let Expr::Ident(name) = &callee.node {
                    if let Some(modes) = self.functions.get(name) {
                        param_modes = modes.clone();
                    }
                } else if let Expr::Field { object: _, field } = &callee.node {
                    // Method call - ideally we'd know the type of object
                    // For now, look for any method with this name across all classes
                    // (heuristic until we have full type info in borrowck)
                    for class_sig in self.classes.values() {
                        if let Some(modes) = class_sig.methods.get(field) {
                            param_modes = modes.clone();
                            break;
                        }
                    }
                }

                for (i, arg) in args.iter().enumerate() {
                    self.check_expr(&arg.node, arg.span.clone(), false);

                    let mode = param_modes.get(i).unwrap_or(&ParamMode::Owned);
                    match mode {
                        ParamMode::Owned => {
                            self.try_move(&arg.node, arg.span.clone());
                        }
                        ParamMode::Borrow => {
                            if let Expr::Ident(name) = &arg.node {
                                self.create_borrow(name, false, arg.span.clone());
                            }
                        }
                        ParamMode::BorrowMut => {
                            if let Expr::Ident(name) = &arg.node {
                                self.create_borrow(name, true, arg.span.clone());
                            }
                        }
                    }
                }
            }

            Expr::Field { object, field: _ } => {
                self.check_expr(&object.node, object.span.clone(), need_mut);
            }

            Expr::Index { object, index } => {
                self.check_expr(&object.node, object.span.clone(), need_mut);
                self.check_expr(&index.node, index.span.clone(), false);
            }

            Expr::Construct { ty, args } => {
                // Get constructor param modes
                let param_modes = self
                    .classes
                    .get(ty)
                    .map(|c| c.constructor.clone())
                    .unwrap_or_default();

                for (i, arg) in args.iter().enumerate() {
                    self.check_expr(&arg.node, arg.span.clone(), false);

                    let mode = param_modes.get(i).unwrap_or(&ParamMode::Owned);
                    match mode {
                        ParamMode::Owned => {
                            self.try_move(&arg.node, arg.span.clone());
                        }
                        ParamMode::Borrow => {
                            if let Expr::Ident(name) = &arg.node {
                                self.create_borrow(name, false, arg.span.clone());
                            }
                        }
                        ParamMode::BorrowMut => {
                            if let Expr::Ident(name) = &arg.node {
                                self.create_borrow(name, true, arg.span.clone());
                            }
                        }
                    }
                }
            }

            Expr::Lambda { params, body } => {
                // Lambda captures - check which variables are captured
                self.enter_scope();
                for param in params {
                    self.declare_var(&param.name, param.mutable, span.clone(), false);
                }
                self.check_expr(&body.node, body.span.clone(), false);
                self.exit_scope();
            }

            Expr::Borrow(inner) => {
                // Create immutable borrow
                if let Expr::Ident(name) = &inner.node {
                    self.create_borrow(name, false, span.clone());
                } else {
                    self.check_expr(&inner.node, inner.span.clone(), false);
                }
            }

            Expr::MutBorrow(inner) => {
                // Create mutable borrow
                if let Expr::Ident(name) = &inner.node {
                    self.create_borrow(name, true, span.clone());
                } else {
                    self.check_expr(&inner.node, inner.span.clone(), true);
                }
            }

            Expr::Deref(inner) => {
                self.check_expr(&inner.node, inner.span.clone(), need_mut);
            }

            Expr::Try(inner) => {
                self.check_expr(&inner.node, inner.span.clone(), false);
            }

            Expr::StringInterp(parts) => {
                for part in parts {
                    if let StringPart::Expr(e) = part {
                        self.check_expr(&e.node, e.span.clone(), false);
                    }
                }
            }

            Expr::Match { expr: inner, arms } => {
                self.check_expr(&inner.node, inner.span.clone(), false);
                for arm in arms {
                    self.enter_scope();
                    self.bind_pattern(&arm.pattern, span.clone());
                    for stmt in &arm.body {
                        self.check_stmt(&stmt.node, stmt.span.clone());
                    }
                    self.exit_scope();
                }
            }

            Expr::Await(inner) => {
                self.check_expr(&inner.node, inner.span.clone(), false);
            }

            Expr::AsyncBlock(body) => {
                self.enter_scope();
                for stmt in body {
                    self.check_stmt(&stmt.node, stmt.span.clone());
                }
                self.exit_scope();
            }

            Expr::Require { condition, message } => {
                self.check_expr(&condition.node, condition.span.clone(), false);
                if let Some(msg) = message {
                    self.check_expr(&msg.node, msg.span.clone(), false);
                }
            }

            Expr::Range {
                start,
                end,
                inclusive: _,
            } => {
                if let Some(s) = start {
                    self.check_expr(&s.node, s.span.clone(), false);
                }
                if let Some(e) = end {
                    self.check_expr(&e.node, e.span.clone(), false);
                }
            }

            Expr::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => {
                self.check_expr(&condition.node, condition.span.clone(), false);
                self.enter_scope();
                for stmt in then_branch {
                    self.check_stmt(&stmt.node, stmt.span.clone());
                }
                self.exit_scope();
                if let Some(else_stmts) = else_branch {
                    self.enter_scope();
                    for stmt in else_stmts {
                        self.check_stmt(&stmt.node, stmt.span.clone());
                    }
                    self.exit_scope();
                }
            }

            Expr::Block(body) => {
                self.enter_scope();
                for stmt in body {
                    self.check_stmt(&stmt.node, stmt.span.clone());
                }
                self.exit_scope();
            }

            Expr::Literal(_) | Expr::This => {}
        }
    }

    /// Try to move a value
    fn try_move(&mut self, expr: &Expr, span: Span) {
        if let Expr::Ident(name) = expr {
            // Get info about the variable
            let (needs_drop, state) = {
                if let Some(var) = self.get_var(name) {
                    (var.needs_drop, var.state.clone())
                } else {
                    return;
                }
            };

            // Don't move Copy types
            if !needs_drop {
                return;
            }

            // Check for active borrows
            match state {
                OwnershipState::MutBorrowed(borrow_span) => {
                    self.errors.push(
                        BorrowError::new(
                            format!("Cannot move '{}' while mutably borrowed", name),
                            span.clone(),
                        )
                        .with_note("Mutable borrow occurred here", borrow_span),
                    );
                    return;
                }
                OwnershipState::Borrowed(count) if count > 0 => {
                    self.errors.push(BorrowError::new(
                        format!("Cannot move '{}' while borrowed", name),
                        span.clone(),
                    ));
                    return;
                }
                _ => {}
            }

            // Mark as moved
            if let Some(var) = self.get_var_mut(name) {
                var.state = OwnershipState::Moved(span);
            }
        }
    }

    /// Create a borrow
    fn create_borrow(&mut self, name: &str, mutable: bool, span: Span) {
        // Get current state
        let (var_mutable, state) = {
            if let Some(var) = self.get_var(name) {
                (var.mutable, var.state.clone())
            } else {
                return;
            }
        };

        // Check current state
        match state {
            OwnershipState::Moved(move_span) => {
                self.errors.push(
                    BorrowError::new(format!("Cannot borrow '{}' after move", name), span.clone())
                        .with_note("Value was moved here", move_span),
                );
                return;
            }
            OwnershipState::MutBorrowed(borrow_span) => {
                self.errors.push(
                    BorrowError::new(
                        format!("Cannot borrow '{}' while mutably borrowed", name),
                        span.clone(),
                    )
                    .with_note("Mutable borrow occurred here", borrow_span),
                );
                return;
            }
            OwnershipState::Borrowed(count) if mutable && count > 0 => {
                self.errors.push(BorrowError::new(
                    format!("Cannot mutably borrow '{}' while immutably borrowed", name),
                    span.clone(),
                ));
                return;
            }
            _ => {}
        }

        // Check mutability for mut borrow
        if mutable && !var_mutable {
            self.errors.push(BorrowError::new(
                format!("Cannot mutably borrow immutable variable '{}'", name),
                span.clone(),
            ));
            return;
        }

        // Update state
        if let Some(var) = self.get_var_mut(name) {
            if mutable {
                var.state = OwnershipState::MutBorrowed(span.clone());
            } else {
                match &mut var.state {
                    OwnershipState::Borrowed(count) => *count += 1,
                    OwnershipState::Owned => var.state = OwnershipState::Borrowed(1),
                    _ => {}
                }
            }
        }

        self.borrows.push(BorrowInfo {
            borrowed_from: name.to_string(),
            mutable,
            span,
            scope_depth: self.scope_depth,
        });
    }

    fn bind_pattern(&mut self, pattern: &Pattern, span: Span) {
        match pattern {
            Pattern::Ident(name) => {
                self.declare_var(name, false, span, false);
            }
            Pattern::Variant(_, bindings) => {
                for binding in bindings {
                    self.declare_var(binding, false, span.clone(), false);
                }
            }
            _ => {}
        }
    }

    fn declare_var(&mut self, name: &str, mutable: bool, span: Span, needs_drop: bool) {
        let var = VarState {
            state: OwnershipState::Owned,
            mutable,
            declared_at: span,
            needs_drop,
        };
        self.scopes
            .last_mut()
            .unwrap()
            .insert(name.to_string(), var);

        if needs_drop {
            self.drop_queue.last_mut().unwrap().push(name.to_string());
        }
    }

    fn get_var(&self, name: &str) -> Option<&VarState> {
        for scope in self.scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn get_var_mut(&mut self, name: &str) -> Option<&mut VarState> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(var) = scope.get_mut(name) {
                return Some(var);
            }
        }
        None
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
        self.drop_queue.push(Vec::new());
        self.scope_depth += 1;
    }

    fn exit_scope(&mut self) {
        // Release borrows from this scope
        self.borrows.retain(|b| b.scope_depth < self.scope_depth);

        // Update borrow counts for variables
        for scope in &mut self.scopes {
            for (_, var) in scope.iter_mut() {
                if let OwnershipState::MutBorrowed(_) = &var.state {
                    // Check if borrow is still active
                    let still_borrowed = self
                        .borrows
                        .iter()
                        .any(|b| b.scope_depth >= self.scope_depth);
                    if !still_borrowed {
                        var.state = OwnershipState::Owned;
                    }
                }
            }
        }

        self.scopes.pop();
        self.drop_queue.pop();
        self.scope_depth -= 1;
    }

    /// Check if a type needs drop (not Copy)
    fn needs_drop(&self, ty: &Type) -> bool {
        match ty {
            Type::Integer | Type::Float | Type::Boolean | Type::Char | Type::None => false,
            Type::Ref(_) | Type::MutRef(_) => false, // References don't own data
            _ => true,                               // Strings, classes, collections need drop
        }
    }
}

/// Format borrow errors with source context
pub fn format_borrow_errors(errors: &[BorrowError], source: &str, filename: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();
    let mut output = String::new();

    for error in errors {
        let (line_num, col) = span_to_location(&error.span, source);

        output.push_str(&format!(
            "\x1b[1;31merror[E0505]\x1b[0m: {}\n",
            error.message
        ));
        output.push_str(&format!(
            "  \x1b[1;34m-->\x1b[0m {}:{}:{}\n",
            filename, line_num, col
        ));
        output.push_str("   \x1b[1;34m|\x1b[0m\n");

        if line_num <= lines.len() {
            output.push_str(&format!(
                "\x1b[1;34m{:3} |\x1b[0m {}\n",
                line_num,
                lines[line_num - 1]
            ));

            let underline_start = col.saturating_sub(1);
            let underline_len = (error.span.end - error.span.start).max(1);
            output.push_str(&format!(
                "   \x1b[1;34m|\x1b[0m {}\x1b[1;31m{}\x1b[0m\n",
                " ".repeat(underline_start),
                "^".repeat(underline_len.min(50))
            ));
        }

        if let Some((note_msg, note_span)) = &error.note {
            let (note_line, _) = span_to_location(note_span, source);
            output.push_str("   \x1b[1;34m|\x1b[0m\n");
            output.push_str(&format!(
                "   \x1b[1;34m= note\x1b[0m: {} (at line {})\n",
                note_msg, note_line
            ));
        }

        output.push('\n');
    }

    output
}

fn span_to_location(span: &Span, source: &str) -> (usize, usize) {
    let mut line_num: usize = 1;
    let mut col: usize = 1;

    for (i, ch) in source.char_indices() {
        if i >= span.start {
            break;
        }
        if ch == '\n' {
            line_num += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line_num, col)
}
