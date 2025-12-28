//! Apex Code Generator - LLVM IR generation

#![allow(dead_code)]

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType};
use inkwell::values::{
    BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue, ValueKind,
};
use inkwell::{AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel};
use std::collections::HashMap;
use std::path::Path;

use crate::ast::*;

/// Codegen error
#[derive(Debug)]
pub struct CodegenError {
    pub message: String,
}

impl CodegenError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

type Result<T> = std::result::Result<T, CodegenError>;

/// Variable in codegen
#[derive(Debug, Clone)]
struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    ty: Type,
}

/// Class info
struct ClassInfo<'ctx> {
    struct_type: StructType<'ctx>,
    field_indices: HashMap<String, u32>,
    field_types: HashMap<String, Type>,
}

/// Loop context for break/continue
struct LoopContext<'ctx> {
    loop_block: BasicBlock<'ctx>,
    after_block: BasicBlock<'ctx>,
}

/// Code generator
pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    variables: HashMap<String, Variable<'ctx>>,
    functions: HashMap<String, (FunctionValue<'ctx>, Type)>,
    classes: HashMap<String, ClassInfo<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
    current_return_type: Option<Type>,
    loop_stack: Vec<LoopContext<'ctx>>,
    str_counter: u32,
    lambda_counter: u32,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            classes: HashMap::new(),
            current_function: None,
            current_return_type: None,
            loop_stack: Vec::new(),
            str_counter: 0,
            lambda_counter: 0,
        }
    }

    /// Compile program
    pub fn compile(&mut self, program: &Program) -> Result<()> {
        // First pass: declare all classes and functions
        for decl in &program.declarations {
            match &decl.node {
                Decl::Class(class) => self.declare_class(class)?,
                Decl::Function(func) => {
                    self.declare_function(func)?;
                }
                Decl::Enum(_) => {}      // TODO
                Decl::Interface(_) => {} // Interfaces don't generate code
                Decl::Module(module) => self.declare_module(module)?,
                Decl::Import(_) => {} // Handled at file level
            }
        }

        // Second pass: compile function bodies
        for decl in &program.declarations {
            match &decl.node {
                Decl::Function(func) => self.compile_function(func)?,
                Decl::Class(class) => self.compile_class(class)?,
                Decl::Enum(_) => {}      // TODO
                Decl::Interface(_) => {} // Interfaces don't generate code
                Decl::Module(module) => self.compile_module(module)?,
                Decl::Import(_) => {} // Handled at file level
            }
        }

        Ok(())
    }

    fn declare_module(&mut self, module: &ModuleDecl) -> Result<()> {
        // Declare module contents with prefixed names
        for decl in &module.declarations {
            match &decl.node {
                Decl::Function(func) => {
                    let mut prefixed_func = func.clone();
                    prefixed_func.name = format!("{}__{}", module.name, func.name);
                    self.declare_function(&prefixed_func)?;
                }
                Decl::Class(class) => {
                    let mut prefixed_class = class.clone();
                    prefixed_class.name = format!("{}__{}", module.name, class.name);
                    self.declare_class(&prefixed_class)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    fn compile_module(&mut self, module: &ModuleDecl) -> Result<()> {
        // Compile module contents
        for decl in &module.declarations {
            match &decl.node {
                Decl::Function(func) => {
                    let mut prefixed_func = func.clone();
                    prefixed_func.name = format!("{}__{}", module.name, func.name);
                    self.compile_function(&prefixed_func)?;
                }
                Decl::Class(class) => {
                    let mut prefixed_class = class.clone();
                    prefixed_class.name = format!("{}__{}", module.name, class.name);
                    self.compile_class(&prefixed_class)?;
                }
                _ => {}
            }
        }
        Ok(())
    }

    // === Type System ===

    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Integer => self.context.i64_type().into(),
            Type::Float => self.context.f64_type().into(),
            Type::Boolean => self.context.bool_type().into(),
            Type::String => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Char => self.context.i8_type().into(),
            Type::None => self.context.i8_type().into(),
            Type::Named(_name) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Generic(_, _) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Function(_, _) => self
                .context
                .struct_type(
                    &[
                        self.context.ptr_type(AddressSpace::default()).into(), // function pointer
                        self.context.ptr_type(AddressSpace::default()).into(), // environment pointer
                    ],
                    false,
                )
                .into(),
            // Option<T> is represented as a struct { is_some: i8, value: T }
            Type::Option(inner) => {
                let inner_ty = self.llvm_type(inner);
                self.context
                    .struct_type(
                        &[
                            self.context.i8_type().into(), // tag: 0=None, 1=Some
                            inner_ty,                      // value
                        ],
                        false,
                    )
                    .into()
            }
            // Result<T, E> is represented as struct { is_ok: i8, ok_value: T, err_value: E }
            Type::Result(ok_ty, err_ty) => {
                let ok_llvm = self.llvm_type(ok_ty);
                let err_llvm = self.llvm_type(err_ty);
                self.context
                    .struct_type(
                        &[
                            self.context.i8_type().into(), // tag: 1=Ok, 0=Error
                            ok_llvm,                       // ok value
                            err_llvm,                      // error value
                        ],
                        false,
                    )
                    .into()
            }
            // List<T> is represented as struct { capacity: i64, length: i64, data: ptr }
            Type::List(_) => {
                self.context
                    .struct_type(
                        &[
                            self.context.i64_type().into(),                        // capacity
                            self.context.i64_type().into(),                        // length
                            self.context.ptr_type(AddressSpace::default()).into(), // data pointer
                        ],
                        false,
                    )
                    .into()
            }
            // Map<K, V> - for now just a pointer (will need proper implementation)
            Type::Map(_, _) => {
                self.context
                    .struct_type(
                        &[
                            self.context.i64_type().into(),                        // capacity
                            self.context.i64_type().into(),                        // length
                            self.context.ptr_type(AddressSpace::default()).into(), // keys pointer
                            self.context.ptr_type(AddressSpace::default()).into(), // values pointer
                        ],
                        false,
                    )
                    .into()
            }
            // Set<T> - similar to List
            Type::Set(_) => {
                self.context
                    .struct_type(
                        &[
                            self.context.i64_type().into(),                        // capacity
                            self.context.i64_type().into(),                        // length
                            self.context.ptr_type(AddressSpace::default()).into(), // data pointer
                        ],
                        false,
                    )
                    .into()
            }
            // Reference types - represented as pointers
            Type::Ref(_) | Type::MutRef(_) => self.context.ptr_type(AddressSpace::default()).into(),
            // Smart pointers - all represented as pointers
            Type::Box(_) | Type::Rc(_) | Type::Arc(_) => {
                self.context.ptr_type(AddressSpace::default()).into()
            }
            // Task<T> - currently represented as T in the stub implementation
            Type::Task(inner) => self.llvm_type(inner),
        }
    }

    // === Classes ===

    fn declare_class(&mut self, class: &ClassDecl) -> Result<()> {
        let mut field_llvm_types: Vec<BasicTypeEnum<'ctx>> = Vec::new();
        let mut field_indices: HashMap<String, u32> = HashMap::new();
        let mut field_types_map: HashMap<String, Type> = HashMap::new();

        for (i, field) in class.fields.iter().enumerate() {
            field_llvm_types.push(self.llvm_type(&field.ty));
            field_indices.insert(field.name.clone(), i as u32);
            field_types_map.insert(field.name.clone(), field.ty.clone());
        }

        let struct_type = self.context.struct_type(&field_llvm_types, false);
        self.classes.insert(
            class.name.clone(),
            ClassInfo {
                struct_type,
                field_indices,
                field_types: field_types_map,
            },
        );

        // Declare constructor
        if class.constructor.is_some() {
            self.declare_class_constructor(class)?;
        }

        // Declare methods
        for method in &class.methods {
            self.declare_class_method(class, method)?;
        }

        Ok(())
    }

    fn declare_class_constructor(&mut self, class: &ClassDecl) -> Result<()> {
        let constructor = class.constructor.as_ref().unwrap();
        let param_types: Vec<BasicMetadataTypeEnum> = constructor
            .params
            .iter()
            .map(|p| self.llvm_type(&p.ty).into())
            .collect();

        let mut llvm_params: Vec<BasicMetadataTypeEnum> = vec![
            self.context.ptr_type(AddressSpace::default()).into(), // env_ptr
        ];
        llvm_params.extend(param_types);

        let ret_type = self.context.ptr_type(AddressSpace::default());
        let fn_type = ret_type.fn_type(&llvm_params, false);

        let name = format!("{}__new", class.name);
        let func = self.module.add_function(&name, fn_type, None);
        self.functions.insert(
            name,
            (
                func,
                Type::Function(
                    constructor.params.iter().map(|p| p.ty.clone()).collect(),
                    Box::new(Type::Named(class.name.clone())),
                ),
            ),
        );

        Ok(())
    }

    fn declare_class_method(&mut self, class: &ClassDecl, method: &FunctionDecl) -> Result<()> {
        let self_type = self.context.ptr_type(AddressSpace::default());

        let mut llvm_params: Vec<BasicMetadataTypeEnum> = vec![
            self.context.ptr_type(AddressSpace::default()).into(), // env_ptr
            self_type.into(),                                      // this
        ];
        for param in &method.params {
            llvm_params.push(self.llvm_type(&param.ty).into());
        }

        let fn_type = match &method.return_type {
            Type::None => self.context.void_type().fn_type(&llvm_params, false),
            ty => self.llvm_type(ty).fn_type(&llvm_params, false),
        };

        let name = format!("{}__{}", class.name, method.name);
        let func = self.module.add_function(&name, fn_type, None);
        self.functions.insert(
            name,
            (
                func,
                Type::Function(
                    method.params.iter().map(|p| p.ty.clone()).collect(),
                    Box::new(method.return_type.clone()),
                ),
            ),
        );

        Ok(())
    }

    fn compile_class(&mut self, class: &ClassDecl) -> Result<()> {
        if let Some(constructor) = &class.constructor {
            self.compile_constructor(class, constructor)?;
        }

        for method in &class.methods {
            self.compile_method(class, method)?;
        }

        Ok(())
    }

    fn compile_constructor(&mut self, class: &ClassDecl, constructor: &Constructor) -> Result<()> {
        let name = format!("{}__new", class.name);
        let (func, _) = self.functions.get(&name).unwrap().clone();

        self.current_function = Some(func);
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();

        // Allocate parameters
        // Param 0 is env_ptr, constructor params start at 1
        for (i, param) in constructor.params.iter().enumerate() {
            let llvm_param = func.get_nth_param((i + 1) as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.llvm_type(&param.ty), &param.name)
                .unwrap();
            self.builder.build_store(alloca, llvm_param).unwrap();
            self.variables.insert(
                param.name.clone(),
                Variable {
                    ptr: alloca,
                    ty: param.ty.clone(),
                },
            );
        }

        // Allocate instance
        let class_info = self.classes.get(&class.name).unwrap();
        let struct_type = class_info.struct_type;
        let malloc = self.get_or_declare_malloc();
        let size = struct_type.size_of().unwrap();
        let ptr = self
            .builder
            .build_call(malloc, &[size.into()], "instance")
            .unwrap();
        let instance = match ptr.try_as_basic_value() {
            ValueKind::Basic(val) => val.into_pointer_value(),
            _ => panic!("malloc should return a value"),
        };

        // Store 'this'
        let this_alloca = self
            .builder
            .build_alloca(self.context.ptr_type(AddressSpace::default()), "this")
            .unwrap();
        self.builder.build_store(this_alloca, instance).unwrap();
        self.variables.insert(
            "this".to_string(),
            Variable {
                ptr: this_alloca,
                ty: Type::Named(class.name.clone()),
            },
        );

        // Compile body
        for stmt in &constructor.body {
            self.compile_stmt(&stmt.node)?;
        }

        // Return instance
        let this_val = self
            .builder
            .build_load(
                self.context.ptr_type(AddressSpace::default()),
                this_alloca,
                "this",
            )
            .unwrap();
        self.builder.build_return(Some(&this_val)).unwrap();

        self.current_function = None;
        Ok(())
    }

    fn compile_method(&mut self, class: &ClassDecl, method: &FunctionDecl) -> Result<()> {
        let name = format!("{}__{}", class.name, method.name);
        let (func, _) = self.functions.get(&name).unwrap().clone();

        self.current_function = Some(func);
        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();

        // Param 0 is env_ptr
        // Store 'this' (Param 1)
        let this_param = func.get_nth_param(1).unwrap();
        let class_info = self.classes.get(&class.name).unwrap();
        let _struct_type = class_info.struct_type;
        let this_alloca = self
            .builder
            .build_alloca(self.context.ptr_type(AddressSpace::default()), "this")
            .unwrap();
        self.builder.build_store(this_alloca, this_param).unwrap();
        self.variables.insert(
            "this".to_string(),
            Variable {
                ptr: this_alloca,
                ty: Type::Named(class.name.clone()),
            },
        );

        // Store parameters
        // Start from index 2 because 0=env_ptr, 1=this
        for (i, param) in method.params.iter().enumerate() {
            let llvm_param = func.get_nth_param((i + 2) as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.llvm_type(&param.ty), &param.name)
                .unwrap();
            self.builder.build_store(alloca, llvm_param).unwrap();
            self.variables.insert(
                param.name.clone(),
                Variable {
                    ptr: alloca,
                    ty: param.ty.clone(),
                },
            );
        }

        // Compile body
        for stmt in &method.body {
            self.compile_stmt(&stmt.node)?;
        }

        // Add implicit return
        if self.needs_terminator() {
            match &method.return_type {
                Type::None => {
                    self.builder.build_return(None).unwrap();
                }
                _ => {
                    self.builder.build_unreachable().unwrap();
                }
            }
        }

        self.current_function = None;
        Ok(())
    }

    // === Functions ===

    fn declare_function(&mut self, func: &FunctionDecl) -> Result<FunctionValue<'ctx>> {
        let param_types: Vec<BasicMetadataTypeEnum> = func
            .params
            .iter()
            .map(|p| self.llvm_type(&p.ty).into())
            .collect();

        let mut llvm_params: Vec<BasicMetadataTypeEnum> = vec![
            self.context.ptr_type(AddressSpace::default()).into(), // env_ptr
        ];
        llvm_params.extend(param_types);

        // Main function always returns i32 for C runtime compatibility
        let fn_type = if func.name == "main" {
            // Main doesn't get an env_ptr in standard C entry
            let main_params: Vec<BasicMetadataTypeEnum> = func
                .params
                .iter()
                .map(|p| self.llvm_type(&p.ty).into())
                .collect();
            self.context.i32_type().fn_type(&main_params, false)
        } else {
            match &func.return_type {
                Type::None => self.context.void_type().fn_type(&llvm_params, false),
                ty => self.llvm_type(ty).fn_type(&llvm_params, false),
            }
        };

        let function = self.module.add_function(&func.name, fn_type, None);
        self.functions.insert(
            func.name.clone(),
            (
                function,
                Type::Function(
                    func.params.iter().map(|p| p.ty.clone()).collect(),
                    Box::new(func.return_type.clone()),
                ),
            ),
        );
        Ok(function)
    }

    fn compile_function(&mut self, func: &FunctionDecl) -> Result<()> {
        let (function, _) = self.functions.get(&func.name).unwrap().clone();

        self.current_function = Some(function);
        self.current_return_type = Some(func.return_type.clone());
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);
        self.variables.clear();
        self.loop_stack.clear();

        // Allocate parameters
        // Param 0 is env_ptr
        for (i, param) in func.params.iter().enumerate() {
            let llvm_param = function.get_nth_param((i + 1) as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.llvm_type(&param.ty), &param.name)
                .unwrap();
            self.builder.build_store(alloca, llvm_param).unwrap();
            self.variables.insert(
                param.name.clone(),
                Variable {
                    ptr: alloca,
                    ty: param.ty.clone(),
                },
            );
        }

        // Compile body
        for stmt in &func.body {
            self.compile_stmt(&stmt.node)?;
        }

        // Add implicit return
        if self.needs_terminator() {
            if func.name == "main" {
                // Main returns 0 for success
                let zero = self.context.i32_type().const_int(0, false);
                self.builder.build_return(Some(&zero)).unwrap();
            } else {
                match &func.return_type {
                    Type::None => {
                        self.builder.build_return(None).unwrap();
                    }
                    _ => {
                        self.builder.build_unreachable().unwrap();
                    }
                }
            }
        }

        self.current_function = None;
        Ok(())
    }

    // === Statements ===

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let {
                name,
                ty,
                value,
                mutable: _,
            } => {
                let val = self.compile_expr(&value.node)?;
                let alloca = self.builder.build_alloca(self.llvm_type(ty), name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.variables.insert(
                    name.clone(),
                    Variable {
                        ptr: alloca,
                        ty: ty.clone(),
                    },
                );
            }

            Stmt::Assign { target, value } => {
                let val = self.compile_expr(&value.node)?;
                let ptr = self.compile_lvalue(&target.node)?;
                self.builder.build_store(ptr, val).unwrap();
            }

            Stmt::Expr(expr) => {
                self.compile_expr(&expr.node)?;
            }

            Stmt::Return(value) => {
                // Check if we're in main function (returns i32)
                let is_main = self
                    .current_function
                    .map(|f| f.get_name().to_str().unwrap_or("") == "main")
                    .unwrap_or(false);

                match value {
                    Some(expr) => {
                        // Check if returning None literal
                        if matches!(&expr.node, Expr::Literal(Literal::None)) {
                            if is_main {
                                let zero = self.context.i32_type().const_int(0, false);
                                self.builder.build_return(Some(&zero)).unwrap();
                            } else {
                                // Check if function returns Task<None> (needs i8 return) or just None (void)
                                let return_type = self.current_return_type.as_ref();
                                let is_task_none = return_type
                                .map(|t| matches!(t, Type::Task(inner) if matches!(**inner, Type::None)))
                                .unwrap_or(false);

                                if is_task_none {
                                    // Task<None> returns i8 in our stub implementation
                                    let none_val = self.context.i8_type().const_int(0, false);
                                    self.builder.build_return(Some(&none_val)).unwrap();
                                } else {
                                    // Regular None return (void function)
                                    self.builder.build_return(None).unwrap();
                                }
                            }
                        } else {
                            let val = self.compile_expr(&expr.node)?;
                            self.builder.build_return(Some(&val)).unwrap();
                        }
                    }
                    None => {
                        if is_main {
                            let zero = self.context.i32_type().const_int(0, false);
                            self.builder.build_return(Some(&zero)).unwrap();
                        } else {
                            self.builder.build_return(None).unwrap();
                        }
                    }
                }
            }

            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.compile_if(condition, then_block, else_block.as_ref())?;
            }

            Stmt::While { condition, body } => {
                self.compile_while(condition, body)?;
            }

            Stmt::For {
                var,
                var_type,
                iterable,
                body,
            } => {
                self.compile_for(var, var_type.as_ref(), iterable, body)?;
            }

            Stmt::Break => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    self.builder
                        .build_unconditional_branch(loop_ctx.after_block)
                        .unwrap();
                }
            }

            Stmt::Continue => {
                if let Some(loop_ctx) = self.loop_stack.last() {
                    self.builder
                        .build_unconditional_branch(loop_ctx.loop_block)
                        .unwrap();
                }
            }

            Stmt::Match { expr, arms } => {
                self.compile_match_stmt(expr, arms)?;
            }
        }

        Ok(())
    }

    fn compile_if(
        &mut self,
        cond: &Spanned<Expr>,
        then_block: &Block,
        else_block: Option<&Block>,
    ) -> Result<()> {
        let cond_val = self.compile_expr(&cond.node)?.into_int_value();
        let func = self.current_function.unwrap();

        let then_bb = self.context.append_basic_block(func, "then");
        let else_bb = self.context.append_basic_block(func, "else");
        let merge_bb = self.context.append_basic_block(func, "merge");

        self.builder
            .build_conditional_branch(cond_val, then_bb, else_bb)
            .unwrap();

        // Then
        self.builder.position_at_end(then_bb);
        for stmt in then_block {
            self.compile_stmt(&stmt.node)?;
        }
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        // Else
        self.builder.position_at_end(else_bb);
        if let Some(else_stmts) = else_block {
            for stmt in else_stmts {
                self.compile_stmt(&stmt.node)?;
            }
        }
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(merge_bb);
        Ok(())
    }

    fn compile_while(&mut self, cond: &Spanned<Expr>, body: &Block) -> Result<()> {
        let func = self.current_function.unwrap();

        let cond_bb = self.context.append_basic_block(func, "while.cond");
        let body_bb = self.context.append_basic_block(func, "while.body");
        let after_bb = self.context.append_basic_block(func, "while.after");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition
        self.builder.position_at_end(cond_bb);
        let cond_val = self.compile_expr(&cond.node)?.into_int_value();
        self.builder
            .build_conditional_branch(cond_val, body_bb, after_bb)
            .unwrap();

        // Body
        self.builder.position_at_end(body_bb);
        self.loop_stack.push(LoopContext {
            loop_block: cond_bb,
            after_block: after_bb,
        });
        for stmt in body {
            self.compile_stmt(&stmt.node)?;
        }
        self.loop_stack.pop();
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        self.builder.position_at_end(after_bb);
        Ok(())
    }

    fn compile_for(
        &mut self,
        var: &str,
        var_type: Option<&Type>,
        iterable: &Spanned<Expr>,
        body: &Block,
    ) -> Result<()> {
        // For now, treat as range-based loop: for (i in 0..10)
        // Simplified: assume iterable is a range expression
        let func = self.current_function.unwrap();

        let ty = var_type.cloned().unwrap_or(Type::Integer);
        let var_alloca = self.builder.build_alloca(self.llvm_type(&ty), var).unwrap();

        // Initialize to start value (simplified: 0)
        let zero = self.context.i64_type().const_int(0, false);
        self.builder.build_store(var_alloca, zero).unwrap();
        self.variables.insert(
            var.to_string(),
            Variable {
                ptr: var_alloca,
                ty: ty.clone(),
            },
        );

        // Get end value from iterable
        let end_val = self.compile_expr(&iterable.node)?.into_int_value();

        let cond_bb = self.context.append_basic_block(func, "for.cond");
        let body_bb = self.context.append_basic_block(func, "for.body");
        let inc_bb = self.context.append_basic_block(func, "for.inc");
        let after_bb = self.context.append_basic_block(func, "for.after");

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        // Condition
        self.builder.position_at_end(cond_bb);
        let current = self
            .builder
            .build_load(self.context.i64_type(), var_alloca, var)
            .unwrap()
            .into_int_value();
        let cond = self
            .builder
            .build_int_compare(IntPredicate::SLT, current, end_val, "cmp")
            .unwrap();
        self.builder
            .build_conditional_branch(cond, body_bb, after_bb)
            .unwrap();

        // Body
        self.builder.position_at_end(body_bb);
        self.loop_stack.push(LoopContext {
            loop_block: inc_bb,
            after_block: after_bb,
        });
        for stmt in body {
            self.compile_stmt(&stmt.node)?;
        }
        self.loop_stack.pop();
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(inc_bb).unwrap();
        }

        // Increment
        self.builder.position_at_end(inc_bb);
        let current = self
            .builder
            .build_load(self.context.i64_type(), var_alloca, var)
            .unwrap()
            .into_int_value();
        let one = self.context.i64_type().const_int(1, false);
        let next = self.builder.build_int_add(current, one, "inc").unwrap();
        self.builder.build_store(var_alloca, next).unwrap();
        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(after_bb);
        Ok(())
    }

    fn compile_match_stmt(&mut self, expr: &Spanned<Expr>, arms: &[MatchArm]) -> Result<()> {
        let val = self.compile_expr(&expr.node)?;
        let func = self.current_function.unwrap();

        // IMPORTANT: Do NOT create merge_bb here - we create it AFTER all arm blocks
        // This ensures merge_bb is last in LLVM's block list

        // Track blocks that need to branch to merge (we'll patch them later)
        let mut blocks_needing_merge: Vec<inkwell::basic_block::BasicBlock> = Vec::new();

        for arm in arms {
            let arm_bb = self.context.append_basic_block(func, "match.arm");

            match &arm.pattern {
                Pattern::Wildcard => {
                    self.builder.build_unconditional_branch(arm_bb).unwrap();
                    self.builder.position_at_end(arm_bb);
                    for stmt in &arm.body {
                        self.compile_stmt(&stmt.node)?;
                    }
                    if self.needs_terminator() {
                        blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                    }
                    let merge_bb = self.context.append_basic_block(func, "match.merge");
                    for bb in blocks_needing_merge {
                        self.builder.position_at_end(bb);
                        if self.needs_terminator() {
                            self.builder.build_unconditional_branch(merge_bb).unwrap();
                        }
                    }
                    self.builder.position_at_end(merge_bb);
                    return Ok(());
                }

                Pattern::Ident(binding) => {
                    self.builder.build_unconditional_branch(arm_bb).unwrap();
                    self.builder.position_at_end(arm_bb);
                    let alloca = self.builder.build_alloca(val.get_type(), binding).unwrap();
                    self.builder.build_store(alloca, val).unwrap();
                    self.variables.insert(
                        binding.clone(),
                        Variable {
                            ptr: alloca,
                            ty: Type::Integer,
                        },
                    );
                    for stmt in &arm.body {
                        self.compile_stmt(&stmt.node)?;
                    }
                    if self.needs_terminator() {
                        blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                    }
                    let merge_bb = self.context.append_basic_block(func, "match.merge");
                    for bb in blocks_needing_merge {
                        self.builder.position_at_end(bb);
                        if self.needs_terminator() {
                            self.builder.build_unconditional_branch(merge_bb).unwrap();
                        }
                    }
                    self.builder.position_at_end(merge_bb);
                    return Ok(());
                }

                Pattern::Literal(lit) => {
                    let next_bb = self.context.append_basic_block(func, "match.next");
                    let pattern_val = self.compile_literal(lit)?;
                    let cond = self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            val.into_int_value(),
                            pattern_val.into_int_value(),
                            "cmp",
                        )
                        .unwrap();
                    self.builder
                        .build_conditional_branch(cond, arm_bb, next_bb)
                        .unwrap();
                    self.builder.position_at_end(arm_bb);
                    for stmt in &arm.body {
                        self.compile_stmt(&stmt.node)?;
                    }
                    if self.needs_terminator() {
                        blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                    }
                    self.builder.position_at_end(next_bb);
                }

                Pattern::Variant(variant_name, bindings) => {
                    let next_bb = self.context.append_basic_block(func, "match.next");
                    match variant_name.as_str() {
                        "Some" => {
                            let tag = self
                                .builder
                                .build_extract_value(val.into_struct_value(), 0, "tag")
                                .unwrap();
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    tag.into_int_value(),
                                    self.context.i8_type().const_int(1, false),
                                    "is_some",
                                )
                                .unwrap();
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .unwrap();
                            self.builder.position_at_end(arm_bb);
                            if !bindings.is_empty() {
                                let inner_val = self
                                    .builder
                                    .build_extract_value(val.into_struct_value(), 1, "inner")
                                    .unwrap();
                                let alloca = self
                                    .builder
                                    .build_alloca(inner_val.get_type(), &bindings[0])
                                    .unwrap();
                                self.builder.build_store(alloca, inner_val).unwrap();
                                self.variables.insert(
                                    bindings[0].clone(),
                                    Variable {
                                        ptr: alloca,
                                        ty: Type::Integer,
                                    },
                                );
                            }
                            for stmt in &arm.body {
                                self.compile_stmt(&stmt.node)?;
                            }
                            if self.needs_terminator() {
                                blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                            }
                            self.builder.position_at_end(next_bb);
                        }
                        "None" => {
                            let tag = self
                                .builder
                                .build_extract_value(val.into_struct_value(), 0, "tag")
                                .unwrap();
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    tag.into_int_value(),
                                    self.context.i8_type().const_int(0, false),
                                    "is_none",
                                )
                                .unwrap();
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .unwrap();
                            self.builder.position_at_end(arm_bb);
                            for stmt in &arm.body {
                                self.compile_stmt(&stmt.node)?;
                            }
                            if self.needs_terminator() {
                                blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                            }
                            self.builder.position_at_end(next_bb);
                        }
                        "Ok" => {
                            let tag = self
                                .builder
                                .build_extract_value(val.into_struct_value(), 0, "tag")
                                .unwrap();
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    tag.into_int_value(),
                                    self.context.i8_type().const_int(1, false),
                                    "is_ok",
                                )
                                .unwrap();
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .unwrap();
                            self.builder.position_at_end(arm_bb);
                            if !bindings.is_empty() {
                                let ok_val = self
                                    .builder
                                    .build_extract_value(val.into_struct_value(), 1, "ok")
                                    .unwrap();
                                let alloca = self
                                    .builder
                                    .build_alloca(ok_val.get_type(), &bindings[0])
                                    .unwrap();
                                self.builder.build_store(alloca, ok_val).unwrap();
                                self.variables.insert(
                                    bindings[0].clone(),
                                    Variable {
                                        ptr: alloca,
                                        ty: Type::Integer,
                                    },
                                );
                            }
                            for stmt in &arm.body {
                                self.compile_stmt(&stmt.node)?;
                            }
                            if self.needs_terminator() {
                                blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                            }
                            self.builder.position_at_end(next_bb);
                        }
                        "Error" => {
                            let tag = self
                                .builder
                                .build_extract_value(val.into_struct_value(), 0, "tag")
                                .unwrap();
                            let cond = self
                                .builder
                                .build_int_compare(
                                    IntPredicate::EQ,
                                    tag.into_int_value(),
                                    self.context.i8_type().const_int(0, false),
                                    "is_error",
                                )
                                .unwrap();
                            self.builder
                                .build_conditional_branch(cond, arm_bb, next_bb)
                                .unwrap();
                            self.builder.position_at_end(arm_bb);
                            if !bindings.is_empty() {
                                let err_val = self
                                    .builder
                                    .build_extract_value(val.into_struct_value(), 2, "err")
                                    .unwrap();
                                let alloca = self
                                    .builder
                                    .build_alloca(err_val.get_type(), &bindings[0])
                                    .unwrap();
                                self.builder.build_store(alloca, err_val).unwrap();
                                self.variables.insert(
                                    bindings[0].clone(),
                                    Variable {
                                        ptr: alloca,
                                        ty: Type::String,
                                    },
                                );
                            }
                            for stmt in &arm.body {
                                self.compile_stmt(&stmt.node)?;
                            }
                            if self.needs_terminator() {
                                blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                            }
                            self.builder.position_at_end(next_bb);
                        }
                        _ => {
                            self.builder.build_unconditional_branch(arm_bb).unwrap();
                            self.builder.position_at_end(arm_bb);
                            for stmt in &arm.body {
                                self.compile_stmt(&stmt.node)?;
                            }
                            if self.needs_terminator() {
                                blocks_needing_merge.push(self.builder.get_insert_block().unwrap());
                            }
                            let merge_bb = self.context.append_basic_block(func, "match.merge");
                            for bb in blocks_needing_merge {
                                self.builder.position_at_end(bb);
                                if self.needs_terminator() {
                                    self.builder.build_unconditional_branch(merge_bb).unwrap();
                                }
                            }
                            self.builder.position_at_end(merge_bb);
                            return Ok(());
                        }
                    }
                }
            }
        }

        // Create merge_bb AFTER all arm blocks
        let merge_bb = self.context.append_basic_block(func, "match.merge");
        if self.needs_terminator() {
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }
        for bb in blocks_needing_merge {
            self.builder.position_at_end(bb);
            if self.needs_terminator() {
                self.builder.build_unconditional_branch(merge_bb).unwrap();
            }
        }
        self.builder.position_at_end(merge_bb);
        Ok(())
    }

    // === Expressions ===

    fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Literal(lit) => self.compile_literal(lit),

            Expr::Ident(name) => {
                if let Some(var) = self.variables.get(name) {
                    let val = self
                        .builder
                        .build_load(self.llvm_type(&var.ty), var.ptr, name)
                        .unwrap();
                    Ok(val)
                } else if let Some((func, ty)) = self.functions.get(name) {
                    // Create a closure struct { fn_ptr, null_env }
                    let struct_ty = self.llvm_type(ty).into_struct_type();
                    let mut closure = struct_ty.get_undef();

                    let fn_ptr = func.as_global_value().as_pointer_value();
                    let null_env = self.context.ptr_type(AddressSpace::default()).const_null();

                    closure = self
                        .builder
                        .build_insert_value(closure, fn_ptr, 0, "fn")
                        .unwrap()
                        .into_struct_value();
                    closure = self
                        .builder
                        .build_insert_value(closure, null_env, 1, "env")
                        .unwrap()
                        .into_struct_value();

                    Ok(closure.into())
                } else {
                    Err(CodegenError::new(format!("Unknown variable: {}", name)))
                }
            }

            Expr::Binary { op, left, right } => self.compile_binary(*op, &left.node, &right.node),

            Expr::Unary { op, expr } => self.compile_unary(*op, &expr.node),

            Expr::Call { callee, args } => self.compile_call(&callee.node, args),

            Expr::Field { object, field } => self.compile_field(&object.node, field),

            Expr::Index { object, index } => self.compile_index(&object.node, &index.node),

            Expr::Construct { ty, args } => self.compile_construct(ty, args),

            Expr::This => {
                if let Some(var) = self.variables.get("this") {
                    let val = self
                        .builder
                        .build_load(
                            self.context.ptr_type(AddressSpace::default()),
                            var.ptr,
                            "this",
                        )
                        .unwrap();
                    Ok(val)
                } else {
                    Err(CodegenError::new("'this' not available"))
                }
            }

            Expr::StringInterp(parts) => self.compile_string_interp(parts),

            Expr::Lambda { params, body } => self.compile_lambda(params, body),

            Expr::Match { expr, arms } => self.compile_match_expr(&expr.node, arms),

            Expr::Try(inner) => self.compile_try(&inner.node),

            Expr::Borrow(inner) | Expr::MutBorrow(inner) => {
                // Get pointer to the expression (lvalue)
                self.compile_borrow(&inner.node)
            }

            Expr::Deref(inner) => {
                // Dereference the pointer
                self.compile_deref(&inner.node)
            }

            Expr::Await(inner) => {
                // For now, await just evaluates the expression
                // Full async runtime would require coroutine support
                self.compile_expr(&inner.node)
            }

            Expr::AsyncBlock(body) => {
                // Compile async block as regular block for now
                // Full implementation would wrap in a Task
                let mut result = self.context.i8_type().const_int(0, false).into();
                for stmt in body {
                    match &stmt.node {
                        Stmt::Return(Some(expr)) => {
                            // Just evaluate the expression, don't emit ret
                            result = self.compile_expr(&expr.node)?;
                        }
                        Stmt::Return(None) => {
                            // Return None value
                            result = self.context.i8_type().const_int(0, false).into();
                        }
                        _ => {
                            self.compile_stmt(&stmt.node)?;
                            if let Stmt::Expr(expr) = &stmt.node {
                                result = self.compile_expr(&expr.node)?;
                            }
                        }
                    }
                }
                Ok(result)
            }

            Expr::Require { condition, message } => {
                // Compile require(condition) as an assert
                let cond_val = self.compile_expr(&condition.node)?;
                let cond = cond_val.into_int_value();

                let current_fn = self
                    .current_function
                    .ok_or(CodegenError::new("require outside of function"))?;

                let assert_block = self.context.append_basic_block(current_fn, "require.ok");
                let fail_block = self.context.append_basic_block(current_fn, "require.fail");

                self.builder
                    .build_conditional_branch(cond, assert_block, fail_block)
                    .unwrap();

                // Fail block - call abort or print message
                self.builder.position_at_end(fail_block);
                if let Some(msg) = message {
                    // Print the error message
                    let msg_spanned = Spanned::new(msg.node.clone(), msg.span.clone());
                    self.compile_print(&[msg_spanned], true)?;
                }
                // Call exit(1) or abort
                if let Some(exit_fn) = self.module.get_function("exit") {
                    self.builder
                        .build_call(
                            exit_fn,
                            &[self.context.i32_type().const_int(1, false).into()],
                            "exit",
                        )
                        .unwrap();
                }
                self.builder.build_unreachable().unwrap();

                // Continue in assert block
                self.builder.position_at_end(assert_block);
                Ok(self.context.i8_type().const_int(0, false).into())
            }

            Expr::Range {
                start,
                end,
                inclusive: _,
            } => {
                // Ranges are handled specially in for loops
                // For now, return a dummy value
                let start_val = if let Some(s) = start {
                    self.compile_expr(&s.node)?
                } else {
                    self.context.i64_type().const_int(0, false).into()
                };
                let _end_val = if let Some(e) = end {
                    self.compile_expr(&e.node)?
                } else {
                    self.context.i64_type().const_int(0, false).into()
                };
                Ok(start_val)
            }

            Expr::IfExpr {
                condition,
                then_branch,
                else_branch,
            } => self.compile_if_expr(&condition.node, then_branch, else_branch.as_ref()),

            Expr::Block(body) => {
                let mut result = self.context.i8_type().const_int(0, false).into();
                for stmt in body {
                    self.compile_stmt(&stmt.node)?;
                    if let Stmt::Expr(expr) = &stmt.node {
                        result = self.compile_expr(&expr.node)?;
                    }
                }
                Ok(result)
            }
        }
    }

    fn compile_if_expr(
        &mut self,
        condition: &Expr,
        then_branch: &[Spanned<Stmt>],
        else_branch: Option<&Vec<Spanned<Stmt>>>,
    ) -> Result<BasicValueEnum<'ctx>> {
        let cond_val = self.compile_expr(condition)?;
        let cond = cond_val.into_int_value();

        let current_fn = self
            .current_function
            .ok_or(CodegenError::new("if expression outside of function"))?;

        let then_block = self.context.append_basic_block(current_fn, "if.then");
        let else_block = self.context.append_basic_block(current_fn, "if.else");
        let merge_block = self.context.append_basic_block(current_fn, "if.merge");

        self.builder
            .build_conditional_branch(cond, then_block, else_block)
            .unwrap();

        // Then branch
        self.builder.position_at_end(then_block);
        let mut then_result = self.context.i8_type().const_int(0, false).into();
        for stmt in then_branch {
            self.compile_stmt(&stmt.node)?;
            if let Stmt::Expr(expr) = &stmt.node {
                then_result = self.compile_expr(&expr.node)?;
            }
        }
        self.builder
            .build_unconditional_branch(merge_block)
            .unwrap();
        let then_block = self.builder.get_insert_block().unwrap();

        // Else branch
        self.builder.position_at_end(else_block);
        let mut else_result = self.context.i8_type().const_int(0, false).into();
        if let Some(else_stmts) = else_branch {
            for stmt in else_stmts {
                self.compile_stmt(&stmt.node)?;
                if let Stmt::Expr(expr) = &stmt.node {
                    else_result = self.compile_expr(&expr.node)?;
                }
            }
        }
        self.builder
            .build_unconditional_branch(merge_block)
            .unwrap();
        let else_block = self.builder.get_insert_block().unwrap();

        // Merge block with phi node
        self.builder.position_at_end(merge_block);
        if then_result.get_type() == else_result.get_type() {
            let phi = self
                .builder
                .build_phi(then_result.get_type(), "if.result")
                .unwrap();
            phi.add_incoming(&[(&then_result, then_block), (&else_result, else_block)]);
            Ok(phi.as_basic_value())
        } else {
            Ok(then_result)
        }
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<BasicValueEnum<'ctx>> {
        match lit {
            Literal::Integer(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),
            Literal::Float(n) => Ok(self.context.f64_type().const_float(*n).into()),
            Literal::Boolean(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),
            Literal::String(s) => {
                let str_val = self.context.const_string(s.as_bytes(), true);
                let name = format!("str.{}", self.str_counter);
                self.str_counter += 1;
                let global = self.module.add_global(str_val.get_type(), None, &name);
                global.set_initializer(&str_val);
                global.set_constant(true);
                Ok(global.as_pointer_value().into())
            }
            Literal::Char(c) => Ok(self.context.i8_type().const_int(*c as u64, false).into()),
            Literal::None => Ok(self.context.i8_type().const_int(0, false).into()),
        }
    }

    fn compile_binary(
        &mut self,
        op: BinOp,
        left: &Expr,
        right: &Expr,
    ) -> Result<BasicValueEnum<'ctx>> {
        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        // Integer operations
        if lhs.is_int_value() && rhs.is_int_value() {
            let l = lhs.into_int_value();
            let r = rhs.into_int_value();

            let result = match op {
                BinOp::Add => self.builder.build_int_add(l, r, "add").unwrap(),
                BinOp::Sub => self.builder.build_int_sub(l, r, "sub").unwrap(),
                BinOp::Mul => self.builder.build_int_mul(l, r, "mul").unwrap(),
                BinOp::Div => self.builder.build_int_signed_div(l, r, "div").unwrap(),
                BinOp::Mod => self.builder.build_int_signed_rem(l, r, "mod").unwrap(),
                BinOp::Eq => self
                    .builder
                    .build_int_compare(IntPredicate::EQ, l, r, "eq")
                    .unwrap(),
                BinOp::NotEq => self
                    .builder
                    .build_int_compare(IntPredicate::NE, l, r, "ne")
                    .unwrap(),
                BinOp::Lt => self
                    .builder
                    .build_int_compare(IntPredicate::SLT, l, r, "lt")
                    .unwrap(),
                BinOp::LtEq => self
                    .builder
                    .build_int_compare(IntPredicate::SLE, l, r, "le")
                    .unwrap(),
                BinOp::Gt => self
                    .builder
                    .build_int_compare(IntPredicate::SGT, l, r, "gt")
                    .unwrap(),
                BinOp::GtEq => self
                    .builder
                    .build_int_compare(IntPredicate::SGE, l, r, "ge")
                    .unwrap(),
                BinOp::And => self.builder.build_and(l, r, "and").unwrap(),
                BinOp::Or => self.builder.build_or(l, r, "or").unwrap(),
            };
            return Ok(result.into());
        }

        // Float operations
        if lhs.is_float_value() && rhs.is_float_value() {
            let l = lhs.into_float_value();
            let r = rhs.into_float_value();

            let result = match op {
                BinOp::Add => self.builder.build_float_add(l, r, "fadd").unwrap().into(),
                BinOp::Sub => self.builder.build_float_sub(l, r, "fsub").unwrap().into(),
                BinOp::Mul => self.builder.build_float_mul(l, r, "fmul").unwrap().into(),
                BinOp::Div => self.builder.build_float_div(l, r, "fdiv").unwrap().into(),
                BinOp::Eq => self
                    .builder
                    .build_float_compare(FloatPredicate::OEQ, l, r, "feq")
                    .unwrap()
                    .into(),
                BinOp::NotEq => self
                    .builder
                    .build_float_compare(FloatPredicate::ONE, l, r, "fne")
                    .unwrap()
                    .into(),
                BinOp::Lt => self
                    .builder
                    .build_float_compare(FloatPredicate::OLT, l, r, "flt")
                    .unwrap()
                    .into(),
                BinOp::LtEq => self
                    .builder
                    .build_float_compare(FloatPredicate::OLE, l, r, "fle")
                    .unwrap()
                    .into(),
                BinOp::Gt => self
                    .builder
                    .build_float_compare(FloatPredicate::OGT, l, r, "fgt")
                    .unwrap()
                    .into(),
                BinOp::GtEq => self
                    .builder
                    .build_float_compare(FloatPredicate::OGE, l, r, "fge")
                    .unwrap()
                    .into(),
                _ => return Err(CodegenError::new("Invalid float operation")),
            };
            return Ok(result);
        }

        Err(CodegenError::new("Type mismatch in binary operation"))
    }

    fn compile_unary(&mut self, op: UnaryOp, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let val = self.compile_expr(expr)?;

        match op {
            UnaryOp::Neg => {
                if val.is_int_value() {
                    Ok(self
                        .builder
                        .build_int_neg(val.into_int_value(), "neg")
                        .unwrap()
                        .into())
                } else if val.is_float_value() {
                    Ok(self
                        .builder
                        .build_float_neg(val.into_float_value(), "fneg")
                        .unwrap()
                        .into())
                } else {
                    Err(CodegenError::new("Cannot negate non-numeric value"))
                }
            }
            UnaryOp::Not => Ok(self
                .builder
                .build_not(val.into_int_value(), "not")
                .unwrap()
                .into()),
        }
    }

    fn compile_call(
        &mut self,
        callee: &Expr,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        // Check for built-in functions
        if let Expr::Ident(name) = callee {
            if name == "println" || name == "print" {
                return self.compile_print(args, name == "println");
            }

            // Standard library functions
            if let Some(result) = self.compile_stdlib_function(name, args)? {
                return Ok(result);
            }
        }

        // Check for Option/Result static methods
        if let Expr::Field { object, field } = callee {
            if let Expr::Ident(type_name) = &object.node {
                match (type_name.as_str(), field.as_str()) {
                    ("Option", "some") => {
                        if args.len() != 1 {
                            return Err(CodegenError::new(
                                "Option.some() requires exactly 1 argument",
                            ));
                        }
                        let val = self.compile_expr(&args[0].node)?;
                        return self.create_option_some(val);
                    }
                    ("Option", "none") => {
                        return self.create_option_none();
                    }
                    ("Result", "ok") => {
                        if args.len() != 1 {
                            return Err(CodegenError::new(
                                "Result.ok() requires exactly 1 argument",
                            ));
                        }
                        let val = self.compile_expr(&args[0].node)?;
                        return self.create_result_ok(val);
                    }
                    ("Result", "error") => {
                        if args.len() != 1 {
                            return Err(CodegenError::new(
                                "Result.error() requires exactly 1 argument",
                            ));
                        }
                        let val = self.compile_expr(&args[0].node)?;
                        return self.create_result_error(val);
                    }
                    _ => {}
                }
            }
        }

        // Method call on object
        if let Expr::Field { object, field } = callee {
            return self.compile_method_call(&object.node, field, args);
        }

        // Regular function call
        let func = match callee {
            Expr::Ident(name) => {
                // First check if it's a function pointer/local variable
                if let Some(var) = self.variables.get(name) {
                    if let Type::Function(param_types, ret_type) = &var.ty {
                        let closure_val = self
                            .builder
                            .build_load(self.llvm_type(&var.ty), var.ptr, name)
                            .unwrap()
                            .into_struct_value();

                        let ptr = self
                            .builder
                            .build_extract_value(closure_val, 0, "fn_ptr")
                            .unwrap()
                            .into_pointer_value();
                        let env_ptr = self
                            .builder
                            .build_extract_value(closure_val, 1, "env_ptr")
                            .unwrap();

                        // Construct FunctionType (including env_ptr as first arg)
                        let llvm_ret = self.llvm_type(ret_type);
                        let mut llvm_params: Vec<BasicMetadataTypeEnum> = vec![
                            self.context.ptr_type(AddressSpace::default()).into(), // env_ptr
                        ];
                        for p in param_types {
                            llvm_params.push(self.llvm_type(p).into());
                        }

                        let fn_type = match llvm_ret {
                            BasicTypeEnum::IntType(i) => i.fn_type(&llvm_params, false),
                            BasicTypeEnum::FloatType(f) => f.fn_type(&llvm_params, false),
                            BasicTypeEnum::PointerType(p) => p.fn_type(&llvm_params, false),
                            BasicTypeEnum::StructType(s) => s.fn_type(&llvm_params, false),
                            _ => {
                                // Default to i8 type for void-like returns if needed
                                self.context.i8_type().fn_type(&llvm_params, false)
                            }
                        };

                        let mut compiled_args: Vec<BasicValueEnum> = vec![env_ptr];
                        for a in args {
                            compiled_args.push(self.compile_expr(&a.node)?);
                        }

                        let args_meta: Vec<BasicMetadataValueEnum> =
                            compiled_args.iter().map(|a| (*a).into()).collect();

                        let call = self
                            .builder
                            .build_indirect_call(fn_type, ptr, &args_meta, "call")
                            .unwrap();

                        let result = match call.try_as_basic_value() {
                            ValueKind::Basic(val) => val,
                            ValueKind::Instruction(_) => {
                                self.context.i8_type().const_int(0, false).into()
                            }
                        };
                        return Ok(result);
                    }
                }

                // Fall back to global function lookup
                if let Some((f, _)) = self.functions.get(name) {
                    *f
                } else if let Some(f) = self.module.get_function(name) {
                    f
                } else {
                    return Err(CodegenError::new(format!("Unknown function: {}", name)));
                }
            }
            _ => return Err(CodegenError::new("Invalid callee")),
        };

        let mut compiled_args: Vec<BasicValueEnum> = Vec::new();
        // Add null env_ptr for direct calls (except main)
        if func.get_name().to_str().unwrap() != "main" {
            compiled_args.push(
                self.context
                    .ptr_type(AddressSpace::default())
                    .const_null()
                    .into(),
            );
        }

        for a in args {
            compiled_args.push(self.compile_expr(&a.node)?);
        }

        let args_meta: Vec<BasicMetadataValueEnum> =
            compiled_args.iter().map(|a| (*a).into()).collect();
        let call = self.builder.build_call(func, &args_meta, "call").unwrap();

        match call.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            ValueKind::Instruction(_) => Ok(self.context.i8_type().const_int(0, false).into()),
        }
    }

    fn compile_method_call(
        &mut self,
        object: &Expr,
        method: &str,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        // Infer object type first
        let obj_ty = self.infer_object_type(object);

        // Handle built-in types (List, Map, Set, Option, Result) for any expression
        if let Some(ref ty) = obj_ty {
            match ty {
                Type::List(_) => {
                    // Get pointer to the list
                    let list_ptr = match object {
                        Expr::Ident(name) => self.variables.get(name).map(|v| v.ptr),
                        Expr::Field { object: obj, field } => {
                            self.compile_field_ptr(&obj.node, field).ok()
                        }
                        Expr::This => self.variables.get("this").map(|v| v.ptr),
                        _ => None,
                    };
                    if let Some(ptr) = list_ptr {
                        return self.compile_list_method_ptr(ptr, ty, method, args);
                    }
                }
                Type::Map(_, _) => {
                    if let Expr::Ident(name) = object {
                        return self.compile_map_method(name, method, args);
                    }
                }
                Type::Set(_) => {
                    if let Expr::Ident(name) = object {
                        return self.compile_set_method(name, method, args);
                    }
                }
                Type::Option(_) => {
                    if let Expr::Ident(name) = object {
                        return self.compile_option_method(name, method, args);
                    }
                }
                Type::Result(_, _) => {
                    if let Expr::Ident(name) = object {
                        return self.compile_result_method(name, method, args);
                    }
                }
                _ => {}
            }
        }

        let obj_val = self.compile_expr(object)?;

        // Get class name from inferred type
        let class_name = obj_ty
            .as_ref()
            .and_then(|ty| self.type_to_class_name(ty))
            .ok_or_else(|| {
                CodegenError::new(format!(
                    "Cannot determine object type for method call: {:?}",
                    object
                ))
            })?;

        let _class_info = self
            .classes
            .get(&class_name)
            .ok_or_else(|| CodegenError::new(format!("Unknown class: {}", class_name)))?;

        let func_name = format!("{}__{}", class_name, method);

        let (func, _) = self
            .functions
            .get(&func_name)
            .ok_or_else(|| CodegenError::new(format!("Unknown method: {}", func_name)))?
            .clone();

        let mut compiled_args: Vec<BasicValueEnum> = vec![
            self.context
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(), // env_ptr
            obj_val, // this
        ];
        for a in args {
            compiled_args.push(self.compile_expr(&a.node)?);
        }

        let args_meta: Vec<BasicMetadataValueEnum> =
            compiled_args.iter().map(|a| (*a).into()).collect();
        let call = self.builder.build_call(func, &args_meta, "call").unwrap();

        match call.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            ValueKind::Instruction(_) => Ok(self.context.i8_type().const_int(0, false).into()),
        }
    }

    fn compile_field(&mut self, object: &Expr, field: &str) -> Result<BasicValueEnum<'ctx>> {
        let obj_ptr = self.compile_expr(object)?.into_pointer_value();

        // Get class name using type inference
        let obj_ty = self.infer_object_type(object);
        let class_name = obj_ty
            .as_ref()
            .and_then(|ty| self.type_to_class_name(ty))
            .ok_or_else(|| {
                CodegenError::new(format!(
                    "Cannot determine object type for field access: {:?}.{}",
                    object, field
                ))
            })?;

        let class_info = self
            .classes
            .get(&class_name)
            .ok_or_else(|| CodegenError::new(format!("Unknown class: {}", class_name)))?;

        let field_idx = *class_info
            .field_indices
            .get(field)
            .ok_or_else(|| CodegenError::new(format!("Unknown field: {}", field)))?;

        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let idx = i32_type.const_int(field_idx as u64, false);

        let field_ptr = unsafe {
            self.builder
                .build_gep(
                    class_info.struct_type.as_basic_type_enum(),
                    obj_ptr,
                    &[zero, idx],
                    field,
                )
                .unwrap()
        };

        let field_type = class_info
            .struct_type
            .get_field_type_at_index(field_idx)
            .unwrap();
        Ok(self
            .builder
            .build_load(field_type, field_ptr, field)
            .unwrap())
    }

    /// Get pointer to a field (for in-place modifications on collections)
    fn compile_field_ptr(&mut self, object: &Expr, field: &str) -> Result<PointerValue<'ctx>> {
        let obj_ptr = self.compile_expr(object)?.into_pointer_value();

        let obj_ty = self.infer_object_type(object);
        let class_name = obj_ty
            .as_ref()
            .and_then(|ty| self.type_to_class_name(ty))
            .ok_or_else(|| CodegenError::new("Cannot determine object type for field ptr"))?;

        let class_info = self
            .classes
            .get(&class_name)
            .ok_or_else(|| CodegenError::new(format!("Unknown class: {}", class_name)))?;

        let field_idx = *class_info
            .field_indices
            .get(field)
            .ok_or_else(|| CodegenError::new(format!("Unknown field: {}", field)))?;

        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let idx = i32_type.const_int(field_idx as u64, false);

        let field_ptr = unsafe {
            self.builder
                .build_gep(
                    class_info.struct_type.as_basic_type_enum(),
                    obj_ptr,
                    &[zero, idx],
                    field,
                )
                .unwrap()
        };

        Ok(field_ptr)
    }

    fn compile_index(&mut self, object: &Expr, index: &Expr) -> Result<BasicValueEnum<'ctx>> {
        let obj_ptr = self.compile_expr(object)?.into_pointer_value();
        let idx = self.compile_expr(index)?.into_int_value();

        let elem_ptr = unsafe {
            self.builder
                .build_gep(self.context.i64_type(), obj_ptr, &[idx], "elem")
                .unwrap()
        };

        Ok(self
            .builder
            .build_load(self.context.i64_type(), elem_ptr, "load")
            .unwrap())
    }

    fn compile_construct(
        &mut self,
        ty: &str,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        // Handle List<T> construction
        if ty == "List" || ty.starts_with("List<") {
            return self.create_empty_list();
        }

        // Handle Map<K,V> construction
        if ty == "Map" || ty.starts_with("Map<") {
            return self.create_empty_map();
        }

        // Handle Option<T> construction (default to None)
        if ty == "Option" || ty.starts_with("Option<") {
            return self.create_option_none();
        }

        // Handle Result<T,E> construction (default to Error with zeroed memory)
        if ty == "Result" || ty.starts_with("Result<") {
            return self.create_default_result();
        }

        // Handle Set<T> construction
        if ty == "Set" || ty.starts_with("Set<") {
            return self.create_empty_set();
        }

        // Handle Smart Pointer construction
        if ty == "Box" || ty.starts_with("Box<") {
            return self.create_empty_box();
        }
        if ty == "Rc" || ty.starts_with("Rc<") {
            return self.create_empty_rc();
        }
        if ty == "Arc" || ty.starts_with("Arc<") {
            return self.create_empty_arc();
        }

        let func_name = format!("{}__new", ty);

        let (func, _) = self
            .functions
            .get(&func_name)
            .ok_or_else(|| CodegenError::new(format!("Unknown type: {}", ty)))?
            .clone();

        let mut compiled_args: Vec<BasicValueEnum> = vec![
            self.context
                .ptr_type(AddressSpace::default())
                .const_null()
                .into(), // env_ptr
        ];
        for a in args {
            compiled_args.push(self.compile_expr(&a.node)?);
        }

        let args_meta: Vec<BasicMetadataValueEnum> =
            compiled_args.iter().map(|a| (*a).into()).collect();
        let call = self.builder.build_call(func, &args_meta, "new").unwrap();

        match call.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            _ => panic!("Constructor should return a value"),
        }
    }

    fn compile_print(
        &mut self,
        args: &[Spanned<Expr>],
        newline: bool,
    ) -> Result<BasicValueEnum<'ctx>> {
        let printf = self.get_or_declare_printf();

        for arg in args {
            let val = self.compile_expr(&arg.node)?;

            let (fmt, print_args): (&str, Vec<BasicMetadataValueEnum>) = if val.is_int_value() {
                ("%lld", vec![val.into()])
            } else if val.is_float_value() {
                ("%f", vec![val.into()])
            } else {
                ("%s", vec![val.into()])
            };

            let fmt_str = self.context.const_string(fmt.as_bytes(), true);
            let fmt_name = format!("fmt.{}", self.str_counter);
            self.str_counter += 1;
            let fmt_global = self.module.add_global(fmt_str.get_type(), None, &fmt_name);
            fmt_global.set_initializer(&fmt_str);

            let mut call_args: Vec<BasicMetadataValueEnum> =
                vec![fmt_global.as_pointer_value().into()];
            call_args.extend(print_args);

            self.builder
                .build_call(printf, &call_args, "printf")
                .unwrap();
        }

        if newline {
            let nl_str = self.context.const_string(b"\n", true);
            let nl_name = format!("nl.{}", self.str_counter);
            self.str_counter += 1;
            let nl_global = self.module.add_global(nl_str.get_type(), None, &nl_name);
            nl_global.set_initializer(&nl_str);
            self.builder
                .build_call(printf, &[nl_global.as_pointer_value().into()], "printf")
                .unwrap();
        }

        Ok(self.context.i32_type().const_int(0, false).into())
    }

    fn compile_string_interp(&mut self, parts: &[StringPart]) -> Result<BasicValueEnum<'ctx>> {
        // Build format string and collect arguments
        let mut fmt_str = String::new();
        let mut args: Vec<BasicMetadataValueEnum> = Vec::new();

        for part in parts {
            match part {
                StringPart::Literal(s) => {
                    // Escape % characters for printf
                    fmt_str.push_str(&s.replace('%', "%%"));
                }
                StringPart::Expr(expr) => {
                    let val = self.compile_expr(&expr.node)?;
                    if val.is_int_value() {
                        fmt_str.push_str("%lld");
                        args.push(val.into());
                    } else if val.is_float_value() {
                        fmt_str.push_str("%f");
                        args.push(val.into());
                    } else {
                        fmt_str.push_str("%s");
                        args.push(val.into());
                    }
                }
            }
        }

        // Allocate buffer for result (simplified: fixed size)
        let sprintf = self.get_or_declare_sprintf();
        let malloc = self.get_or_declare_malloc();

        let buffer_size = self.context.i64_type().const_int(4096, false);
        let buffer_call = self
            .builder
            .build_call(malloc, &[buffer_size.into()], "strbuf")
            .unwrap();
        let buffer = match buffer_call.try_as_basic_value() {
            ValueKind::Basic(val) => val.into_pointer_value(),
            _ => panic!("malloc should return a value"),
        };

        // Create format string
        let fmt_val = self.context.const_string(fmt_str.as_bytes(), true);
        let fmt_name = format!("fmt.{}", self.str_counter);
        self.str_counter += 1;
        let fmt_global = self.module.add_global(fmt_val.get_type(), None, &fmt_name);
        fmt_global.set_initializer(&fmt_val);

        // Call sprintf
        let mut sprintf_args: Vec<BasicMetadataValueEnum> =
            vec![buffer.into(), fmt_global.as_pointer_value().into()];
        sprintf_args.extend(args);
        self.builder
            .build_call(sprintf, &sprintf_args, "sprintf")
            .unwrap();

        Ok(buffer.into())
    }

    fn get_or_declare_sprintf(&mut self) -> FunctionValue<'ctx> {
        let name = "sprintf";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }

        let sprintf_type = self.context.i32_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            true,
        );
        self.module.add_function(name, sprintf_type, None)
    }

    // === Set<T> methods ===

    fn compile_set_method(
        &mut self,
        set_name: &str,
        method: &str,
        _args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = self.variables.get(set_name).unwrap();
        let set_ptr = var.ptr;
        let set_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        match method {
            "length" => {
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            set_type.as_basic_type_enum(),
                            set_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap();
                Ok(length)
            }
            "add" | "contains" | "remove" => {
                // Stubs for now
                Ok(self.context.bool_type().const_int(0, false).into())
            }
            _ => Err(CodegenError::new(format!("Unknown Set method: {}", method))),
        }
    }

    // === Option<T> methods ===

    fn compile_option_method(
        &mut self,
        option_name: &str,
        method: &str,
        _args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = self.variables.get(option_name).unwrap();
        let option_ptr = var.ptr;
        // Assuming Option<T> is { is_some: i8, value: T }
        // We need to infer T from var.ty
        let option_ty = match &var.ty {
            Type::Option(inner_ty) => inner_ty,
            _ => return Err(CodegenError::new("Expected Option type")),
        };
        let llvm_inner_ty = self.llvm_type(option_ty);

        let option_struct_type = self
            .context
            .struct_type(&[self.context.i8_type().into(), llvm_inner_ty], false);

        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);

        match method {
            "is_some" => {
                let is_some_ptr = unsafe {
                    self.builder
                        .build_gep(
                            option_struct_type.as_basic_type_enum(),
                            option_ptr,
                            &[zero, i32_type.const_int(0, false)],
                            "is_some_ptr",
                        )
                        .unwrap()
                };
                let is_some = self
                    .builder
                    .build_load(self.context.i8_type(), is_some_ptr, "is_some")
                    .unwrap();
                Ok(is_some)
            }
            "is_none" => {
                let is_some_ptr = unsafe {
                    self.builder
                        .build_gep(
                            option_struct_type.as_basic_type_enum(),
                            option_ptr,
                            &[zero, i32_type.const_int(0, false)],
                            "is_some_ptr",
                        )
                        .unwrap()
                };
                let is_some = self
                    .builder
                    .build_load(self.context.i8_type(), is_some_ptr, "is_some")
                    .unwrap()
                    .into_int_value();
                let is_none = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        is_some,
                        self.context.i8_type().const_int(0, false),
                        "is_none",
                    )
                    .unwrap();
                Ok(is_none.into())
            }
            "unwrap" => {
                // For now, just return the value without checking if it's Some
                // A proper implementation would add a runtime check and panic if None
                let value_ptr = unsafe {
                    self.builder
                        .build_gep(
                            option_struct_type.as_basic_type_enum(),
                            option_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "value_ptr",
                        )
                        .unwrap()
                };
                let value = self
                    .builder
                    .build_load(llvm_inner_ty, value_ptr, "unwrapped_value")
                    .unwrap();
                Ok(value)
            }
            _ => Err(CodegenError::new(format!(
                "Unknown Option method: {}",
                method
            ))),
        }
    }

    // === Result<T, E> methods ===

    fn compile_result_method(
        &mut self,
        result_name: &str,
        method: &str,
        _args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = self.variables.get(result_name).unwrap();
        let result_ptr = var.ptr;
        // Result<T, E> is struct { is_ok: i8, ok_value: T, err_value: E }
        let (ok_ty, err_ty) = match &var.ty {
            Type::Result(ok, err) => (ok, err),
            _ => return Err(CodegenError::new("Expected Result type")),
        };
        let ok_llvm = self.llvm_type(ok_ty);
        let err_llvm = self.llvm_type(err_ty);

        let result_struct_type = self
            .context
            .struct_type(&[self.context.i8_type().into(), ok_llvm, err_llvm], false);

        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);

        match method {
            "is_ok" => {
                let tag_ptr = unsafe {
                    self.builder
                        .build_gep(
                            result_struct_type.as_basic_type_enum(),
                            result_ptr,
                            &[zero, i32_type.const_int(0, false)],
                            "tag_ptr",
                        )
                        .unwrap()
                };
                let tag = self
                    .builder
                    .build_load(self.context.i8_type(), tag_ptr, "tag")
                    .unwrap();
                Ok(tag)
            }
            "is_error" => {
                let tag_ptr = unsafe {
                    self.builder
                        .build_gep(
                            result_struct_type.as_basic_type_enum(),
                            result_ptr,
                            &[zero, i32_type.const_int(0, false)],
                            "tag_ptr",
                        )
                        .unwrap()
                };
                let tag = self
                    .builder
                    .build_load(self.context.i8_type(), tag_ptr, "tag")
                    .unwrap()
                    .into_int_value();
                let is_error = self
                    .builder
                    .build_int_compare(
                        IntPredicate::EQ,
                        tag,
                        self.context.i8_type().const_int(0, false),
                        "is_error",
                    )
                    .unwrap();
                Ok(is_error.into())
            }
            "unwrap" => {
                // Returns Ok value, panics if Error (runtime check omitted for now)
                let ok_ptr = unsafe {
                    self.builder
                        .build_gep(
                            result_struct_type.as_basic_type_enum(),
                            result_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "ok_ptr",
                        )
                        .unwrap()
                };
                let value = self
                    .builder
                    .build_load(ok_llvm, ok_ptr, "unwrapped_ok")
                    .unwrap();
                Ok(value)
            }
            _ => Err(CodegenError::new(format!(
                "Unknown Result method: {}",
                method
            ))),
        }
    }

    // === Option<T> helpers ===

    fn create_option_some(&mut self, value: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>> {
        // Option is struct { is_some: i8, value: T }
        let option_type = self
            .context
            .struct_type(&[self.context.i8_type().into(), value.get_type()], false);

        let alloca = self.builder.build_alloca(option_type, "option").unwrap();

        // Set is_some = 1
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let tag_ptr = unsafe {
            self.builder
                .build_gep(
                    option_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "tag",
                )
                .unwrap()
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(1, false))
            .unwrap();

        // Set value
        let value_ptr = unsafe {
            self.builder
                .build_gep(
                    option_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "value",
                )
                .unwrap()
        };
        self.builder.build_store(value_ptr, value).unwrap();

        Ok(self
            .builder
            .build_load(option_type, alloca, "option")
            .unwrap())
    }

    fn create_option_none(&mut self) -> Result<BasicValueEnum<'ctx>> {
        // Option<i64> as default - struct { is_some: i8, value: i64 }
        let option_type = self.context.struct_type(
            &[
                self.context.i8_type().into(),
                self.context.i64_type().into(),
            ],
            false,
        );

        let alloca = self.builder.build_alloca(option_type, "option").unwrap();

        // Set is_some = 0
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let tag_ptr = unsafe {
            self.builder
                .build_gep(
                    option_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "tag",
                )
                .unwrap()
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(0, false))
            .unwrap();

        // Set value to 0 (unused)
        let value_ptr = unsafe {
            self.builder
                .build_gep(
                    option_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "value",
                )
                .unwrap()
        };
        self.builder
            .build_store(value_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        Ok(self
            .builder
            .build_load(option_type, alloca, "option")
            .unwrap())
    }

    // === Result<T, E> helpers ===

    fn create_result_ok(&mut self, value: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>> {
        // Result is struct { is_ok: i8, ok_value: T, err_value: ptr }
        let result_type = self.context.struct_type(
            &[
                self.context.i8_type().into(),
                value.get_type(),
                self.context.ptr_type(AddressSpace::default()).into(), // error as string ptr
            ],
            false,
        );

        let alloca = self.builder.build_alloca(result_type, "result").unwrap();

        // Set is_ok = 1
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let tag_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "tag",
                )
                .unwrap()
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(1, false))
            .unwrap();

        // Set ok_value
        let ok_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "ok",
                )
                .unwrap()
        };
        self.builder.build_store(ok_ptr, value).unwrap();

        // Set err_value to null
        let err_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "err",
                )
                .unwrap()
        };
        let null = self.context.ptr_type(AddressSpace::default()).const_null();
        self.builder.build_store(err_ptr, null).unwrap();

        Ok(self
            .builder
            .build_load(result_type, alloca, "result")
            .unwrap())
    }

    fn create_result_error(&mut self, error: BasicValueEnum<'ctx>) -> Result<BasicValueEnum<'ctx>> {
        // Result is struct { is_ok: i8, ok_value: i64, err_value: ptr }
        let result_type = self.context.struct_type(
            &[
                self.context.i8_type().into(),
                self.context.i64_type().into(), // default ok type
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let alloca = self.builder.build_alloca(result_type, "result").unwrap();

        // Set is_ok = 0
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let tag_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "tag",
                )
                .unwrap()
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(0, false))
            .unwrap();

        // Set ok_value to 0
        let ok_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "ok",
                )
                .unwrap()
        };
        self.builder
            .build_store(ok_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        // Set err_value
        let err_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "err",
                )
                .unwrap()
        };
        self.builder.build_store(err_ptr, error).unwrap();

        Ok(self
            .builder
            .build_load(result_type, alloca, "result")
            .unwrap())
    }

    fn create_default_result(&mut self) -> Result<BasicValueEnum<'ctx>> {
        // Result is struct { is_ok: i8, ok_value: i64, err_value: ptr }
        // We default to Error (tag=0) with null pointer
        let result_type = self.context.struct_type(
            &[
                self.context.i8_type().into(),
                self.context.i64_type().into(), // default ok type
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let alloca = self
            .builder
            .build_alloca(result_type, "default_result")
            .unwrap();

        // Set is_ok = 0
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let tag_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "tag",
                )
                .unwrap()
        };
        self.builder
            .build_store(tag_ptr, self.context.i8_type().const_int(0, false))
            .unwrap();

        // Set ok_value to 0
        let ok_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "ok",
                )
                .unwrap()
        };
        self.builder
            .build_store(ok_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        // Set err_value to null
        let err_ptr = unsafe {
            self.builder
                .build_gep(
                    result_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "err",
                )
                .unwrap()
        };
        let null = self.context.ptr_type(AddressSpace::default()).const_null();
        self.builder.build_store(err_ptr, null).unwrap();

        Ok(self
            .builder
            .build_load(result_type, alloca, "result")
            .unwrap())
    }

    // === List<T> helpers ===

    fn create_empty_list(&mut self) -> Result<BasicValueEnum<'ctx>> {
        // List struct: { capacity: i64, length: i64, data: ptr }
        let list_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let alloca = self.builder.build_alloca(list_type, "list").unwrap();

        // Initial capacity = 8
        let initial_capacity: u64 = 8;
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let capacity_ptr = unsafe {
            self.builder
                .build_gep(
                    list_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "capacity",
                )
                .unwrap()
        };
        self.builder
            .build_store(
                capacity_ptr,
                self.context.i64_type().const_int(initial_capacity, false),
            )
            .unwrap();

        // Length = 0
        let length_ptr = unsafe {
            self.builder
                .build_gep(
                    list_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "length",
                )
                .unwrap()
        };
        self.builder
            .build_store(length_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        // Allocate data - malloc(capacity * 8) for i64 elements
        let malloc = self.get_or_declare_malloc();
        let size = self
            .context
            .i64_type()
            .const_int(initial_capacity * 8, false);
        let call_result = self
            .builder
            .build_call(malloc, &[size.into()], "data")
            .unwrap();
        let data_ptr = match call_result.try_as_basic_value() {
            ValueKind::Basic(val) => val,
            _ => panic!("malloc should return a value"),
        };

        let data_ptr_field = unsafe {
            self.builder
                .build_gep(
                    list_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "data_ptr",
                )
                .unwrap()
        };
        self.builder.build_store(data_ptr_field, data_ptr).unwrap();

        Ok(self.builder.build_load(list_type, alloca, "list").unwrap())
    }

    fn get_or_declare_malloc(&mut self) -> FunctionValue<'ctx> {
        let name = "malloc";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }

        let malloc_type = self
            .context
            .ptr_type(AddressSpace::default())
            .fn_type(&[self.context.i64_type().into()], false);
        self.module.add_function(name, malloc_type, None)
    }

    fn get_or_declare_realloc(&mut self) -> FunctionValue<'ctx> {
        let name = "realloc";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }

        let realloc_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i64_type().into(),
            ],
            false,
        );
        self.module.add_function(name, realloc_type, None)
    }

    // === Map<K,V> helpers ===

    fn create_empty_map(&mut self) -> Result<BasicValueEnum<'ctx>> {
        // Map struct: { capacity: i64, length: i64, keys: ptr, values: ptr }
        let map_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let alloca = self.builder.build_alloca(map_type, "map").unwrap();

        // Initial capacity = 8
        let initial_capacity: u64 = 8;
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let capacity_ptr = unsafe {
            self.builder
                .build_gep(
                    map_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "capacity",
                )
                .unwrap()
        };
        self.builder
            .build_store(
                capacity_ptr,
                self.context.i64_type().const_int(initial_capacity, false),
            )
            .unwrap();

        // Length = 0
        let length_ptr = unsafe {
            self.builder
                .build_gep(
                    map_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "length",
                )
                .unwrap()
        };
        self.builder
            .build_store(length_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        // Allocate keys and values arrays
        let malloc = self.get_or_declare_malloc();
        let size = self
            .context
            .i64_type()
            .const_int(initial_capacity * 8, false);

        let keys_call = self
            .builder
            .build_call(malloc, &[size.into()], "keys")
            .unwrap();
        let keys_ptr = match keys_call.try_as_basic_value() {
            ValueKind::Basic(val) => val,
            _ => panic!("malloc should return a value"),
        };
        let keys_field = unsafe {
            self.builder
                .build_gep(
                    map_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "keys_ptr",
                )
                .unwrap()
        };
        self.builder.build_store(keys_field, keys_ptr).unwrap();

        let values_call = self
            .builder
            .build_call(malloc, &[size.into()], "values")
            .unwrap();
        let values_ptr = match values_call.try_as_basic_value() {
            ValueKind::Basic(val) => val,
            _ => panic!("malloc should return a value"),
        };
        let values_field = unsafe {
            self.builder
                .build_gep(
                    map_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(3, false)],
                    "values_ptr",
                )
                .unwrap()
        };
        self.builder.build_store(values_field, values_ptr).unwrap();

        Ok(self.builder.build_load(map_type, alloca, "map").unwrap())
    }

    fn create_empty_set(&mut self) -> Result<BasicValueEnum<'ctx>> {
        // Set struct: { capacity: i64, length: i64, data: ptr }
        let set_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        let alloca = self.builder.build_alloca(set_type, "set").unwrap();

        // Initial capacity = 8
        let initial_capacity: u64 = 8;
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let capacity_ptr = unsafe {
            self.builder
                .build_gep(
                    set_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(0, false)],
                    "capacity",
                )
                .unwrap()
        };
        self.builder
            .build_store(
                capacity_ptr,
                self.context.i64_type().const_int(initial_capacity, false),
            )
            .unwrap();

        // Length = 0
        let length_ptr = unsafe {
            self.builder
                .build_gep(
                    set_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(1, false)],
                    "length",
                )
                .unwrap()
        };
        self.builder
            .build_store(length_ptr, self.context.i64_type().const_int(0, false))
            .unwrap();

        // Allocate data - malloc(capacity * 8)
        let malloc = self.get_or_declare_malloc();
        let size = self
            .context
            .i64_type()
            .const_int(initial_capacity * 8, false);
        let call_result = self
            .builder
            .build_call(malloc, &[size.into()], "data")
            .unwrap();
        let data_ptr = match call_result.try_as_basic_value() {
            ValueKind::Basic(val) => val,
            _ => panic!("malloc should return a value"),
        };

        let data_ptr_field = unsafe {
            self.builder
                .build_gep(
                    set_type.as_basic_type_enum(),
                    alloca,
                    &[zero, i32_type.const_int(2, false)],
                    "data_ptr",
                )
                .unwrap()
        };
        self.builder.build_store(data_ptr_field, data_ptr).unwrap();

        Ok(self.builder.build_load(set_type, alloca, "set").unwrap())
    }

    fn create_empty_box(&mut self) -> Result<BasicValueEnum<'ctx>> {
        let malloc = self.get_or_declare_malloc();
        let size = self.context.i64_type().const_int(8, false);
        let call_result = self
            .builder
            .build_call(malloc, &[size.into()], "box")
            .unwrap();
        match call_result.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            _ => panic!("malloc should return a value"),
        }
    }

    fn create_empty_rc(&mut self) -> Result<BasicValueEnum<'ctx>> {
        let malloc = self.get_or_declare_malloc();
        let size = self.context.i64_type().const_int(16, false); // refcount + data
        let call_result = self
            .builder
            .build_call(malloc, &[size.into()], "rc")
            .unwrap();
        match call_result.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            _ => panic!("malloc should return a value"),
        }
    }

    fn create_empty_arc(&mut self) -> Result<BasicValueEnum<'ctx>> {
        let malloc = self.get_or_declare_malloc();
        let size = self.context.i64_type().const_int(16, false); // atomic refcount + data
        let call_result = self
            .builder
            .build_call(malloc, &[size.into()], "arc")
            .unwrap();
        match call_result.try_as_basic_value() {
            ValueKind::Basic(val) => Ok(val),
            _ => panic!("malloc should return a value"),
        }
    }

    fn compile_list_method(
        &mut self,
        list_name: &str,
        method: &str,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = self.variables.get(list_name).unwrap();
        let list_ptr = var.ptr;
        let list_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        match method {
            "push" => {
                // Get current length and capacity
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap()
                    .into_int_value();

                // Get data pointer
                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                // Calculate element pointer: data + length * 8
                let offset = self
                    .builder
                    .build_int_mul(
                        length,
                        self.context.i64_type().const_int(8, false),
                        "offset",
                    )
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                // Store the value
                let value = self.compile_expr(&args[0].node)?;
                self.builder.build_store(elem_ptr, value).unwrap();

                // Increment length
                let new_length = self
                    .builder
                    .build_int_add(
                        length,
                        self.context.i64_type().const_int(1, false),
                        "new_len",
                    )
                    .unwrap();
                self.builder.build_store(length_ptr, new_length).unwrap();

                Ok(self.context.i8_type().const_int(0, false).into())
            }
            "get" => {
                // Get data pointer
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                // Calculate element pointer: data + index * 8
                let index = self.compile_expr(&args[0].node)?.into_int_value();
                let offset = self
                    .builder
                    .build_int_mul(index, self.context.i64_type().const_int(8, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                // Load and return the value
                let val = self
                    .builder
                    .build_load(self.context.i64_type(), elem_ptr, "val")
                    .unwrap();
                Ok(val)
            }
            "length" => {
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap();
                Ok(length)
            }
            "set" => {
                // Get data pointer
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                // Calculate element pointer
                let index = self.compile_expr(&args[0].node)?.into_int_value();
                let offset = self
                    .builder
                    .build_int_mul(index, self.context.i64_type().const_int(8, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                // Store the value
                let value = self.compile_expr(&args[1].node)?;
                self.builder.build_store(elem_ptr, value).unwrap();

                Ok(self.context.i8_type().const_int(0, false).into())
            }
            _ => Err(CodegenError::new(format!(
                "Unknown List method: {}",
                method
            ))),
        }
    }

    /// Compile List method call with pointer (for non-identifier expressions like this.items)
    fn compile_list_method_ptr(
        &mut self,
        list_ptr: PointerValue<'ctx>,
        _list_ty: &Type,
        method: &str,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let list_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);

        match method {
            "push" => {
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap()
                    .into_int_value();

                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                let offset = self
                    .builder
                    .build_int_mul(
                        length,
                        self.context.i64_type().const_int(8, false),
                        "offset",
                    )
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                let value = self.compile_expr(&args[0].node)?;
                self.builder.build_store(elem_ptr, value).unwrap();

                let new_length = self
                    .builder
                    .build_int_add(
                        length,
                        self.context.i64_type().const_int(1, false),
                        "new_len",
                    )
                    .unwrap();
                self.builder.build_store(length_ptr, new_length).unwrap();

                Ok(self.context.i8_type().const_int(0, false).into())
            }
            "length" => {
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap();
                Ok(length)
            }
            "get" => {
                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                let index = self.compile_expr(&args[0].node)?.into_int_value();
                let offset = self
                    .builder
                    .build_int_mul(index, self.context.i64_type().const_int(8, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                let val = self
                    .builder
                    .build_load(self.context.i64_type(), elem_ptr, "val")
                    .unwrap();
                Ok(val)
            }
            "set" => {
                let data_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            list_type.as_basic_type_enum(),
                            list_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "data_ptr_ptr",
                        )
                        .unwrap()
                };
                let data_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        data_ptr_ptr,
                        "data",
                    )
                    .unwrap()
                    .into_pointer_value();

                let index = self.compile_expr(&args[0].node)?.into_int_value();
                let offset = self
                    .builder
                    .build_int_mul(index, self.context.i64_type().const_int(8, false), "offset")
                    .unwrap();
                let elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), data_ptr, &[offset], "elem_ptr")
                        .unwrap()
                };

                // Store the value
                let value = self.compile_expr(&args[1].node)?;
                self.builder.build_store(elem_ptr, value).unwrap();

                Ok(self.context.i8_type().const_int(0, false).into())
            }
            _ => Err(CodegenError::new(format!(
                "Unknown List method: {}",
                method
            ))),
        }
    }

    fn compile_map_method(
        &mut self,
        map_name: &str,
        method: &str,
        args: &[Spanned<Expr>],
    ) -> Result<BasicValueEnum<'ctx>> {
        let var = self.variables.get(map_name).unwrap();
        let map_ptr = var.ptr;
        let map_type = self.context.struct_type(
            &[
                self.context.i64_type().into(),
                self.context.i64_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );

        match method {
            "length" => {
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap();
                Ok(length)
            }
            "set" => {
                // Simple implementation: append key-value at end (no duplicate check)
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap()
                    .into_int_value();

                let keys_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "keys_ptr_ptr",
                        )
                        .unwrap()
                };
                let keys_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        keys_ptr_ptr,
                        "keys",
                    )
                    .unwrap()
                    .into_pointer_value();

                let values_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(3, false)],
                            "vals_ptr_ptr",
                        )
                        .unwrap()
                };
                let values_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        values_ptr_ptr,
                        "vals",
                    )
                    .unwrap()
                    .into_pointer_value();

                let offset = self
                    .builder
                    .build_int_mul(
                        length,
                        self.context.i64_type().const_int(8, false),
                        "offset",
                    )
                    .unwrap();

                // Store key
                let key_elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), keys_ptr, &[offset], "key_ptr")
                        .unwrap()
                };
                let key = self.compile_expr(&args[0].node)?;
                self.builder.build_store(key_elem_ptr, key).unwrap();

                // Store value
                let val_elem_ptr = unsafe {
                    self.builder
                        .build_gep(self.context.i8_type(), values_ptr, &[offset], "val_ptr")
                        .unwrap()
                };
                let value = self.compile_expr(&args[1].node)?;
                self.builder.build_store(val_elem_ptr, value).unwrap();

                // Increment length
                let new_length = self
                    .builder
                    .build_int_add(
                        length,
                        self.context.i64_type().const_int(1, false),
                        "new_len",
                    )
                    .unwrap();
                self.builder.build_store(length_ptr, new_length).unwrap();

                Ok(self.context.i8_type().const_int(0, false).into())
            }
            "insert" => {
                // Alias for set
                self.compile_map_method(map_name, "set", args)
            }
            "get" => {
                // Simple linear search
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let length_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(1, false)],
                            "len_ptr",
                        )
                        .unwrap()
                };
                let _length = self
                    .builder
                    .build_load(self.context.i64_type(), length_ptr, "len")
                    .unwrap();

                let keys_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(2, false)],
                            "keys_ptr_ptr",
                        )
                        .unwrap()
                };
                let _keys_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        keys_ptr_ptr,
                        "keys",
                    )
                    .unwrap();

                let values_ptr_ptr = unsafe {
                    self.builder
                        .build_gep(
                            map_type.as_basic_type_enum(),
                            map_ptr,
                            &[zero, i32_type.const_int(3, false)],
                            "vals_ptr_ptr",
                        )
                        .unwrap()
                };
                let _values_ptr = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        values_ptr_ptr,
                        "vals",
                    )
                    .unwrap();

                // For now, just return 0 - proper implementation would need loops
                // TODO: Implement proper key search
                Ok(self.context.i64_type().const_int(0, false).into())
            }
            "contains" => {
                // For now, just return false
                Ok(self.context.bool_type().const_int(0, false).into())
            }
            _ => Err(CodegenError::new(format!("Unknown Map method: {}", method))),
        }
    }

    // === Try operator (?) ===

    fn compile_try(&mut self, inner: &Expr) -> Result<BasicValueEnum<'ctx>> {
        // Get current function and return type
        let function = self
            .current_function
            .ok_or_else(|| CodegenError::new("? operator used outside function"))?;
        let return_type = self
            .current_return_type
            .clone()
            .ok_or_else(|| CodegenError::new("? operator used outside function"))?;

        // Compile the inner expression (should be Option<T> or Result<T, E>)
        let value = self.compile_expr(inner)?;
        let struct_val = value.into_struct_value();

        // Extract the tag (field 0): 0 = None/Error, 1 = Some/Ok
        let tag = self
            .builder
            .build_extract_value(struct_val, 0, "tag")
            .unwrap();
        let tag_int = tag.into_int_value();

        // Compare tag with 1 (Some/Ok)
        let is_some_or_ok = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                tag_int,
                self.context.i8_type().const_int(1, false),
                "is_some_or_ok",
            )
            .unwrap();

        // Create basic blocks
        let success_block = self.context.append_basic_block(function, "try.success");
        let error_block = self.context.append_basic_block(function, "try.error");
        let merge_block = self.context.append_basic_block(function, "try.merge");

        // Branch based on tag
        self.builder
            .build_conditional_branch(is_some_or_ok, success_block, error_block)
            .unwrap();

        // Error block: return early with None/Error
        self.builder.position_at_end(error_block);
        match &return_type {
            Type::Option(inner_ty) => {
                // Return None - create Option with tag = 0
                let inner_llvm = self.llvm_type(inner_ty);
                let option_type = self
                    .context
                    .struct_type(&[self.context.i8_type().into(), inner_llvm], false);
                let alloca = self.builder.build_alloca(option_type, "none_ret").unwrap();
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let tag_ptr = unsafe {
                    self.builder
                        .build_gep(
                            option_type.as_basic_type_enum(),
                            alloca,
                            &[zero, i32_type.const_int(0, false)],
                            "tag",
                        )
                        .unwrap()
                };
                self.builder
                    .build_store(tag_ptr, self.context.i8_type().const_int(0, false))
                    .unwrap();
                let loaded = self.builder.build_load(option_type, alloca, "ret").unwrap();
                self.builder.build_return(Some(&loaded)).unwrap();
            }
            Type::Result(ok_ty, err_ty) => {
                // Return Error - propagate the error from the inner Result
                let ok_llvm = self.llvm_type(ok_ty);
                let err_llvm = self.llvm_type(err_ty);
                let result_type = self
                    .context
                    .struct_type(&[self.context.i8_type().into(), ok_llvm, err_llvm], false);

                // Extract error value from inner and build new Error result
                let err_val = self
                    .builder
                    .build_extract_value(struct_val, 2, "err_val")
                    .unwrap();
                let alloca = self.builder.build_alloca(result_type, "err_ret").unwrap();
                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let tag_ptr = unsafe {
                    self.builder
                        .build_gep(
                            result_type.as_basic_type_enum(),
                            alloca,
                            &[zero, i32_type.const_int(0, false)],
                            "tag",
                        )
                        .unwrap()
                };
                self.builder
                    .build_store(tag_ptr, self.context.i8_type().const_int(0, false))
                    .unwrap();
                let err_ptr = unsafe {
                    self.builder
                        .build_gep(
                            result_type.as_basic_type_enum(),
                            alloca,
                            &[zero, i32_type.const_int(2, false)],
                            "err",
                        )
                        .unwrap()
                };
                self.builder.build_store(err_ptr, err_val).unwrap();
                let loaded = self.builder.build_load(result_type, alloca, "ret").unwrap();
                self.builder.build_return(Some(&loaded)).unwrap();
            }
            _ => {
                return Err(CodegenError::new(
                    "? operator can only be used in functions returning Option or Result",
                ));
            }
        }

        // Success block: extract the value and continue
        self.builder.position_at_end(success_block);
        let extracted = self
            .builder
            .build_extract_value(struct_val, 1, "unwrapped")
            .unwrap();
        self.builder
            .build_unconditional_branch(merge_block)
            .unwrap();

        // Merge block: return the extracted value
        self.builder.position_at_end(merge_block);

        Ok(extracted)
    }

    // === Standard Library Functions ===

    fn compile_stdlib_function(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
    ) -> Result<Option<BasicValueEnum<'ctx>>> {
        match name {
            // Math functions
            "abs" => {
                let val = self.compile_expr(&args[0].node)?;
                if val.is_int_value() {
                    let v = val.into_int_value();
                    let is_neg = self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLT,
                            v,
                            self.context.i64_type().const_int(0, false),
                            "is_neg",
                        )
                        .unwrap();
                    let neg = self.builder.build_int_neg(v, "neg").unwrap();
                    let result = self.builder.build_select(is_neg, neg, v, "abs").unwrap();
                    Ok(Some(result))
                } else {
                    let fabs = self.get_or_declare_math_func("fabs", true);
                    let call = self.builder.build_call(fabs, &[val.into()], "abs").unwrap();
                    Ok(Some(self.extract_call_value(call)))
                }
            }
            "min" => {
                let a = self.compile_expr(&args[0].node)?;
                let b = self.compile_expr(&args[1].node)?;
                if a.is_int_value() {
                    let av = a.into_int_value();
                    let bv = b.into_int_value();
                    let cond = self
                        .builder
                        .build_int_compare(IntPredicate::SLT, av, bv, "cmp")
                        .unwrap();
                    let result = self.builder.build_select(cond, av, bv, "min").unwrap();
                    Ok(Some(result))
                } else {
                    let fmin = self.get_or_declare_math_func2("fmin");
                    let call = self
                        .builder
                        .build_call(fmin, &[a.into(), b.into()], "min")
                        .unwrap();
                    Ok(Some(self.extract_call_value(call)))
                }
            }
            "max" => {
                let a = self.compile_expr(&args[0].node)?;
                let b = self.compile_expr(&args[1].node)?;
                if a.is_int_value() {
                    let av = a.into_int_value();
                    let bv = b.into_int_value();
                    let cond = self
                        .builder
                        .build_int_compare(IntPredicate::SGT, av, bv, "cmp")
                        .unwrap();
                    let result = self.builder.build_select(cond, av, bv, "max").unwrap();
                    Ok(Some(result))
                } else {
                    let fmax = self.get_or_declare_math_func2("fmax");
                    let call = self
                        .builder
                        .build_call(fmax, &[a.into(), b.into()], "max")
                        .unwrap();
                    Ok(Some(self.extract_call_value(call)))
                }
            }
            "sqrt" => {
                let val = self.compile_expr(&args[0].node)?;
                let sqrt = self.get_or_declare_math_func("sqrt", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(sqrt, &[fval.into()], "sqrt")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "pow" => {
                let base = self.compile_expr(&args[0].node)?;
                let exp = self.compile_expr(&args[1].node)?;
                let pow_fn = self.get_or_declare_math_func2("pow");
                let fbase = if base.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            base.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    base
                };
                let fexp = if exp.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            exp.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    exp
                };
                let call = self
                    .builder
                    .build_call(pow_fn, &[fbase.into(), fexp.into()], "pow")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "sin" => {
                let val = self.compile_expr(&args[0].node)?;
                let sin_fn = self.get_or_declare_math_func("sin", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(sin_fn, &[fval.into()], "sin")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "cos" => {
                let val = self.compile_expr(&args[0].node)?;
                let cos_fn = self.get_or_declare_math_func("cos", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(cos_fn, &[fval.into()], "cos")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "tan" => {
                let val = self.compile_expr(&args[0].node)?;
                let tan_fn = self.get_or_declare_math_func("tan", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(tan_fn, &[fval.into()], "tan")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "floor" => {
                let val = self.compile_expr(&args[0].node)?;
                let floor_fn = self.get_or_declare_math_func("floor", true);
                let call = self
                    .builder
                    .build_call(floor_fn, &[val.into()], "floor")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "ceil" => {
                let val = self.compile_expr(&args[0].node)?;
                let ceil_fn = self.get_or_declare_math_func("ceil", true);
                let call = self
                    .builder
                    .build_call(ceil_fn, &[val.into()], "ceil")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "round" => {
                let val = self.compile_expr(&args[0].node)?;
                let round_fn = self.get_or_declare_math_func("round", true);
                let call = self
                    .builder
                    .build_call(round_fn, &[val.into()], "round")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "log" => {
                let val = self.compile_expr(&args[0].node)?;
                let log_fn = self.get_or_declare_math_func("log", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(log_fn, &[fval.into()], "log")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "log10" => {
                let val = self.compile_expr(&args[0].node)?;
                let log10_fn = self.get_or_declare_math_func("log10", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(log10_fn, &[fval.into()], "log10")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "exp" => {
                let val = self.compile_expr(&args[0].node)?;
                let exp_fn = self.get_or_declare_math_func("exp", true);
                let fval = if val.is_int_value() {
                    self.builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap()
                        .into()
                } else {
                    val
                };
                let call = self
                    .builder
                    .build_call(exp_fn, &[fval.into()], "exp")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }

            // Type conversion functions
            "to_float" => {
                let val = self.compile_expr(&args[0].node)?;
                if val.is_int_value() {
                    let result = self
                        .builder
                        .build_signed_int_to_float(
                            val.into_int_value(),
                            self.context.f64_type(),
                            "tofloat",
                        )
                        .unwrap();
                    Ok(Some(result.into()))
                } else {
                    Ok(Some(val))
                }
            }
            "to_int" => {
                let val = self.compile_expr(&args[0].node)?;
                if val.is_float_value() {
                    let result = self
                        .builder
                        .build_float_to_signed_int(
                            val.into_float_value(),
                            self.context.i64_type(),
                            "toint",
                        )
                        .unwrap();
                    Ok(Some(result.into()))
                } else {
                    Ok(Some(val))
                }
            }
            "to_string" => {
                let val = self.compile_expr(&args[0].node)?;
                let sprintf = self.get_or_declare_sprintf();
                let malloc = self.get_or_declare_malloc();

                // Allocate buffer
                let buffer_size = self.context.i64_type().const_int(64, false);
                let buffer_call = self
                    .builder
                    .build_call(malloc, &[buffer_size.into()], "strbuf")
                    .unwrap();
                let buffer = self.extract_call_value(buffer_call).into_pointer_value();

                // Format string based on type
                let (fmt, print_args): (&str, Vec<BasicMetadataValueEnum>) = if val.is_int_value() {
                    ("%lld", vec![val.into()])
                } else if val.is_float_value() {
                    ("%f", vec![val.into()])
                } else {
                    ("%s", vec![val.into()])
                };

                let fmt_val = self.context.const_string(fmt.as_bytes(), true);
                let fmt_name = format!("fmt.{}", self.str_counter);
                self.str_counter += 1;
                let fmt_global = self.module.add_global(fmt_val.get_type(), None, &fmt_name);
                fmt_global.set_initializer(&fmt_val);

                let mut sprintf_args: Vec<BasicMetadataValueEnum> =
                    vec![buffer.into(), fmt_global.as_pointer_value().into()];
                sprintf_args.extend(print_args);
                self.builder
                    .build_call(sprintf, &sprintf_args, "sprintf")
                    .unwrap();

                Ok(Some(buffer.into()))
            }

            // String functions
            "strlen" => {
                let s = self.compile_expr(&args[0].node)?;
                let strlen_fn = self.get_or_declare_strlen();
                let call = self
                    .builder
                    .build_call(strlen_fn, &[s.into()], "len")
                    .unwrap();
                Ok(Some(self.extract_call_value(call)))
            }
            "strcmp" => {
                let s1 = self.compile_expr(&args[0].node)?;
                let s2 = self.compile_expr(&args[1].node)?;
                let strcmp_fn = self.get_or_declare_strcmp();
                let call = self
                    .builder
                    .build_call(strcmp_fn, &[s1.into(), s2.into()], "cmp")
                    .unwrap();
                // strcmp returns i32, extend to i64
                let result = self.extract_call_value(call).into_int_value();
                let extended = self
                    .builder
                    .build_int_s_extend(result, self.context.i64_type(), "cmp64")
                    .unwrap();
                Ok(Some(extended.into()))
            }
            "strcat" => {
                // Allocate new buffer and concatenate
                let s1 = self.compile_expr(&args[0].node)?;
                let s2 = self.compile_expr(&args[1].node)?;

                let strlen_fn = self.get_or_declare_strlen();
                let malloc = self.get_or_declare_malloc();
                let strcpy_fn = self.get_or_declare_strcpy();
                let strcat_fn = self.get_or_declare_strcat();

                // Get lengths
                let len1_call = self
                    .builder
                    .build_call(strlen_fn, &[s1.into()], "len1")
                    .unwrap();
                let len1 = self.extract_call_value(len1_call).into_int_value();
                let len2_call = self
                    .builder
                    .build_call(strlen_fn, &[s2.into()], "len2")
                    .unwrap();
                let len2 = self.extract_call_value(len2_call).into_int_value();

                // Allocate len1 + len2 + 1
                let total_len = self.builder.build_int_add(len1, len2, "total").unwrap();
                let buffer_size = self
                    .builder
                    .build_int_add(
                        total_len,
                        self.context.i64_type().const_int(1, false),
                        "bufsize",
                    )
                    .unwrap();

                let buffer_call = self
                    .builder
                    .build_call(malloc, &[buffer_size.into()], "buf")
                    .unwrap();
                let buffer = self.extract_call_value(buffer_call).into_pointer_value();

                // strcpy(buffer, s1)
                self.builder
                    .build_call(strcpy_fn, &[buffer.into(), s1.into()], "")
                    .unwrap();
                // strcat(buffer, s2)
                self.builder
                    .build_call(strcat_fn, &[buffer.into(), s2.into()], "")
                    .unwrap();

                Ok(Some(buffer.into()))
            }

            // I/O functions
            "read_line" => {
                // Read a line from stdin
                let malloc = self.get_or_declare_malloc();
                let fgets = self.get_or_declare_fgets();
                let stdin = self.get_or_declare_stdin();

                let buffer_size = self.context.i64_type().const_int(1024, false);
                let buffer_call = self
                    .builder
                    .build_call(malloc, &[buffer_size.into()], "linebuf")
                    .unwrap();
                let buffer = self.extract_call_value(buffer_call).into_pointer_value();

                let stdin_val = self
                    .builder
                    .build_load(
                        self.context.ptr_type(AddressSpace::default()),
                        stdin,
                        "stdin",
                    )
                    .unwrap();

                self.builder
                    .build_call(
                        fgets,
                        &[
                            buffer.into(),
                            self.context.i32_type().const_int(1024, false).into(),
                            stdin_val.into(),
                        ],
                        "fgets",
                    )
                    .unwrap();

                Ok(Some(buffer.into()))
            }
            "exit" => {
                let code = self.compile_expr(&args[0].node)?;
                let exit_fn = self.get_or_declare_exit();
                let code_i32 = self
                    .builder
                    .build_int_truncate(code.into_int_value(), self.context.i32_type(), "exitcode")
                    .unwrap();
                self.builder
                    .build_call(exit_fn, &[code_i32.into()], "")
                    .unwrap();
                Ok(Some(self.context.i8_type().const_int(0, false).into()))
            }

            // Not a stdlib function
            _ => Ok(None),
        }
    }

    fn get_or_declare_math_func(&mut self, name: &str, single_arg: bool) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        if single_arg {
            let fn_type = self
                .context
                .f64_type()
                .fn_type(&[self.context.f64_type().into()], false);
            self.module.add_function(name, fn_type, None)
        } else {
            let fn_type = self.context.f64_type().fn_type(
                &[
                    self.context.f64_type().into(),
                    self.context.f64_type().into(),
                ],
                false,
            );
            self.module.add_function(name, fn_type, None)
        }
    }

    fn get_or_declare_math_func2(&mut self, name: &str) -> FunctionValue<'ctx> {
        self.get_or_declare_math_func(name, false)
    }

    fn get_or_declare_strlen(&mut self) -> FunctionValue<'ctx> {
        let name = "strlen";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.i64_type().fn_type(
            &[self.context.ptr_type(AddressSpace::default()).into()],
            false,
        );
        self.module.add_function(name, fn_type, None)
    }

    fn get_or_declare_strcmp(&mut self) -> FunctionValue<'ctx> {
        let name = "strcmp";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.i32_type().fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, None)
    }

    fn get_or_declare_strcpy(&mut self) -> FunctionValue<'ctx> {
        let name = "strcpy";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, None)
    }

    fn get_or_declare_strcat(&mut self) -> FunctionValue<'ctx> {
        let name = "strcat";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, None)
    }

    fn get_or_declare_fgets(&mut self) -> FunctionValue<'ctx> {
        let name = "fgets";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self.context.ptr_type(AddressSpace::default()).fn_type(
            &[
                self.context.ptr_type(AddressSpace::default()).into(),
                self.context.i32_type().into(),
                self.context.ptr_type(AddressSpace::default()).into(),
            ],
            false,
        );
        self.module.add_function(name, fn_type, None)
    }

    fn get_or_declare_stdin(&mut self) -> PointerValue<'ctx> {
        let name = "__acrt_iob_func";
        let func = if let Some(f) = self.module.get_function(name) {
            f
        } else {
            let fn_type = self
                .context
                .ptr_type(AddressSpace::default())
                .fn_type(&[self.context.i32_type().into()], false);
            self.module.add_function(name, fn_type, None)
        };
        // stdin is __acrt_iob_func(0) on Windows
        let call = self
            .builder
            .build_call(
                func,
                &[self.context.i32_type().const_int(0, false).into()],
                "stdin",
            )
            .unwrap();
        self.extract_call_value(call).into_pointer_value()
    }

    fn get_or_declare_exit(&mut self) -> FunctionValue<'ctx> {
        let name = "exit";
        if let Some(f) = self.module.get_function(name) {
            return f;
        }
        let fn_type = self
            .context
            .void_type()
            .fn_type(&[self.context.i32_type().into()], false);
        self.module.add_function(name, fn_type, None)
    }

    /// Helper to extract basic value from call result
    fn extract_call_value(
        &self,
        call: inkwell::values::CallSiteValue<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        match call.try_as_basic_value() {
            ValueKind::Basic(val) => val,
            _ => panic!("Expected call to return a value"),
        }
    }

    // === Borrow/Deref ===

    fn compile_borrow(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        // Get pointer to the lvalue
        let ptr = self.compile_lvalue(expr)?;
        Ok(ptr.into())
    }

    fn compile_deref(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>> {
        // Compile the expression to get a pointer value
        let ptr_val = self.compile_expr(expr)?.into_pointer_value();

        // For now, assume i64 as the default dereferenced type
        // A full implementation would track the reference type
        let val = self
            .builder
            .build_load(self.context.i64_type(), ptr_val, "deref")
            .unwrap();
        Ok(val)
    }

    // === Lambda functions ===

    fn compile_lambda(
        &mut self,
        params: &[Parameter],
        body: &Spanned<Expr>,
    ) -> Result<BasicValueEnum<'ctx>> {
        // 1. Identify captures
        let captures = self.identify_captures(&body.node, params);

        // 2. Infer return type
        let ret_apex_ty = self.infer_expr_type(&body.node, params);
        let ret_llvm_ty = self.llvm_type(&ret_apex_ty);

        // 3. Create environment struct in outer scope
        let mut env_types = Vec::new();
        for (_, ty) in &captures {
            env_types.push(self.llvm_type(ty));
        }
        let env_struct_ty = self.context.struct_type(&env_types, false);

        let malloc = self.get_or_declare_malloc();
        let size = env_struct_ty.size_of().unwrap();
        let env_ptr_raw = match self
            .builder
            .build_call(malloc, &[size.into()], "env_ptr")
            .unwrap()
            .try_as_basic_value()
        {
            ValueKind::Basic(val) => val.into_pointer_value(),
            _ => panic!("malloc should return a value"),
        };

        // Fill environment
        for (i, (name, ty)) in captures.iter().enumerate() {
            let var = self.variables.get(name).unwrap();
            let val = self
                .builder
                .build_load(self.llvm_type(ty), var.ptr, name)
                .unwrap();
            let field_ptr = unsafe {
                self.builder
                    .build_gep(
                        env_struct_ty,
                        env_ptr_raw,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(i as u64, false),
                        ],
                        "capture",
                    )
                    .unwrap()
            };
            self.builder.build_store(field_ptr, val).unwrap();
        }

        // Save current function context
        let saved_function = self.current_function;
        let saved_return_type = self.current_return_type.clone();
        let saved_variables = std::mem::take(&mut self.variables);

        // Create unique name for lambda
        let lambda_name = format!("__lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;

        // Build parameter types (including env_ptr as first arg)
        let mut llvm_params: Vec<BasicMetadataTypeEnum> = vec![
            self.context.ptr_type(AddressSpace::default()).into(), // env_ptr
        ];
        for p in params {
            llvm_params.push(self.llvm_type(&p.ty).into());
        }

        // Create function with inferred return type
        let fn_type = match ret_llvm_ty {
            BasicTypeEnum::IntType(i) => i.fn_type(&llvm_params, false),
            BasicTypeEnum::FloatType(f) => f.fn_type(&llvm_params, false),
            BasicTypeEnum::PointerType(p) => p.fn_type(&llvm_params, false),
            BasicTypeEnum::StructType(s) => s.fn_type(&llvm_params, false),
            _ => self.context.i8_type().fn_type(&llvm_params, false),
        };
        let lambda_fn = self.module.add_function(&lambda_name, fn_type, None);

        // Set up function body
        self.current_function = Some(lambda_fn);
        self.current_return_type = Some(ret_apex_ty.clone());

        let entry = self.context.append_basic_block(lambda_fn, "entry");
        self.builder.position_at_end(entry);

        // Populate local variables from env_ptr
        let env_ptr_arg = lambda_fn.get_nth_param(0).unwrap().into_pointer_value();
        for (i, (name, ty)) in captures.iter().enumerate() {
            let field_ptr = unsafe {
                self.builder
                    .build_gep(
                        env_struct_ty,
                        env_ptr_arg,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i32_type().const_int(i as u64, false),
                        ],
                        "load_capture",
                    )
                    .unwrap()
            };
            let alloca = self.builder.build_alloca(self.llvm_type(ty), name).unwrap();
            let val = self
                .builder
                .build_load(self.llvm_type(ty), field_ptr, "cap_val")
                .unwrap();
            self.builder.build_store(alloca, val).unwrap();
            self.variables.insert(
                name.clone(),
                Variable {
                    ptr: alloca,
                    ty: ty.clone(),
                },
            );
        }

        // Allocate parameters (starting from index 1)
        for (i, param) in params.iter().enumerate() {
            let llvm_param = lambda_fn.get_nth_param((i + 1) as u32).unwrap();
            let alloca = self
                .builder
                .build_alloca(self.llvm_type(&param.ty), &param.name)
                .unwrap();
            self.builder.build_store(alloca, llvm_param).unwrap();
            self.variables.insert(
                param.name.clone(),
                Variable {
                    ptr: alloca,
                    ty: param.ty.clone(),
                },
            );
        }

        // Compile body expression
        let result = self.compile_expr(&body.node)?;

        // Build return with proper casting if needed
        let final_result = if result.get_type() != ret_llvm_ty {
            // Handle i32 to i64 (like from println)
            if result.is_int_value() && ret_llvm_ty.is_int_type() {
                let res_int = result.into_int_value();
                let ret_int = ret_llvm_ty.into_int_type();
                if res_int.get_type().get_bit_width() < ret_int.get_bit_width() {
                    self.builder
                        .build_int_z_extend(res_int, ret_int, "ret_cast")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_truncate(res_int, ret_int, "ret_cast")
                        .unwrap()
                        .into()
                }
            } else {
                result
            }
        } else {
            result
        };

        self.builder.build_return(Some(&final_result)).unwrap();

        // Restore context
        self.current_function = saved_function;
        self.current_return_type = saved_return_type;
        self.variables = saved_variables;

        // Position builder back to the original function
        if let Some(func) = saved_function {
            if let Some(block) = func.get_last_basic_block() {
                self.builder.position_at_end(block);
            }
        }

        // Return closure struct { fn_ptr, env_ptr }
        let ptr_type = self.context.ptr_type(AddressSpace::default());
        let closure_ty = self
            .context
            .struct_type(&[ptr_type.into(), ptr_type.into()], false);

        let mut closure = closure_ty.get_undef();
        closure = self
            .builder
            .build_insert_value(
                closure,
                lambda_fn.as_global_value().as_pointer_value(),
                0,
                "fn",
            )
            .unwrap()
            .into_struct_value();
        closure = self
            .builder
            .build_insert_value(closure, env_ptr_raw, 1, "env")
            .unwrap()
            .into_struct_value();

        Ok(closure.into())
    }

    fn compile_match_expr(
        &mut self,
        expr: &Expr,
        arms: &[MatchArm],
    ) -> Result<BasicValueEnum<'ctx>> {
        // Simplified: return value of first matching arm
        let _val = self.compile_expr(expr)?;

        for arm in arms {
            if matches!(arm.pattern, Pattern::Wildcard | Pattern::Ident(_)) {
                // Execute body and return last expression
                for (i, stmt) in arm.body.iter().enumerate() {
                    if i == arm.body.len() - 1 {
                        if let Stmt::Expr(e) = &stmt.node {
                            return self.compile_expr(&e.node);
                        }
                    }
                    self.compile_stmt(&stmt.node)?;
                }
            }
        }

        Ok(self.context.i64_type().const_int(0, false).into())
    }

    fn compile_lvalue(&mut self, expr: &Expr) -> Result<PointerValue<'ctx>> {
        match expr {
            Expr::Ident(name) => self
                .variables
                .get(name)
                .map(|v| v.ptr)
                .ok_or_else(|| CodegenError::new(format!("Unknown variable: {}", name))),
            Expr::Field { object, field } => {
                let obj_ptr = self.compile_expr(&object.node)?.into_pointer_value();

                let class_name = match &object.node {
                    Expr::Ident(name) => self.variables.get(name).and_then(|v| match &v.ty {
                        Type::Named(n) => Some(n.clone()),
                        _ => None,
                    }),
                    Expr::This => self.variables.get("this").and_then(|v| match &v.ty {
                        Type::Named(n) => Some(n.clone()),
                        _ => None,
                    }),
                    _ => None,
                }
                .ok_or_else(|| CodegenError::new("Cannot determine object type"))?;

                let class_info = self
                    .classes
                    .get(&class_name)
                    .ok_or_else(|| CodegenError::new(format!("Unknown class: {}", class_name)))?;

                let field_idx = *class_info
                    .field_indices
                    .get(field)
                    .ok_or_else(|| CodegenError::new(format!("Unknown field: {}", field)))?;

                let i32_type = self.context.i32_type();
                let zero = i32_type.const_int(0, false);
                let idx = i32_type.const_int(field_idx as u64, false);

                unsafe {
                    Ok(self
                        .builder
                        .build_gep(
                            class_info.struct_type.as_basic_type_enum(),
                            obj_ptr,
                            &[zero, idx],
                            field,
                        )
                        .unwrap())
                }
            }
            _ => Err(CodegenError::new("Invalid lvalue")),
        }
    }

    // === Helpers ===

    /// Infer the Apex Type of an expression
    fn infer_object_type(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Ident(name) => self.variables.get(name).map(|v| v.ty.clone()),
            Expr::This => self.variables.get("this").map(|v| v.ty.clone()),
            Expr::Field { object, field } => {
                let obj_ty = self.infer_object_type(&object.node)?;
                let class_name = match &obj_ty {
                    Type::Named(n) => n.clone(),
                    _ => return None,
                };
                let class_info = self.classes.get(&class_name)?;
                class_info.field_types.get(field).cloned()
            }
            _ => None,
        }
    }

    /// Extract class name from a Type (handles Named, Ref, MutRef, etc.)
    fn type_to_class_name(&self, ty: &Type) -> Option<String> {
        match ty {
            Type::Named(name) => Some(name.clone()),
            Type::Ref(inner)
            | Type::MutRef(inner)
            | Type::Box(inner)
            | Type::Rc(inner)
            | Type::Arc(inner) => self.type_to_class_name(inner),
            _ => None,
        }
    }

    fn needs_terminator(&self) -> bool {
        self.builder
            .get_insert_block()
            .map(|b| b.get_terminator().is_none())
            .unwrap_or(false)
    }

    fn get_or_declare_printf(&mut self) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function("printf") {
            return f;
        }

        let printf_type = self.context.i32_type().fn_type(
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true,
        );
        self.module.add_function("printf", printf_type, None)
    }

    // === Output ===

    pub fn write_ir(&self, path: &Path) -> std::result::Result<(), String> {
        self.module.print_to_file(path).map_err(|e| e.to_string())
    }

    pub fn write_object(&self, path: &Path) -> std::result::Result<(), String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("Failed to init target: {}", e))?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple).map_err(|e| e.to_string())?;

        let machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("Failed to create target machine")?;

        machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| e.to_string())
    }
    fn identify_captures(&self, expr: &Expr, params: &[Parameter]) -> Vec<(String, Type)> {
        let mut captures = Vec::new();
        let mut seen = std::collections::HashSet::new();
        let mut param_names = std::collections::HashSet::new();
        for p in params {
            param_names.insert(p.name.clone());
        }

        self.walk_expr_for_captures(expr, &param_names, &mut captures, &mut seen);
        captures
    }

    fn walk_expr_for_captures(
        &self,
        expr: &Expr,
        params: &std::collections::HashSet<String>,
        captures: &mut Vec<(String, Type)>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        match expr {
            Expr::Ident(name) => {
                if !params.contains(name) && !seen.contains(name) {
                    if let Some(var) = self.variables.get(name) {
                        seen.insert(name.clone());
                        captures.push((name.clone(), var.ty.clone()));
                    }
                }
            }
            Expr::Binary { left, right, .. } => {
                self.walk_expr_for_captures(&left.node, params, captures, seen);
                self.walk_expr_for_captures(&right.node, params, captures, seen);
            }
            Expr::Unary { expr, .. } => {
                self.walk_expr_for_captures(&expr.node, params, captures, seen);
            }
            Expr::Call { callee, args } => {
                self.walk_expr_for_captures(&callee.node, params, captures, seen);
                for arg in args {
                    self.walk_expr_for_captures(&arg.node, params, captures, seen);
                }
            }
            Expr::Field { object, .. } => {
                self.walk_expr_for_captures(&object.node, params, captures, seen);
            }
            Expr::Index { object, index } => {
                self.walk_expr_for_captures(&object.node, params, captures, seen);
                self.walk_expr_for_captures(&index.node, params, captures, seen);
            }
            Expr::Construct { args, .. } => {
                for arg in args {
                    self.walk_expr_for_captures(&arg.node, params, captures, seen);
                }
            }
            Expr::StringInterp(parts) => {
                for part in parts {
                    if let StringPart::Expr(e) = part {
                        self.walk_expr_for_captures(&e.node, params, captures, seen);
                    }
                }
            }
            Expr::Lambda {
                params: l_params,
                body: l_body,
            } => {
                let mut nested_params = params.clone();
                for p in l_params {
                    nested_params.insert(p.name.clone());
                }
                self.walk_expr_for_captures(&l_body.node, &nested_params, captures, seen);
            }
            Expr::Match { expr, arms } => {
                self.walk_expr_for_captures(&expr.node, params, captures, seen);
                for arm in arms {
                    for stmt in &arm.body {
                        self.walk_stmt_for_captures(&stmt.node, params, captures, seen);
                    }
                }
            }
            Expr::Try(inner) => {
                self.walk_expr_for_captures(&inner.node, params, captures, seen);
            }
            Expr::Await(inner) => {
                self.walk_expr_for_captures(&inner.node, params, captures, seen);
            }
            Expr::AsyncBlock(stmts) => {
                for stmt in stmts {
                    self.walk_stmt_for_captures(&stmt.node, params, captures, seen);
                }
            }
            _ => {}
        }
    }

    fn walk_stmt_for_captures(
        &self,
        stmt: &Stmt,
        params: &std::collections::HashSet<String>,
        captures: &mut Vec<(String, Type)>,
        seen: &mut std::collections::HashSet<String>,
    ) {
        match stmt {
            Stmt::Expr(e) => self.walk_expr_for_captures(&e.node, params, captures, seen),
            Stmt::Let { value, .. } => {
                self.walk_expr_for_captures(&value.node, params, captures, seen);
                // Let doesn't capture the variable it's declaring, but we'll ignore shadowing for now
            }
            Stmt::Assign { target, value } => {
                self.walk_expr_for_captures(&target.node, params, captures, seen);
                self.walk_expr_for_captures(&value.node, params, captures, seen);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.walk_expr_for_captures(&condition.node, params, captures, seen);
                for s in then_block {
                    self.walk_stmt_for_captures(&s.node, params, captures, seen);
                }
                if let Some(eb) = else_block {
                    for s in eb {
                        self.walk_stmt_for_captures(&s.node, params, captures, seen);
                    }
                }
            }
            Stmt::While { condition, body } => {
                self.walk_expr_for_captures(&condition.node, params, captures, seen);
                for s in body {
                    self.walk_stmt_for_captures(&s.node, params, captures, seen);
                }
            }
            Stmt::For { iterable, body, .. } => {
                self.walk_expr_for_captures(&iterable.node, params, captures, seen);
                for s in body {
                    self.walk_stmt_for_captures(&s.node, params, captures, seen);
                }
            }
            Stmt::Return(Some(expr)) => {
                self.walk_expr_for_captures(&expr.node, params, captures, seen);
            }
            _ => {}
        }
    }

    fn infer_expr_type(&self, expr: &Expr, params: &[Parameter]) -> Type {
        match expr {
            Expr::Literal(lit) => match lit {
                Literal::Integer(_) => Type::Integer,
                Literal::Float(_) => Type::Float,
                Literal::Boolean(_) => Type::Boolean,
                Literal::String(_) => Type::String,
                Literal::Char(_) => Type::Char,
                Literal::None => Type::None,
            },
            Expr::Ident(name) => {
                // Check parameters first
                if let Some(p) = params.iter().find(|p| p.name == *name) {
                    return p.ty.clone();
                }
                // Then local variables
                if let Some(var) = self.variables.get(name) {
                    return var.ty.clone();
                }
                // Then global functions
                if let Some((_, ty)) = self.functions.get(name) {
                    return ty.clone();
                }
                Type::Integer
            }
            Expr::Binary { op, left, .. } => match op {
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Lt
                | BinOp::LtEq
                | BinOp::Gt
                | BinOp::GtEq
                | BinOp::And
                | BinOp::Or => Type::Boolean,
                _ => self.infer_expr_type(&left.node, params),
            },
            Expr::Unary { op, expr } => match op {
                UnaryOp::Not => Type::Boolean,
                UnaryOp::Neg => self.infer_expr_type(&expr.node, params),
            },
            Expr::Call { callee, .. } => match &callee.node {
                Expr::Ident(name) if name == "println" => Type::None,
                _ => {
                    let callee_ty = self.infer_expr_type(&callee.node, params);
                    if let Type::Function(_, ret_ty) = callee_ty {
                        *ret_ty
                    } else {
                        Type::Integer
                    }
                }
            },
            Expr::Field { object, .. } => {
                let _obj_ty = self.infer_expr_type(&object.node, params);
                Type::Integer
            }
            Expr::Lambda { params, body } => {
                let ret_ty = self.infer_expr_type(&body.node, params);
                Type::Function(
                    params.iter().map(|p| p.ty.clone()).collect(),
                    Box::new(ret_ty),
                )
            }
            _ => Type::Integer,
        }
    }
}
