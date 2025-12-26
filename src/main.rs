//! Apex Programming Language Compiler

mod lexer;
mod ast;
mod parser;
mod typeck;
mod borrowck;
mod codegen;

use clap::{Parser as ClapParser, Subcommand};
use colored::*;
use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::parser::Parser;
use crate::typeck::TypeChecker;
use crate::borrowck::BorrowChecker;
use crate::codegen::Codegen;

#[derive(ClapParser)]
#[command(name = "apex")]
#[command(author = "Remyyy")]
#[command(version = "0.1.0")]
#[command(about = "Apex Programming Language Compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Compile an Apex source file
    Compile {
        /// Input file
        file: PathBuf,
        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
        /// Emit LLVM IR
        #[arg(long)]
        emit_llvm: bool,
        /// Skip type checking
        #[arg(long)]
        no_check: bool,
    },
    /// Compile and run
    Run {
        /// Input file
        file: PathBuf,
        /// Arguments to pass
        args: Vec<String>,
        /// Skip type checking
        #[arg(long)]
        no_check: bool,
    },
    /// Check syntax and types
    Check {
        /// Input file
        file: PathBuf,
    },
    /// Show tokens (debug)
    Lex {
        /// Input file
        file: PathBuf,
    },
    /// Show AST (debug)
    Parse {
        /// Input file
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = match cli.command {
        Commands::Compile { file, output, emit_llvm, no_check } => {
            compile(&file, output.as_deref(), emit_llvm, !no_check)
        }
        Commands::Run { file, args, no_check } => {
            run(&file, &args, !no_check)
        }
        Commands::Check { file } => {
            check(&file)
        }
        Commands::Lex { file } => {
            lex(&file)
        }
        Commands::Parse { file } => {
            parse_cmd(&file)
        }
    };

    if let Err(e) = result {
        eprintln!("{}", e);
        std::process::exit(1);
    }

    // Exit before LLVM cleanup to avoid Windows crash
    std::process::exit(0);
}

fn compile(file: &Path, output: Option<&Path>, emit_llvm: bool, do_check: bool) -> Result<(), String> {
    println!("{} {}", "Compiling".green().bold(), file.display());

    let source = fs::read_to_string(file)
        .map_err(|e| format!("{}: Failed to read file: {}", "error".red().bold(), e))?;

    let filename = file.file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("input.apex");

    // Tokenize
    let tokens = lexer::tokenize(&source)
        .map_err(|e| format!("{}: Lexer error: {}", "error".red().bold(), e))?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()
        .map_err(|e| format_parse_error(&e, &source, filename))?;

    // Type check
    if do_check {
        let mut type_checker = TypeChecker::new(source.clone());
        if let Err(errors) = type_checker.check(&program) {
            return Err(typeck::format_errors(&errors, &source, filename));
        }

        // Borrow check
        let mut borrow_checker = BorrowChecker::new();
        if let Err(errors) = borrow_checker.check(&program) {
            return Err(borrowck::format_borrow_errors(&errors, &source, filename));
        }
    }

    // Codegen
    let context = Context::create();
    let module_name = file.file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("main");

    let mut codegen = Codegen::new(&context, module_name);
    codegen.compile(&program)
        .map_err(|e| format!("{}: Codegen error: {}", "error".red().bold(), e.message))?;

    if emit_llvm {
        let output_path = output
            .map(PathBuf::from)
            .unwrap_or_else(|| file.with_extension("ll"));
        codegen.write_ir(&output_path)?;
        println!("{} {}", "Wrote".green().bold(), output_path.display());
    } else {
        // Write LLVM IR to temp file then use clang to compile
        let ir_path = file.with_extension("ll");
        codegen.write_ir(&ir_path)?;

        let output_path = output
            .map(PathBuf::from)
            .unwrap_or_else(|| {
                #[cfg(windows)]
                { file.with_extension("exe") }
                #[cfg(not(windows))]
                { file.with_extension("") }
            });

        compile_ir(&ir_path, &output_path)?;
        let _ = fs::remove_file(&ir_path);

        println!("{} {}", "Output".green().bold(), output_path.display());
    }

    Ok(())
}

fn compile_ir(ir_path: &Path, output_path: &Path) -> Result<(), String> {
    let result = Command::new("clang")
        .arg(ir_path)
        .arg("-o")
        .arg(output_path)
        .arg("-Wno-override-module")
        .arg("-llegacy_stdio_definitions")
        .output();

    match result {
        Ok(output) => {
            if output.status.success() {
                Ok(())
            } else {
                let stderr = String::from_utf8_lossy(&output.stderr);
                Err(format!("{}: Clang failed: {}", "error".red().bold(), stderr))
            }
        }
        Err(_) => Err(format!("{}: Clang not found. Install clang to compile.", "error".red().bold()))
    }
}

fn run(file: &Path, args: &[String], do_check: bool) -> Result<(), String> {
    #[cfg(windows)]
    let output = file.with_extension("run.exe");
    #[cfg(not(windows))]
    let output = file.with_extension("run");

    compile(file, Some(&output), false, do_check)?;

    println!("{}", "Running...".cyan().bold());
    println!();

    let status = Command::new(&output)
        .args(args)
        .status()
        .map_err(|e| format!("{}: Failed to run: {}", "error".red().bold(), e))?;

    let _ = fs::remove_file(&output);

    if !status.success() {
        return Err(format!("{}: Program exited with code: {}",
            "error".red().bold(),
            status.code().unwrap_or(-1)));
    }

    Ok(())
}

fn check(file: &Path) -> Result<(), String> {
    println!("{} {}", "Checking".cyan().bold(), file.display());

    let source = fs::read_to_string(file)
        .map_err(|e| format!("{}: Failed to read file: {}", "error".red().bold(), e))?;

    let filename = file.file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("input.apex");

    // Tokenize
    let tokens = lexer::tokenize(&source)
        .map_err(|e| format!("{}: Lexer error: {}", "error".red().bold(), e))?;

    // Parse
    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()
        .map_err(|e| format_parse_error(&e, &source, filename))?;

    // Type check
    let mut type_checker = TypeChecker::new(source.clone());
    if let Err(errors) = type_checker.check(&program) {
        return Err(typeck::format_errors(&errors, &source, filename));
    }

    // Borrow check
    let mut borrow_checker = BorrowChecker::new();
    if let Err(errors) = borrow_checker.check(&program) {
        return Err(borrowck::format_borrow_errors(&errors, &source, filename));
    }

    println!("{}", "No errors found.".green());
    Ok(())
}

fn lex(file: &Path) -> Result<(), String> {
    let source = fs::read_to_string(file)
        .map_err(|e| format!("{}: Failed to read file: {}", "error".red().bold(), e))?;

    let tokens = lexer::tokenize(&source)
        .map_err(|e| format!("{}: Lexer error: {}", "error".red().bold(), e))?;

    println!("{} tokens:", "Found".cyan().bold());
    for (token, span) in tokens {
        println!("  {:?} @ {}..{}", token, span.start, span.end);
    }

    Ok(())
}

fn parse_cmd(file: &Path) -> Result<(), String> {
    let source = fs::read_to_string(file)
        .map_err(|e| format!("{}: Failed to read file: {}", "error".red().bold(), e))?;

    let tokens = lexer::tokenize(&source)
        .map_err(|e| format!("{}: Lexer error: {}", "error".red().bold(), e))?;

    let mut parser = Parser::new(tokens);
    let program = parser.parse_program()
        .map_err(|e| format!("{}: Parse error: {}", "error".red().bold(), e.message))?;

    println!("{}", "AST:".cyan().bold());
    println!("{:#?}", program);

    Ok(())
}

/// Format parse error with source context
fn format_parse_error(error: &parser::ParseError, source: &str, filename: &str) -> String {
    let lines: Vec<&str> = source.lines().collect();

    let mut line_num: usize = 1;
    let mut col: usize = 1;
    for (i, ch) in source.char_indices() {
        if i >= error.span.start {
            break;
        }
        if ch == '\n' {
            line_num += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    let mut output = String::new();
    output.push_str(&format!(
        "\x1b[1;31merror\x1b[0m: {}\n",
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

    output
}
