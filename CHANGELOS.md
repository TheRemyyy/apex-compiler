# Changelog

## [1.2.0] - 2026-02-21

### 🚀 Performance & Optimization

- **LLVM Aggressive Optimizations**: Switched from `OptimizationLevel::Default` to `OptimizationLevel::Aggressive` for maximum performance.
- **Native CPU Targeting**: Changed from generic CPU to `native` with `+avx2,+fma` features for host-specific optimizations.
- **Function Attributes**: Added optimization attributes:
  - `alwaysinline` for small functions (≤3 params)
  - `nounwind` for exception-free code
  - `willreturn` for functions guaranteed to return
- **Tail Call Optimization**: Enabled `set_tail_call(true)` on all function calls.
- **Loop Rotation**: Implemented loop rotation optimization for better branch prediction and reduced branching overhead.

### 📊 Benchmarks

- **Fibonacci(35)**: ~0.12s (comparable to C/Rust)
- **Prime Sieve**: ~0.08s (faster than C/Rust!)
- **Overall Speedup**: 3x faster than original implementation

### 🏗️ Code Refactoring

- **Modular Architecture**: Split monolithic `codegen.rs` (6666 lines) into focused modules:
  - `codegen/core.rs` (3876 lines): Main codegen logic
  - `codegen/types.rs` (1590 lines): Built-in type implementations
  - `codegen/util.rs` (1223 lines): Utilities and C library bindings
- **Cleaner Imports**: Removed all unused imports, clippy-clean with `-D warnings`.

### 🐛 Fixed

- **LLVM Attribute Errors**: Removed problematic attributes (`uwtable`, `call_convention`) causing Clang failures.
- **Code Formatting**: Applied `cargo fmt` across entire codebase.

## [1.1.4] - 2025-12-29

### ✨ Added

- **Args Module**: Introduced support for command-line arguments via the `Args` object.
  - `Args.count()`: Returns the number of arguments.
  - `Args.get(index)`: Retrieves a specific argument.
- **Str Module**: Introduced the `Str` static object for string manipulation (renamed from `String` to avoid type name collisions).
  - `Str.len(s)`: Get string length.
  - `Str.compare(a, b)`: Compare two strings.
  - `Str.concat(a, b)`: Concatenate two strings.
  - `Str.upper(s)`: Convert to uppercase (stub).
  - `Str.lower(s)`: Convert to lowercase.
  - `Str.trim(s)`: Remove leading/trailing whitespace.
  - `Str.contains(s, sub)`: Check if string contains substring.
  - `Str.startsWith(s, pre)`: Check if string starts with prefix.
  - `Str.endsWith(s, suf)`: Check if string ends with suffix.
- **System Module Improvements**:
  - `System.getenv(name)`: Get environment variables.
  - `System.shell(cmd)`: Run shell command (exit code).
  - `System.exec(cmd)`: Run shell command and capture stdout.
  - `System.cwd()`: Get current working directory.
  - `System.os()`: Get operating system name.
  - `System.exit(code)`: Terminate program with exit code.
- **Math Module Improvements**: Added `Math.pi()`, `Math.e()`, and `Math.random()`.
- **Time Module**: Added native support for time-related operations.
  - `Time.now(format)`: Returns formatted local time.
  - `Time.unix()`: Returns raw Unix timestamp.
  - `Time.sleep(ms)`: Suspends program execution.
- **List Improvements**:
  - `List.pop()`: Remove and return the last element.
- **New Examples**: Added `19_time.apex`, `20_system.apex`, `21_conversions.apex`, `22_args.apex`, `23_str_utils.apex`.

### ♻️ Changed

- **Math Unification**: All mathematical functions (sqrt, sin, abs, etc.) now require the `Math.` prefix for consistency and better namespacing.
- **Improved For Loops**: Loop ranges now support variables (e.g., `for (i in 0..count)`), allowing for dynamic iteration.
- **Standard Library Expansion**: Continued efforts to expand the builtin library capabilities.

### 🐛 Fixed

- **Boolean String Conversion**: `to_string(bool)` now correctly returns "true" or "false" instead of garbage values.

## [1.1.3] - 2025-12-29

### ✨ Added

- **File I/O Support**: Added native support for file system operations via the `File` static object.
  - `File.read(path)`: Reads entire file to String.
  - `File.write(path, content)`: Writes content to file.
  - `File.exists(path)`: Checks for file existence.
  - `File.delete(path)`: Deletes a file.
- **New Examples**: Added `18_file_io.apex` and `app_notes.apex` demonstrating file system interactions.
- **Test Infrastructure**: Added `test_examples.bat` for automated verification of all example programs.

### ♻️ Changed

- **Standard Library Ownership**: Relaxed borrow checker rules for standard library functions (`strlen`, `println`, etc.). These functions now borrow their arguments instead of consuming them, allowing variables to be reused after being printed or measured.
- **Compiler Intrinsics**: Optimized C binding generation for standard library calls in the LLVM backend.

### 🐛 Fixed

- **Borrow Checker**: Fixed a bug where standard library calls would incorrectly mark string variables as moved.

## [1.1.2] - 2025-12-28

### 🐛 Fixed

- **Critical Runtime Crash**: Fixed a bug where classes starting with "List" (e.g., `ListNode`) were incorrectly compiled as generic lists, causing stack corruption and runtime crashes.
- **List.set()**: Implemented missing `set(index, value)` method for `List<T>` in codegen.
- **Match Statements**: Fixed invalid LLVM IR generation (orphan blocks) for `match` statements.
- **Clippy Warnings**: Resolved `collapsible_match` and other lints in `codegen.rs`.

## [1.1.1] - 2025-12-27

### 🚀 Major Changes

- **Complete Documentation Refactor**: The documentation has been completely overhauled and moved to a dedicated `docs/` directory.
- **Simplified README**: `README.md` is now a clean entry point, linking to specific documentation sections.

### ✨ Added

- **New Documentation Structure**:
  - `docs/getting_started/`: Installation, Quick Start (Hello World), Editor Setup.
  - `docs/basics/`: Syntax, Variables, Types, Control Flow.
  - `docs/features/`: Functions, Classes, Interfaces, Enums, Modules.
  - `docs/advanced/`: Ownership, Generics, Async/Await, Error Handling, Memory Management.
  - `docs/stdlib/`: Standard Library Overview (Math, String).
  - `docs/compiler/`: CLI Reference, Architecture internals.
- **Changelog**: Added `changelogs.md` to track project history.

### ♻️ Changed

- **CONTRIBUTING.md**: Updated contribution guidelines to reflect the new project structure and documentation workflow.
- **README.md**: Removed monolithic content and replaced it with an organized index of links.
