# Changelog

All notable changes to the Apex Programming Language Compiler will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added
- **BREAKING**: Standard library functions now require explicit imports
  - All stdlib functions organized under `std.*` namespaces
  - `std.io` - println, print, read_line
  - `std.fs` - File__read, File__write, File__exists, File__delete  
  - `std.system` - System__getenv, System__shell, System__exec, etc.
  - `std.time` - Time__now, Time__unix, Time__sleep
  - `std.math` - Math__sqrt, Math__sin, Math__cos, Math__pow, etc.
  - `std.string` - Str__len, Str__compare, Str__concat, etc.
  - Builtin functions (to_string, length) don't require imports
- New `stdlib` module for standard library namespace registry
- Import checking in `apex check` and `apex compile` commands

### Changed
- All 28 example files updated with required stdlib imports
- Import checker now validates both user-defined and stdlib functions

### Fixed
- Consistent import behavior across user and standard library functions

## [1.3.0] - 2025-02-21

### Added
- Multi-file project support with `apex.toml` configuration
- Java-style namespace system with `package` declarations
- Import system with wildcard (`import utils.math.*`) and specific (`import utils.math.factorial`) syntax
- New CLI commands: `apex new`, `apex build`, `apex run`, `apex info`
- Import checking for multi-file projects
- Project configuration via `apex.toml`
- CI workflow testing 32 examples including multi-file projects

### Changed
- Split codegen into modular structure (core.rs, types.rs, util.rs)

## [1.2.0] - 2025-02-20

### Added
- Ownership and borrowing system
- Borrow checker with lifetime analysis
- Mutable/immutable references (`&mut`, `&`)
- Move semantics for heap-allocated types

## [1.1.0] - 2025-01-15

### Added
- Pattern matching with `match` expressions
- Enums with associated values
- Interfaces and implementations
- Generics for functions, classes, and interfaces
- Lambda expressions and closures
- String interpolation
- Async/await support
- Standard library modules (File I/O, Time, System, Math, String utils)
- Error handling with Result<T, E> and Option<T>

## [1.0.0] - 2024-12-29

### Added
- Initial release of Apex Programming Language Compiler
- LLVM-based compilation
- Type system (Integer, Float, Boolean, String, Char, arrays, custom classes)
- Functions with return type annotations
- Classes with methods and fields
- Control flow (if/else, while, for loops)
- Basic standard library (println, print, math functions)
- CLI with compile, check, lex, parse commands
