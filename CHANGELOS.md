# Changelog

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
