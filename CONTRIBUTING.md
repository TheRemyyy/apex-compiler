# Contributing to Apex

Thank you for your interest in contributing to Apex! This document provides guidelines and information for contributors.

## Development Setup

### Prerequisites

- Rust 1.83 or later
- LLVM 21.0 or later (21.1.7 recommended)
- Clang (for linking)
- Git

### Building from Source

```bash
git clone https://github.com/TheRemyyy/apex-compiler.git
cd apex-compiler
cargo build
```

### Running Tests

```bash
# Run all tests
cargo test

# Run specific test
cargo test test_name

# Check all examples
for file in examples/*.apex; do
    cargo run -- check "$file"
done
```

## Project Structure

```
apex-compiler/
├── src/
│   ├── main.rs       # CLI entry point
│   ├── lexer.rs      # Tokenization
│   ├── parser.rs     # Parsing
│   ├── ast.rs        # AST definitions
│   ├── typeck.rs     # Type checking
│   ├── borrowck.rs   # Borrow checking
│   └── codegen.rs    # LLVM code generation
├── examples/         # Example Apex programs
└── tests/            # Test suite
```

## Compiler Architecture

### Compilation Pipeline

1. **Lexing** (`lexer.rs`) — Source code → Tokens
2. **Parsing** (`parser.rs`) — Tokens → AST
3. **Type Checking** (`typeck.rs`) — AST → Typed AST
4. **Borrow Checking** (`borrowck.rs`) — Ownership validation
5. **Code Generation** (`codegen.rs`) — AST → LLVM IR
6. **Linking** (Clang) — LLVM IR → Executable

### Key Components

#### Lexer

The lexer (`lexer.rs`) converts source code into tokens:

- Handles keywords, identifiers, literals, operators
- Tracks source locations for error reporting
- Supports string interpolation tokenization

#### Parser

The parser (`parser.rs`) is a recursive descent parser that:

- Builds an Abstract Syntax Tree (AST)
- Handles operator precedence
- Provides detailed error messages with source locations
- Supports all language constructs

#### Type Checker

The type checker (`typeck.rs`) ensures type safety:

- Resolves types for all expressions
- Checks function signatures
- Validates generic type parameters
- Handles type inference
- Reports type errors with helpful messages

#### Borrow Checker

The borrow checker (`borrowck.rs`) enforces memory safety:

- Tracks ownership of values
- Validates borrow rules (one mutable or many immutable)
- Prevents use-after-move errors
- Ensures reference lifetimes

#### Code Generator

The code generator (`codegen.rs`) produces LLVM IR:

- Compiles AST to LLVM IR
- Implements standard library functions
- Handles memory management
- Optimizes generated code

## Contributing Guidelines

### Code Style

- Follow Rust standard formatting (`cargo fmt`)
- Run Clippy and fix warnings (`cargo clippy`)
- Add comments for complex logic
- Use descriptive variable names

### Commit Messages

Use conventional commit format:

```
type(scope): description

[optional body]

[optional footer]
```

Types:

- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation
- `style`: Formatting
- `refactor`: Code restructuring
- `test`: Adding tests
- `chore`: Maintenance

Examples:

```
feat(parser): add support for range expressions
fix(typeck): resolve generic type inference bug
docs(readme): update installation instructions
```

### Pull Request Process

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests if applicable
5. Ensure all tests pass (`cargo test`)
6. Format code (`cargo fmt`)
7. Run Clippy (`cargo clippy`)
8. Commit your changes
9. Push to your fork
10. Open a Pull Request

### PR Checklist

- [ ] Code follows project style
- [ ] Tests added/updated
- [ ] All tests pass
- [ ] Documentation updated
- [ ] Commit messages follow convention
- [ ] No compiler warnings

## Areas for Contribution

### High Priority

- **Standard Library** — Expand built-in functions
- **Error Messages** — Improve error reporting
- **Optimization** — Performance improvements
- **Documentation** — More examples and guides

### Medium Priority

- **IDE Support** — Language Server Protocol (LSP)
- **Package Manager** — Dependency management
- **Debugger** — GDB/LLDB integration
- **Cross-compilation** — Multiple target platforms

### Low Priority

- **Syntax Highlighting** — Editor plugins
- **Benchmarks** — Performance testing suite
- **WebAssembly** — WASM backend
- **REPL** — Interactive interpreter

## Adding Language Features

### Adding a New Keyword

1. Add token to `lexer.rs`:

```rust
"newkeyword" => Token::NewKeyword,
```

1. Add AST node to `ast.rs`:

```rust
pub enum Stmt {
    // ...
    NewFeature { /* fields */ },
}
```

1. Add parser support in `parser.rs`:

```rust
Some(Token::NewKeyword) => self.parse_new_feature()?,
```

1. Add type checking in `typeck.rs`:

```rust
Stmt::NewFeature { .. } => {
    // Type check logic
}
```

1. Add code generation in `codegen.rs`:

```rust
Stmt::NewFeature { .. } => {
    // LLVM IR generation
}
```

1. Add tests and examples

### Adding a Built-in Function

1. Add to type checker (`typeck.rs`):

```rust
"newfunction" => {
    self.check_arg_count(name, args, 1, span.clone());
    Some(ResolvedType::ReturnType)
}
```

1. Add to code generator (`codegen.rs`):

```rust
"newfunction" => {
    // Implement function logic
    Ok(Some(result_value))
}
```

1. Add example usage
2. Update documentation

## Testing

### Unit Tests

Add unit tests in the same file:

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feature() {
        // Test code
    }
}
```

### Integration Tests

Add Apex programs to `examples/` and verify they compile:

```bash
cargo run -- check examples/test_feature.apex
cargo run -- run examples/test_feature.apex
```

### Test Coverage

Aim for:

- Parser: 80%+ coverage
- Type checker: 90%+ coverage
- Code generator: 70%+ coverage

## Documentation

### Code Documentation

Use Rust doc comments:

```rust
/// Brief description
///
/// # Arguments
///
/// * `param` - Parameter description
///
/// # Returns
///
/// Return value description
///
/// # Examples
///
/// ```
/// example_code();
/// ```
pub fn function_name(param: Type) -> ReturnType {
    // Implementation
}
```

### Language Documentation

Update README.md and create guides in `docs/`:

- Language features
- API reference
- Tutorials
- Best practices

## Getting Help

- **Issues** — Report bugs or request features
- **Discussions** — Ask questions or share ideas
- **Discord** — Join the community (link TBD)

## Code of Conduct

Be respectful and constructive. We're all here to build something great together.

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

---

Thank you for contributing to Apex! 🚀
