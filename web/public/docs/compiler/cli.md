# Compiler CLI Reference

The `apex-compiler` command-line interface.

## Usage

```bash
apex-compiler <command> [arguments] [flags]
```

## Commands

| Command | Description | Example |
| :--- | :--- | :--- |
| `check` | Checks code for errors without compiling. Checks types, borrows, etc. | `apex-compiler check main.apex` |
| `compile` | Compiles the source file into a native executable. | `apex-compiler compile main.apex` |
| `run` | Compiles and immediately executes the program. | `apex-compiler run main.apex` |
| `lex` | **Debug:** Outputs the stream of tokens from the lexer. | `apex-compiler lex main.apex` |
| `parse` | **Debug:** Outputs the Abstract Syntax Tree (AST). | `apex-compiler parse main.apex` |

## Flags

| Flag | Abbreviation | Description |
| :--- | :--- | :--- |
| `--output <path>` | `-o` | Specifies the output filename for the executable. |
| `--emit-llvm` | | Emits the LLVM IR (`.ll` file) instead of a binary. Useful for debugging codegen. |
| `--no-check` | | Skips the type checking and borrow checking phases. **Warning: Unsafe.** |
