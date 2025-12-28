# Syntax

Apex syntax is inspired by C, C++, Rust, and TypeScript. It is designed to be familiar yet modern.

## Comments

```apex
// Single-line comment

/*
 * Multi-line comment
 * (Nested comments are not currently supported)
 */
```

## Blocks and Scoping

Apex uses curly braces `{}` to define blocks of code. Variables defined inside a block are scoped to that block.

```apex
function main(): None {
    // Outer scope
    x: Integer = 10;
    
    {
        // Inner scope
        y: Integer = 20;
        println("{x} {y}"); // Access outer and inner
    }
    
    // y is not accessible here
}
```

## Semicolons

Semicolons `;` are required at the end of statements.

```apex
x: Integer = 5; // Required
return None;    // Required
```

Some constructs like `if`, `while`, `function` definitions do not require a semicolon after their closing brace.

## Identifiers

Identifiers (variable names, function names) must start with a letter or underscore, followed by letters, numbers, or underscores.

- Valid: `name`, `_id`, `value2`, `camelCase`
- Invalid: `2name`, `class` (keyword)

## Code Style

- **Functions/Variables**: `camelCase` (recommended)
- **Types/Classes/Interfaces**: `PascalCase` (enforced by convention)
- **Constants**: `SCREAMING_SNAKE_CASE` (convention)
