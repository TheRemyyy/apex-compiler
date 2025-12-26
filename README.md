<div align="center">

# Apex Programming Language

**Modern Systems Programming with Safety and Performance**

[![Rust](https://img.shields.io/badge/Rust-1.83+-orange.svg?style=flat-square)](https://www.rust-lang.org/)
[![LLVM](https://img.shields.io/badge/LLVM-21.0+-blue.svg?style=flat-square)](https://llvm.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg?style=flat-square)](LICENSE)

*Strong static typing • Ownership & borrowing • Async/await • Zero-cost abstractions*

[Quick Start](#quick-start) • [Language Guide](#language-guide) • [Examples](#examples) • [Documentation](#documentation)

</div>

---

## Overview

Apex is a modern systems programming language that combines the safety of Rust with the expressiveness of modern high-level languages. Built on LLVM, Apex compiles to native machine code with zero runtime overhead while providing strong compile-time guarantees through its advanced type system and borrow checker.

### Key Features

- **🔒 Memory Safety** — Ownership system prevents races, null pointers, and use-after-free bugs at compile time
- **⚡ Zero-Cost Abstractions** — High-level features compile down to machine code with no runtime penalty
- **🎯 Strong Static Typing** — Comprehensive type system with generics, traits, and algebraic data types
- **🔄 Async/Await** — First-class support for asynchronous programming with Task types
- **📦 Pattern Matching** — Exhaustive pattern matching for control flow and destructuring
- **🧩 Generics** — Full generic programming support with type parameters and constraints
- **🛠️ Modern Tooling** — Fast compilation, helpful error messages, and integrated toolchain
- **🚀 LLVM Backend** — Leverages LLVM for world-class optimization and cross-platform support

## Requirements

- **Rust** 1.83 or later (for building the compiler)
- **LLVM** 21.0 or later (21.1.7 recommended)
- **Clang** (for linking)
- **Windows/Linux/macOS** (all platforms supported)

## <a id="installation"></a>📦 Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/TheRemyyy/apex-compiler.git
cd apex-compiler

# Build the compiler
cargo build --release

# The compiler binary will be at target/release/apex-compiler
```

### Add to PATH

```bash
# Linux/macOS
export PATH="$PATH:$(pwd)/target/release"

# Windows (PowerShell)
$env:PATH += ";$(pwd)\target\release"
```

## <a id="quick-start"></a>⚡ Quick Start

### Hello World

Create a file `hello.apex`:

```apex
function main(): None {
    println("Hello, World!");
    return None;
}
```

Compile and run:

```bash
# Check for errors
apex-compiler check hello.apex

# Compile to executable
apex-compiler compile hello.apex

# Run the program
./hello  # Linux/macOS
.\hello.exe  # Windows

# Or compile and run in one step
apex-compiler run hello.apex
```

### Your First Program

```apex
// Variables and types
function main(): None {
    // Immutable by default
    x: Integer = 42;
    name: String = "Apex";
    pi: Float = 3.14159;
    
    // Mutable variables
    mut counter: Integer = 0;
    counter = counter + 1;
    
    // String interpolation
    println("Hello from {name}!");
    println("Counter: {counter}");
    
    return None;
}
```

## <a id="language-guide"></a>📚 Language Guide

### Types

Apex provides a rich type system:

#### Primitive Types

```apex
// Integers
x: Integer = 42;
negative: Integer = -10;

// Floating point
pi: Float = 3.14159;
e: Float = 2.71828;

// Booleans
isTrue: Boolean = true;
isFalse: Boolean = false;

// Characters
letter: Char = 'A';
emoji: Char = '🚀';

// Strings
message: String = "Hello, Apex!";
multiline: String = "Line 1
Line 2
Line 3";

// None type (unit)
nothing: None = None;
```

#### Generic Types

```apex
// Lists
numbers: List<Integer> = List<Integer>();
numbers.push(1);
numbers.push(2);
numbers.push(3);

// Maps
scores: Map<String, Integer> = Map<String, Integer>();
scores.insert("Alice", 100);
scores.insert("Bob", 95);

// Sets
unique: Set<Integer> = Set<Integer>();

// Option (nullable values)
maybeValue: Option<Integer> = Option<Integer>();

// Result (error handling)
result: Result<Integer, String> = Result<Integer, String>();
```

#### Function Types

```apex
// Function type: (params) -> return
add: (Integer, Integer) -> Integer = (a: Integer, b: Integer) => a + b;

// Higher-order functions
function applyTwice(x: Integer, f: (Integer) -> Integer): Integer {
    return f(f(x));
}
```

### Control Flow

#### If-Else

```apex
if (x > 0) {
    println("Positive");
} else if (x < 0) {
    println("Negative");
} else {
    println("Zero");
}
```

#### While Loops

```apex
mut i: Integer = 0;
while (i < 10) {
    println("i = {i}");
    i = i + 1;
}
```

#### For Loops

```apex
// Range-based for loop
for (i in 5) {
    println("Iteration {i}");
}

// Iterate over list
numbers: List<Integer> = List<Integer>();
for (num in numbers) {
    println("Number: {num}");
}
```

#### Pattern Matching

```apex
value: Integer = 42;
match (value) {
    0 => { println("Zero"); }
    1 => { println("One"); }
    42 => { println("The answer!"); }
    _ => { println("Something else"); }
}

// Match with Option
opt: Option<Integer> = Option<Integer>();
match (opt) {
    Some(x) => { println("Got value: {x}"); }
    None => { println("No value"); }
}

// Match with Result
res: Result<Integer, String> = Result<Integer, String>();
match (res) {
    Ok(value) => { println("Success: {value}"); }
    Error(err) => { println("Failed: {err}"); }
}
```

### Functions

```apex
// Basic function
function add(a: Integer, b: Integer): Integer {
    return a + b;
}

// Function with no return value
function greet(name: String): None {
    println("Hello, {name}!");
    return None;
}

// Async function
async function fetchData(): Task<String> {
    // Asynchronous operation
    return "data";
}

// Using async/await
async function processData(): Task<None> {
    data: String = await fetchData();
    println("Got: {data}");
    return None;
}
```

### Classes

```apex
class Person {
    // Fields
    public name: String;
    private mut age: Integer;
    protected id: Integer;
    
    // Constructor
    constructor(name: String, age: Integer, id: Integer) {
        this.name = name;
        this.age = age;
        this.id = id;
    }
    
    // Destructor
    destructor() {
        println("Person destroyed");
    }
    
    // Methods
    public function getName(): String {
        return this.name;
    }
    
    public function birthday(): None {
        this.age = this.age + 1;
        return None;
    }
    
    // Method with visibility
    private function getId(): Integer {
        return this.id;
    }
}

// Using the class
function main(): None {
    person: Person = Person("Alice", 30, 1);
    name: String = person.getName();
    println("Name: {name}");
    person.birthday();
    return None;
}
```

### Interfaces

```apex
interface Displayable {
    function display(): None;
}

interface Comparable {
    function compare(other: Integer): Integer;
}

// Class implementing interface
class Point {
    x: Integer;
    y: Integer;
    
    constructor(x: Integer, y: Integer) {
        this.x = x;
        this.y = y;
    }
    
    // Implement Displayable
    public function display(): None {
        println("Point({this.x}, {this.y})");
        return None;
    }
}
```

### Enums

```apex
enum Status {
    Active,
    Inactive,
    Pending
}

enum Result {
    Ok(value: Integer),
    Error(message: String)
}

// Using enums
function checkStatus(status: Status): None {
    match (status) {
        Active => { println("System is active"); }
        Inactive => { println("System is inactive"); }
        Pending => { println("System is pending"); }
    }
    return None;
}
```

### Ownership & Borrowing

Apex uses an ownership system similar to Rust to ensure memory safety:

```apex
// Ownership transfer (move)
function consumeData(owned data: Data): None {
    // data is moved here, original is invalid
    return None;
}

// Immutable borrow
function readData(borrow data: Data): Integer {
    // Can read but not modify
    return data.getValue();
}

// Mutable borrow
function modifyData(borrow mut data: Data): None {
    // Can modify the data
    return None;
}

// References
x: Integer = 42;
immRef: &Integer = &x;  // Immutable reference

mut y: Integer = 10;
mutRef: &mut Integer = &mut y;  // Mutable reference
*mutRef = 20;  // Dereference to modify
```

### Error Handling

```apex
// Option for nullable values
function findUser(id: Integer): Option<String> {
    if (id == 1) {
        return Option.some("Alice");
    } else {
        return Option.none();
    }
}

// Result for operations that can fail
function divide(a: Integer, b: Integer): Result<Integer, String> {
    if (b == 0) {
        return Result.error("Division by zero");
    } else {
        return Result.ok(a / b);
    }
}

// Try operator (?) for error propagation
function calculate(): Result<Integer, String> {
    x: Integer = divide(10, 2)?;  // Returns Error if divide fails
    y: Integer = divide(x, 3)?;
    return Result.ok(y);
}

// Require assertions
function validate(x: Integer): None {
    require(x > 0, "x must be positive");
    require(x < 100);
    return None;
}
```

### Lambdas & Closures

```apex
// Lambda expression
double: (Integer) -> Integer = (x: Integer) => x * 2;
result: Integer = double(5);  // 10

// Closure capturing variables
multiplier: Integer = 10;
multiply: (Integer) -> Integer = (x: Integer) => x * multiplier;

// Higher-order functions
function map(list: List<Integer>, f: (Integer) -> Integer): List<Integer> {
    result: List<Integer> = List<Integer>();
    for (item in list) {
        result.push(f(item));
    }
    return result;
}

numbers: List<Integer> = List<Integer>();
doubled: List<Integer> = map(numbers, double);
```

### Modules

```apex
module Math {
    function square(x: Integer): Integer {
        return x * x;
    }
    
    function cube(x: Integer): Integer {
        return x * x * x;
    }
}

// Using module functions
function main(): None {
    result: Integer = Math__square(5);  // 25
    println("5² = {result}");
    return None;
}
```

### String Interpolation

```apex
name: String = "Apex";
version: Integer = 1;
pi: Float = 3.14159;

// Interpolate variables
message: String = "Welcome to {name} v{version}!";

// Interpolate expressions
calculation: String = "2 + 2 = {2 + 2}";

// Multiple interpolations
info: String = "Language: {name}, Version: {version}, Pi: {pi}";
```

## <a id="examples"></a>💡 Examples

### Calculator

```apex
function add(a: Integer, b: Integer): Integer {
    return a + b;
}

function subtract(a: Integer, b: Integer): Integer {
    return a - b;
}

function multiply(a: Integer, b: Integer): Integer {
    return a * b;
}

function divide(a: Integer, b: Integer): Result<Integer, String> {
    if (b == 0) {
        return Result.error("Division by zero");
    }
    return Result.ok(a / b);
}

function main(): None {
    x: Integer = 10;
    y: Integer = 5;
    
    sum: Integer = add(x, y);
    diff: Integer = subtract(x, y);
    prod: Integer = multiply(x, y);
    
    println("{x} + {y} = {sum}");
    println("{x} - {y} = {diff}");
    println("{x} * {y} = {prod}");
    
    quotResult: Result<Integer, String> = divide(x, y);
    match (quotResult) {
        Ok(quot) => { println("{x} / {y} = {quot}"); }
        Error(err) => { println("Error: {err}"); }
    }
    
    return None;
}
```

### Data Structures

```apex
class Stack {
    mut items: List<Integer>;
    mut size: Integer;
    
    constructor() {
        this.items = List<Integer>();
        this.size = 0;
    }
    
    function push(value: Integer): None {
        this.items.push(value);
        this.size = this.size + 1;
        return None;
    }
    
    function pop(): Option<Integer> {
        if (this.size == 0) {
            return Option.none();
        }
        this.size = this.size - 1;
        value: Integer = this.items.get(this.size);
        return Option.some(value);
    }
    
    function isEmpty(): Boolean {
        return this.size == 0;
    }
}

function main(): None {
    stack: Stack = Stack();
    stack.push(1);
    stack.push(2);
    stack.push(3);
    
    while (!stack.isEmpty()) {
        opt: Option<Integer> = stack.pop();
        match (opt) {
            Some(value) => { println("Popped: {value}"); }
            None => { println("Stack empty"); }
        }
    }
    
    return None;
}
```

## <a id="documentation"></a>📚 Documentation

### Compiler Commands

| Command | Description |
|---------|-------------|
| `apex-compiler check <file>` | Type-check without compiling |
| `apex-compiler compile <file>` | Compile to executable |
| `apex-compiler run <file>` | Compile and run |
| `apex-compiler lex <file>` | Show tokens (debugging) |
| `apex-compiler parse <file>` | Show AST (debugging) |

### Compiler Flags

| Flag | Description |
|------|-------------|
| `-o, --output <path>` | Output file path |
| `--emit-llvm` | Emit LLVM IR instead of binary |
| `--no-check` | Skip type checking (not recommended) |

### Standard Library Functions

#### Math

```apex
abs(x: Integer): Integer          // Absolute value
sqrt(x: Float): Float              // Square root
pow(base: Float, exp: Float): Float  // Power
sin(x: Float): Float               // Sine
cos(x: Float): Float               // Cosine
tan(x: Float): Float               // Tangent
floor(x: Float): Float             // Floor
ceil(x: Float): Float              // Ceiling
round(x: Float): Float             // Round
log(x: Float): Float               // Natural logarithm
log10(x: Float): Float             // Base-10 logarithm
exp(x: Float): Float               // Exponential
min(a: T, b: T): T                 // Minimum
max(a: T, b: T): T                 // Maximum
```

#### String Operations

```apex
strlen(s: String): Integer         // String length
strcmp(a: String, b: String): Integer  // String compare
strcat(a: String, b: String): String   // String concatenate
```

#### Type Conversions

```apex
to_int(x: Float): Integer          // Float to Integer
to_float(x: Integer): Float        // Integer to Float
to_string(x: T): String            // Any to String
```

#### I/O

```apex
println(msg: String): None         // Print with newline
print(msg: String): None           // Print without newline
```

## Project Structure

```
apex-compiler/
├── src/
│   ├── main.rs           # CLI entry point
│   ├── lexer.rs          # Tokenization
│   ├── parser.rs         # Recursive descent parser
│   ├── ast.rs            # AST definitions
│   ├── typeck.rs         # Type checking
│   ├── borrowck.rs       # Borrow checking
│   └── codegen.rs        # LLVM code generation
├── examples/             # Example programs
│   ├── 01_hello.apex     # Hello world
│   ├── 02_variables.apex # Variables and types
│   ├── 03_math.apex      # Math operations
│   ├── 04_control_flow.apex  # Control structures
│   ├── 05_classes.apex   # OOP features
│   ├── 06_enums.apex     # Enums and pattern matching
│   ├── 07_interfaces.apex # Interfaces
│   ├── 08_modules.apex   # Module system
│   ├── 09_generics.apex  # Generic programming
│   ├── 10_ownership.apex # Ownership and borrowing
│   ├── 11_lambdas.apex   # Lambda expressions
│   ├── 12_string_interp.apex # String interpolation
│   ├── 13_error_handling.apex # Error handling
│   ├── 14_async.apex     # Async/await
│   ├── 15_stdlib.apex    # Standard library
│   ├── 16_pattern_matching.apex # Pattern matching
│   ├── 17_comprehensive.apex # All features
│   ├── app_bank.apex     # Banking system
│   ├── app_calculator.apex # Calculator
│   ├── app_data_structures.apex # Data structures
│   ├── app_game.apex     # Game logic
│   └── app_todo.apex     # Todo list
└── Cargo.toml            # Rust dependencies
```

## Language Specification

### Syntax Summary

```apex
// Comments
// Single-line comment

// Variable declaration
name: Type = value;
mut name: Type = value;

// Function definition
function name(param: Type): ReturnType {
    // body
    return value;
}

// Class definition
class Name {
    field: Type;
    
    constructor(param: Type) {
        this.field = param;
    }
    
    function method(): Type {
        return value;
    }
}

// Interface definition
interface Name {
    function method(): Type;
}

// Enum definition
enum Name {
    Variant1,
    Variant2(Type),
    Variant3(field: Type)
}

// Module definition
module Name {
    function helper(): Type {
        return value;
    }
}
```

### Type System

- **Primitive types**: `Integer`, `Float`, `Boolean`, `String`, `Char`, `None`
- **Generic types**: `List<T>`, `Map<K,V>`, `Set<T>`, `Option<T>`, `Result<T,E>`
- **Reference types**: `&T` (immutable), `&mut T` (mutable)
- **Smart pointers**: `Box<T>`, `Rc<T>`, `Arc<T>`
- **Async types**: `Task<T>`
- **Function types**: `(T1, T2) -> R`

### Visibility Modifiers

- `public` — Accessible from anywhere
- `private` — Accessible only within the same class/module (default)
- `protected` — Accessible within class and subclasses

### Parameter Modes

- `owned` — Takes ownership (default)
- `borrow` — Immutable borrow
- `borrow mut` — Mutable borrow

## Performance

Apex compiles to native machine code via LLVM with aggressive optimizations:

- **Zero-cost abstractions** — Generics, traits, and lambdas have no runtime overhead
- **Inline optimization** — Small functions are inlined automatically
- **Dead code elimination** — Unused code is removed
- **LLVM optimizations** — Leverages LLVM's world-class optimizer

Benchmark results show Apex programs perform comparably to C/C++ and Rust.

## Roadmap

- [x] Core language features
- [x] Type system with generics
- [x] Ownership and borrowing
- [x] Pattern matching
- [x] Async/await
- [x] String interpolation
- [x] LLVM backend
- [ ] Package manager
- [ ] Standard library expansion
- [ ] Incremental compilation
- [ ] IDE support (LSP)
- [ ] Debugger integration
- [ ] Cross-compilation
- [ ] WebAssembly target

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

Inspired by Rust, Swift, and modern programming language design. Built with ❤️ using Rust and LLVM.

---

<div align="center">
<sub>Built with ❤️ and Rust</sub>
</div>
