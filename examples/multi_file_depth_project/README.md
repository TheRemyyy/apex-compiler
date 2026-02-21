# Java-Style Namespace Example

This example demonstrates Java-style package and import system.

## Project Structure

```
java_style_project/
├── apex.toml
└── src/
    ├── utils/
    │   ├── math.apex      # package utils.math
    │   └── strings.apex   # package utils.strings
    └── main.apex          # package main (entry)
```

## Package Declaration

Each file declares its package at the top:

```apex
// src/utils/math.apex
package utils.math;

function factorial(n: Integer): Integer {
    // ...
}
```

## Import Styles

### 1. Import Specific Functions

```apex
import utils.math.factorial;
import utils.math.power;
```

### 2. Wildcard Import

```apex
import utils.strings.*;
```

### 3. Usage

Imported functions are used directly by name:

```apex
result: Integer = factorial(5);
greeting: String = greet("World");
```

## How It Works

1. **Package = Namespace**: `package utils.math` creates namespace `utils.math`
2. **Function Mangling**: `utils.math.factorial` → `utils__math__factorial` in LLVM
3. **Imports**: Compiler resolves imports and generates correct function names
4. **Wildcard**: `import utils.strings.*` imports all exported functions

## Running

```bash
cd multi_file_depth_project
apex run
```

## Comparison with Java

| Java | Apex |
|------|------|
| `package com.example;` | `package utils.math;` |
| `import com.example.Utils;` | `import utils.math.factorial;` |
| `import com.example.*;` | `import utils.strings.*;` |
| `Utils.method()` | `method()` (direct) |

## Notes

- All functions are "public" by default (for now)
- No `mod.rs` needed (unlike Rust)
- Simple, predictable, Java-like behavior
