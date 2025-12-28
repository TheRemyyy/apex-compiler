# Types

Apex is strongly and statically typed. Every variable must have a type known at compile time.

## Primitive Types

| Type | Description | Example |
|------|-------------|---------|
| `Integer` | 64-bit signed integer | `42`, `-1` |
| `Float` | 64-bit floating point | `3.14`, `-0.01` |
| `Boolean` | True or False | `true`, `false` |
| `Char` | Unicode character | `'a'`, `'🚀'` |
| `String` | UTF-8 encoded string | `"Hello"` |
| `None` | Unit type (empty value) | `None` |

### Integers

Currently, `Integer` is the primary integer type.

```apex
x: Integer = 100_000; // Underscores can be used for readability
```

### Floats

```apex
f: Float = 1.0;
// Note: implicit conversion from Integer to Float is not performed automatically in assignments
```

### Booleans

Used in conditional logic.

```apex
isValid: Boolean = true;
if (isValid) { ... }
```

### Strings

Strings are heap-allocated and UTF-8 encoded.

```apex
s: String = "Text";
```

### None

The `None` type represents the absence of a value, similar to `void` in C or `()` in Rust. It has a single value: `None`.

```apex
function doWork(): None {
    return None;
}
```

## Reference Types

Apex allows references to values.

- `&T`: Immutable reference.
- `&mut T`: Mutable reference.

See [Ownership and Borrowing](../advanced/ownership.md) for more details.

## Composite Types

- **Lists**: `List<T>`
- **Maps**: `Map<K, V>`
- **User-defined**: Classes, Enums, Interfaces.
