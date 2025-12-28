# Error Handling

Apex prefers explicit error handling over exceptions.

## Option<T>

Represents a value that might be present or missing.

```apex
enum Option<T> {
    Some(T),
    None
}
```

Usage:

```apex
function findParams(id: Integer): Option<String> {
    if (id == 0) {
        return Option.None;
    }
    return Option.Some("Param");
}
```

## Result<T, E>

Represents a success (`Ok`) or failure (`Error`).

```apex
enum Result<T, E> {
    Ok(T),
    Error(E)
}
```

Usage:

```apex
function divide(a: Integer, b: Integer): Result<Integer, String> {
    if (b == 0) {
        return Result.Error("Division by zero");
    }
    return Result.Ok(a / b);
}
```

## The `?` Operator

The `?` operator simplifies error propagation. If a Result is `Error`, it returns early.

```apex
function computation(): Result<Integer, String> {
    val: Integer = divide(10, 2)?; // Unwraps or returns Error
    val2: Integer = divide(val, 0)?; // Returns Error("Division by zero")
    return Result.Ok(val2);
}
```
