# Functions

Functions are the building blocks of Apex programs.

## Definition

A function is defined using the `function` keyword.

```apex
function name(param1: Type1, param2: Type2): ReturnType {
    // body...
    return value;
}
```

Example:

```apex
function add(a: Integer, b: Integer): Integer {
    return a + b;
}
```

## Return Values

If a function does not return a meaningful value, it should return `None` and the return type should be `None`.

```apex
function greet(): None {
    println("Hello");
    return None;
}
```

## Lambdas (Anonymous Functions)

Apex supports lambda expressions for concise function definition.

Type: `(ParamTypes) -> ReturnType`

```apex
// Implicit return
square: (Integer) -> Integer = (x: Integer) => x * x;

// Explicit block
complex: (Integer) -> Integer = (x: Integer) => {
    y: Integer = x * 2;
    return y + 1;
};
```

## Higher-Order Functions

Functions can take other functions as arguments or return them.

```apex
function callTwice(f: (Integer) -> None, val: Integer): None {
    f(val);
    f(val);
    return None;
}
```

## Closures

Lambdas can capture variables from their enclosing environment.

```apex
offset: Integer = 10;
adder: (Integer) -> Integer = (x: Integer) => x + offset;
```
