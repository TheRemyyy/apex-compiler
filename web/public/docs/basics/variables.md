# Variables

## Declaration

Variables in Apex are declared using the syntax `name: Type = value;`.

```apex
age: Integer = 30;
name: String = "Alice";
```

> **Note:** The `let` keyword is optional. Both `let x: Integer = 10;` and `x: Integer = 10;` are valid and equivalent.

## Mutability

By default, variables are **immutable**. Once assigned, their value cannot be changed.

```apex
x: Integer = 10;
// x = 20; // Error: Cannot assign to immutable variable
```

To make a variable mutable, use the `mut` keyword:

```apex
mut count: Integer = 0;
count = count + 1; // OK
```

## Shadowing

Apex supports variable shadowing. You can declare a new variable with the same name as a previous one.

```apex
x: Integer = 5;
x: Integer = x + 1; // New variable 'x' shadows the old one
```

This is often useful for type transformations:

```apex
input: String = "100";
input: Integer = to_int(input); // Shadowing with different type
```
