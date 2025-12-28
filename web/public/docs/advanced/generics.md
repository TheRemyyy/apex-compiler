# Generics

Generics allow you to write flexible, reusable code that works with any type.

## Generic Functions

```apex
function identity<T>(x: T): T {
    return x;
}

val: Integer = identity<Integer>(5);
```

## Generic Classes

```apex
class Box<T> {
    value: T;
    
    constructor(value: T) {
        this.value = value;
    }
    
    function get(): T {
        return this.value;
    }
}
```

## Type Constraints

(Future Feature)
You will be able to constrain generics to types that implement specific interfaces.

```apex
// function printAll<T: Display>(item: T) { ... }
```
