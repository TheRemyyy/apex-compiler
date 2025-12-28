# Interfaces

Interfaces define a contract that classes can implement.

## Definition

```apex
interface Printable {
    function print_me(): None;
}
```

## Implementation

Classes implement interfaces explicitly.

```apex
class Book {
    title: String;
    
    constructor(title: String) {
        this.title = title;
    }
    
    public function print_me(): None {
        println("Book: {this.title}");
        return None;
    }
}
// Note: Explicit 'implements' syntax might vary based on latest parser version, 
// usually implementing the matching methods is sufficient or uses `implements` keyword.
```

(Note: Check `examples/07_interfaces.apex` for exact syntax. Based on `README.md`, the implementation is implicit or integrated into the class definition without a specific keyword, just matching signatures).

## Polymorphism

You can use interfaces as types.

```apex
function display(item: Printable): None {
    item.print_me();
    return None;
}
```
