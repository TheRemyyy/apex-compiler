# Interfaces

Interfaces define a contract that classes can implement.

## Definition

```apex
interface Printable {
    function print_me(): None;
}
```

## Implementation

Classes implement interfaces explicitly via `implements`. The compiler validates that required methods exist with compatible signatures.

```apex
class Book implements Printable {
    title: String;
    
    constructor(title: String) {
        this.title = title;
    }
    
    function print_me(): None {
        println("Book: {this.title}");
        return None;
    }
}
```

## Interface Inheritance

Interfaces can extend other interfaces.

```apex
interface Named extends Printable {
    function get_name(): String;
}
```

## Polymorphism

You can use interfaces as types.

```apex
function display(item: Printable): None {
    item.print_me();
    return None;
}
```

See `examples/37_interfaces_contracts.apex` for a full contract + interface-typed-parameter example.
