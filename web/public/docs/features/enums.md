# Enums

Enums allow you to define a type by enumerating its possible variants. Apex enums are algebraic data types, meaning they can hold data.

## Basic Enums

```apex
enum Color {
    Red,
    Green,
    Blue
}
```

## Enums with Data

```apex
enum Message {
    Quit,
    Move(x: Integer, y: Integer),
    Write(String),
    ChangeColor(r: Integer, g: Integer, b: Integer)
}
```

## Pattern Matching with Enums

You use `match` to extract data from enums.

```apex
msg: Message = Message.Write("Hello");

match (msg) {
    Quit => { println("Quitting"); }
    Move(x, y) => { println("Moving to {x}, {y}"); }
    Write(s) => { println("Message: {s}"); }
    _ => { println("Other"); }
}
```
