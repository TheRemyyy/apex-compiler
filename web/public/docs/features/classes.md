# Classes

Apex supports Object-Oriented Programming (OOP) via classes.

## Definition

```apex
class Point {
    // Fields
    public x: Integer;
    public y: Integer;
    
    // Constructor
    constructor(x: Integer, y: Integer) {
        this.x = x;
        this.y = y;
    }
    
    // Methods
    public function move(dx: Integer, dy: Integer): None {
        this.x = this.x + dx;
        this.y = this.y + dy;
        return None;
    }
}
```

## Visibility

- `public`: Accessible from anywhere.
- `private` (default): Accessible only within the class.
- `protected`: Accessible within the class and inherited classes.

```apex
class Account {
    private balance: Integer;
    
    constructor() {
        this.balance = 0;
    }
}
```

## Objects

Objects are instances of classes.

```apex
p: Point = Point(10, 20);
p.move(5, 5);
```

## Destructors

You can define a `destructor` to run code when an object is destroyed (goes out of scope).

```apex
class FileHandler {
    destructor() {
        println("File closed");
    }
}
```
