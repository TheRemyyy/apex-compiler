# Modules

Modules organize code into namespaces.

## Definition

```apex
module Network {
    function connect(): None {
        println("Connecting...");
        return None;
    }
}
```

## Usage

Functions inside a module can be accessed using dot notation.
The compiler lowers `Module.function()` to an internal mangled symbol `Module__function`.

```apex
result: Integer = Math.square(5);
```

You can also alias imports:

```apex
import std.math as math;
import std.io as io;

value: Integer = math.abs(-5);
io.println("{value}");
```

Backward compatibility: direct `Module__function()` calls are still accepted.
