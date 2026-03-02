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

Backward compatibility: direct `Module__function()` calls are still accepted.
