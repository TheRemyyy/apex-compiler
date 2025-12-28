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

Functions inside a module are accessed using double underscore `__` (temporary syntax for this version) or dot notation depending on implementation.

*Based on `README.md`:*

```apex
result: Integer = Math__square(5); 
```

This suggests a specific mangling or access pattern currently. Please verify with `examples/08_modules.apex`.

The official intended syntax for future releases is `Module.function()`.

```apex
// Future / Ideal Syntax
Network.connect();
```
