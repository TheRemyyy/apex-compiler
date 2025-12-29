# Standard Library

The Apex Standard Library (`std`) provides core functionality for building applications. It is compiled into the binary by default.

## Modules

- [Math](math.md): Mathematical functions and constants.
- [String](string.md): String manipulation utilities.
- [Time](time.md): Time retrieval and sleeping.
- [Collections](collections.md): Built-in List and Map types.
- [I/O](io.md): Input and Output operations.
- [System](system.md): System-level interactions (exit, etc.).

> **Note**: The entire Standard Library is currently implemented as **compiler intrinsics**. This means functions like `Math.sqrt` or `String.length` are compiled directly to efficient LLVM instructions or C runtime calls with zero overhead. There are no external `.apex` module files for the standard library yet.
