# Standard Library

The Apex Standard Library (`std`) provides core functionality for building applications. It is compiled into the binary by default.

## Modules

- [Math](math.md): Mathematical functions and constants.
- [Str](string.md): String manipulation utilities.
- [Time](time.md): Time retrieval and sleeping.
- [File](io.md): File system operations.
- [System](system.md): System-level interactions (exit, getenv, etc.).
- [Args](args.md): Command-line arguments.
- [Collections](collections.md): Built-in List and Map types.
- [I/O](io.md): Console input and output.

> **Note**: The entire Standard Library is currently implemented as **compiler intrinsics**. This means functions like `Math.sqrt` or `Str.length` are compiled directly to efficient LLVM instructions or C runtime calls with zero overhead. There are no external `.apex` module files for the standard library yet.
