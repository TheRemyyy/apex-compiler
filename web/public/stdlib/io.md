# Input / Output (I/O)

Apex provides built-in functions for console I/O operations. These are compiler intrinsics that map directly to system calls.

## Output Functions

### `print(message: String): None`

Prints a message to standard output *without* a newline character at the end.

```apex
print("Hello, ");
println("World!"); // Output: Hello, World!
```

### `println(message: String): None`

Prints a message to standard output followed by a newline character.

```apex
println("Status: OK");
```

## Input Functions

### `read_line(): String`

Reads a line of text from standard input (stdin). Returns the string including the newline character (if present).

```apex
print("Enter name: ");
name: String = read_line();
println("Hello, " + name);
```
