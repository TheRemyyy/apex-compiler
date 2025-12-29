# System Module

The `System` module provides functions for interacting with the operating system and environment.

## Functions

The `System` object provides static methods for system-level operations.

### `System.getenv(name: String): String`

Retrieves the value of an environment variable. Returns an empty string if the variable is not set.

```apex
path: String = System.getenv("PATH");
println("Path: {path}");
```

### `System.shell(command: String): Integer`

Executes a command in the system shell and returns the exit code.

```apex
exitCode: Integer = System.shell("echo Hello");
```

### `System.exec(command: String): String`

Executes a command in the system shell and captures its standard output (stdout).

```apex
output: String = System.exec("whoami");
println("Current user: {output}");
```

### `System.cwd(): String`

Returns the current working directory.

```apex
currentDir: String = System.cwd();
println("Working in: {currentDir}");
```