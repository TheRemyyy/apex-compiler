# Multi-File Project Example

This example demonstrates how to organize an Apex project with multiple source files.

## Project Structure

```
.
├── apex.toml          # Project configuration
├── src/
│   ├── math_utils.apex    # Mathematical utilities
│   ├── string_utils.apex  # String manipulation utilities
│   └── main.apex          # Entry point with main function
└── README.md
```

## Configuration (apex.toml)

```toml
name = "multi_file_demo"
version = "1.0.0"
entry = "src/main.apex"        # Entry point file
files = [                       # All source files
    "src/math_utils.apex",
    "src/string_utils.apex",
    "src/main.apex"
]
output = "multi_file_demo"     # Output binary name
opt_level = "3"                # Optimization level
```

## Commands

```bash
# Build the project
apex build

# Build and run
apex run

# Show project info
apex info

# Check for errors
apex check
```

## Notes

- All files listed in `files` are compiled together
- The `entry` file must contain the `main()` function
- Functions from all files are available globally (no explicit imports needed yet)
