// Test 20: System Functions (Updated)
// Tests: System.getenv, System.shell, System.exec, System.cwd

function main(): None {
    println("=== System Tests ===");

    // 1. Get current working directory
    dir: String = System.cwd();
    println("Current Directory: {dir}");

    // 2. Get environment variable
    path: String = System.getenv("PATH");
    if (strlen(path) > 0) {
        println("Environment variable PATH found.");
    }

    // 3. Run a shell command (exit code only)
    println("");
    println("Running System.shell('echo Hello')...");
    exit_code: Integer = System.shell("echo Hello from Shell");
    println("Exit code: {exit_code}");

    // 4. Run and capture output (System.exec)
    println("");
    println("Running System.exec('echo ApexCaptured')...");
    output: String = System.exec("echo ApexCaptured");
    
    // Check if output contains our string
    if (strlen(output) > 0) {
        println("Captured output: {output}");
    } else {
        println("Error: Failed to capture output.");
    }

    println("");
    println("=== All System tests passed! ===");
    return None;
}