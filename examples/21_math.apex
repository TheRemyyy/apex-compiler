// Test 21: Math Functions
// Tests: Math.random, Math.pi, Math.e

function main(): None {
    println("=== Math Tests ===");

    // 1. Constants
    pi: Float = Math.pi();
    e: Float = Math.e();
    println("PI: {pi}");
    println("E: {e}");

    // 2. Random numbers
    println("");
    println("Generating 5 random numbers (0.0 to 1.0):");
    for (i in 5) {
        rnd: Float = Math.random();
        println("  Random: {rnd}");
    }

    // 3. Simple math check
    circle_area: Float = pi * 5.0 * 5.0;
    println("");
    println("Area of circle with radius 5: {circle_area}");

    println("");
    println("=== All Math tests passed! ===");
    return None;
}
