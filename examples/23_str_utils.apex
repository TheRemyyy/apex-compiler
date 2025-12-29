// Test 23: String Utility Functions
// Tests: Str.upper, Str.lower, Str.trim, Str.contains, Str.len

function main(): None {
    println("=== Str Utility Tests ===");

    s: String = "  Apex Language  ";
    println("Original: '{s}'");

    // 1. Length
    len: Integer = Str.len(s);
    println("Length: {len}");

    // 2. Trim
    trimmed: String = Str.trim(s);
    println("Trimmed: '{trimmed}'");
    
    trimmedLen: Integer = Str.len(trimmed);
    println("Trimmed length: {trimmedLen}");

    // 3. Upper / Lower
    upper: String = Str.upper(trimmed);
    lower: String = Str.lower(trimmed);
    println("Upper: {upper}");
    println("Lower: {lower}");

    // 4. Contains
    if (Str.contains(trimmed, "Apex")) {
        println("Contains 'Apex': true");
    } else {
        println("Contains 'Apex': false");
    }

    if (Str.contains(trimmed, "Rust")) {
        println("Contains 'Rust': true");
    } else {
        println("Contains 'Rust': false");
    }

    // 5. Comparison
    if (Str.compare(lower, "apex language") == 0) {
        println("Lower case comparison: OK");
    }

    println("");
    println("=== All Str utility tests passed! ===");
    return None;
}
