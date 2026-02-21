// Main without import - should it find helper()?
package main;

// NO import statement!

function main(): None {
    // Try to use helper without import
    result: String = helper();
    println(result);
    return None;
}
