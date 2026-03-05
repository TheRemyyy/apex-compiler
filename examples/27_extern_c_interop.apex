// Test 27: C Interop (extern function)
// Tests: extern function declarations and direct C ABI calls

import std.io.*;

extern function puts(msg: String): Integer;
extern function strlen(msg: String): Integer;

function main(): None {
    println("=== C Interop Tests ===");

    len: Integer = strlen("hello from apex extern");

    println("strlen(message) = {len}");
    puts("hello from apex extern");

    println("Extern calls executed.");
    return None;
}
