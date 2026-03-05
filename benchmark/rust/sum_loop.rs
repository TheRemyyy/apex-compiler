fn main() {
    let iterations: i64 = 50_000_000;
    let mod_val: i64 = 2_147_483_647;

    let mut x: i64 = 1;
    let mut acc: i64 = 0;

    for _ in 0..iterations {
        x = (x * 1_664_525 + 1_013_904_223) % mod_val;
        acc += x % 1024;
    }

    println!("{acc}");
}
