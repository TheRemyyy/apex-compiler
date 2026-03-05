fn is_prime(n: i64) -> bool {
    if n < 2 {
        return false;
    }
    if n == 2 {
        return true;
    }
    if n % 2 == 0 {
        return false;
    }

    let mut d = 3i64;
    while d * d <= n {
        if n % d == 0 {
            return false;
        }
        d += 2;
    }
    true
}

fn main() {
    let limit: i64 = 200_000;
    let mut count: i64 = 0;
    for n in 2..=limit {
        if is_prime(n) {
            count += 1;
        }
    }

    println!("{count}");
}
