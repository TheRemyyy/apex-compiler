#[inline]
fn idx(i: usize, j: usize, n: usize) -> usize {
    i * n + j
}

fn main() {
    let size: usize = 100;
    let total = size * size;

    let mut a = vec![0i32; total];
    let mut b = vec![0i32; total];
    let mut c = vec![0i32; total];

    for p in 0..total {
        a[p] = ((p as i32 * 17 + 13) % 97) - 48;
        b[p] = ((p as i32 * 31 + 7) % 89) - 44;
    }

    for i in 0..size {
        for j in 0..size {
            let mut sum: i64 = 0;
            for k in 0..size {
                sum += i64::from(a[idx(i, k, size)]) * i64::from(b[idx(k, j, size)]);
            }
            c[idx(i, j, size)] = sum as i32;
        }
    }

    let checksum: i64 = c.iter().map(|&x| i64::from(x)).sum();
    println!("{checksum}");
}
