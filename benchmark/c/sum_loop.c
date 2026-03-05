#include <stdint.h>
#include <stdio.h>

int main(void) {
    const int64_t iterations = 50000000;
    const int64_t mod_val = 2147483647;

    int64_t x = 1;
    int64_t acc = 0;

    for (int64_t i = 0; i < iterations; i++) {
        x = (x * 1664525 + 1013904223) % mod_val;
        acc += (x % 1024);
    }

    printf("%lld\n", (long long)acc);
    return 0;
}
