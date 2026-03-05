#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

static bool is_prime(int n) {
    if (n < 2) {
        return false;
    }
    if (n == 2) {
        return true;
    }
    if ((n % 2) == 0) {
        return false;
    }

    for (int d = 3; (int64_t)d * d <= n; d += 2) {
        if ((n % d) == 0) {
            return false;
        }
    }
    return true;
}

int main(void) {
    const int limit = 200000;
    int64_t count = 0;
    for (int n = 2; n <= limit; n++) {
        if (is_prime(n)) {
            count++;
        }
    }
    printf("%lld\n", (long long)count);
    return 0;
}
