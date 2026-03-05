#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

static inline int idx(int i, int j, int n) {
    return i * n + j;
}

int main(void) {
    const int size = 100;
    const int total = size * size;

    int64_t *a = (int64_t *)malloc((size_t)total * sizeof(int64_t));
    int64_t *b = (int64_t *)malloc((size_t)total * sizeof(int64_t));
    int64_t *c = (int64_t *)malloc((size_t)total * sizeof(int64_t));
    if (!a || !b || !c) {
        free(a);
        free(b);
        free(c);
        return 1;
    }

    for (int p = 0; p < total; p++) {
        a[p] = ((p * 17 + 13) % 97) - 48;
        b[p] = ((p * 31 + 7) % 89) - 44;
        c[p] = 0;
    }

    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            int64_t sum = 0;
            for (int k = 0; k < size; k++) {
                sum += a[idx(i, k, size)] * b[idx(k, j, size)];
            }
            c[idx(i, j, size)] = sum;
        }
    }

    int64_t checksum = 0;
    for (int q = 0; q < total; q++) {
        checksum += c[q];
    }

    free(a);
    free(b);
    free(c);

    printf("%lld\n", (long long)checksum);
    return 0;
}
