#include <stdio.h>

void print_int(int n) {
    printf("%s %d\n", __func__, n);
    fflush(stdout);
}