#include <stdint.h>

int fac(int n) {
    return (n<=1) ? 1 : n * fac (n-1);
}