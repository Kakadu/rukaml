#include <stdio.h>

void rukaml_print_int(int x)
{
    printf("%s %d\n", __func__, x);
    fflush(stdout);
}