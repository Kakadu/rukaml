#include <stdio.h>

void print_int(int n) {
    printf("%d", n);
    fflush(stdout);
}
void print_newline(void) {
    printf("\n");
    fflush(stdout);
}
void print_string(const char* const s) {
    printf("%s", s);
    fflush(stdout);
}