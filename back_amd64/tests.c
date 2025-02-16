#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>

int add(int a, int b) {
    return a + b;
}

int subtract(int a, int b) {
    return a - b;
}

void test_add(void) {
    CU_ASSERT(add(2, 3) == 5);
    CU_ASSERT(add(-1, 1) == 0);
}

void test_subtract(void) {
    CU_ASSERT(subtract(5, 3) == 2);
    CU_ASSERT(subtract(-1, -1) == 0);
}

#include "rukaml_stdlib.h"

static void test_print_int(void) {
    rukaml_initialize(0L);
    void* clos = rukaml_alloc_closure(rukaml_print_int, 1);
    rukaml_applyN(clos, 1, 541);
    rukaml_apply1((void*)rukaml_print_int, 542);
    CU_ASSERT(1);
}

int main() {
    CU_initialize_registry();

    // CU_pSuite suite = CU_add_suite("MathTestSuite", 0, 0);
    // CU_add_test(suite, "test of add()", test_add);
    // CU_add_test(suite, "test of subtract()", test_subtract);

    {
        CU_pSuite suite = CU_add_suite("rukaml", 0, 0);
        CU_add_test(suite, "test of print_int()", test_print_int);
    }
    CU_basic_run_tests();
    CU_cleanup_registry();

    return 0;
}