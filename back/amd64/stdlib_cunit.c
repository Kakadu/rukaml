#include <CUnit/CUnit.h>
#include <CUnit/Basic.h>
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

    {
        CU_pSuite suite = CU_add_suite("rukaml", 0, 0);
        CU_add_test(suite, "test of print_int()", test_print_int);
    }
    CU_basic_run_tests();

    CU_cleanup_registry();

    return 0;
}
