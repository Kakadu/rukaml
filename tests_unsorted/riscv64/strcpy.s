# https://github.com/riscv/riscv-v-spec/blob/master/v-spec.adoc
.text
.balign 4
.global strcpy
.global strlen
.global main

# size_t strlen(const char *str)
# a0 holds *str

strlen:
    mv a3, a0             # Save start
loop:
    vsetvli a1, x0, e8, m8, ta, ma  # Vector of bytes of maximum length
    vle8ff.v v8, (a3)     # Load bytes
    csrr a1, vl           # Get bytes read
    vmseq.vi v0, v8, 0    # Set v0[i] where v8[i] = 0
    vfirst.m a2, v0       # Find first set bit
    add a3, a3, a1        # Bump pointer
    bltz a2, loop         # Not found?

    add a0, a0, a1        # Sum start + bump
    add a3, a3, a2        # Add index
    sub a0, a3, a0        # Subtract start address+bump

    ret

# char* strcpy(char *dst, const char* src)
strcpy:
    mv a2, a0             # Copy dst
    li t0, -1             # Infinite AVL (application vector length)
loop_strcpy:
    vsetvli x0, t0, e8, m8, ta, ma  # Max length vectors of bytes
    vle8ff.v v8, (a1)        # Get src bytes
    csrr t1, vl              # Get number of bytes fetched
    vmseq.vi v1, v8, 0       # Flag zero bytes
    vfirst.m a3, v1          # Zero found?
    add a1, a1, t1           # Bump pointer
    vmsif.m v0, v1           # Set mask up to and including zero byte.
    vse8.v v8, (a2), v0.t    # Write out bytes
    add a2, a2, t1           # Bump pointer
    bltz a3, loop_strcpy     # Zero byte not found, so loop

    ret
main:
    la    a0, helloworld
    call print_string
    call print_newline
    la    a0, helloworld
    call strlen
    call print_int
    call print_newline

    la    a0, str2
    call print_string
    call print_newline
    la    a0, str2
    call strlen
    call print_int
    call print_newline

    la    a0, str2
    la    a1, helloworld
    call strcpy

    la    a0, str2
    call print_string
    call print_newline
    
    li      a0, 0   # Use 0 return code
    li      a7, 93  # Service command code 93 terminates
    ecall           # Call linux to terminate the program

.data
helloworld:      .ascii "Hello World!\0"
str2:            .ascii "01234567890123456789\0"
