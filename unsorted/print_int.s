.global _start

.text
_start:
    la           s1, arr          # s1: load arr address
    addi         s2, zero, 3      # s2: arr length

    addi         sp, sp, -8       # push 1 item to stack
    sd           ra, 0(sp)        # save return address
    mv           s3, zero         # s3: i loop counter
    j            compare_ipos

L1:
    slli         s4, s3, 3        # s4: i * 8
    add          s5, s1, s4       # s5: address of a[i]
    ld           a0, 0(s5)        # a0: arr[i]
    jal          ra, num_print    # call num_print
    addi         s3, s3, 1        # increment i

compare_ipos:
    blt          s3, s2, L1       # loop if i < 3
    j            exit

num_print:
    addi         sp, sp, -40      # create stack space
    sd           s0, 32(sp)       # store frame pointer
    addi         s0, sp, 40       # new frame pointer

    addi         t0, zero, 0      # initialize sign_bit
    addi         t1, zero, 10     # divisor and new-line char
    addi         t2, s0, -16      # t2: string[n]
    add          a1, zero, t2     # a1: string[0] currently string[n]

    addi         t3, zero, '\n'   # '\n' char
    sb           t3, 0(a1)        # store '\n'

    bge          a0, zero, PVE    # if num >= 0 go to L1 else get absolute
    xori         a0, a0, -1       # (num ^ -1)
    addi         a0, a0, 1        # num + 1
    addi         t0, zero, 1      # set sign-bit to 1

PVE:
    remu         t3, a0, t1       # num % 10
    addi         t3, t3, 48       # convert to ascii
    addi         a1, a1, -1       # decrement start pointer
    sb           t3, 0(a1)        # store value
    divu         a0, a0, t1       # num /= 10
    blt          zero, a0, PVE    # if num > 0 loop

    beq          t0, zero, print  # if sign_bit = 0 go to print else, add '-' char
    addi         t3, zero, '-'    # ascii '-'
    addi         a1, a1, -1       # decrement start pointer
    sb           t3, 0(a1)        # store '-'

print:
    sub          t4, t2, a1       # t4: len -- string[n] - string[0]
    addi         a2, t4, 1        # len + 1
    addi         a0, zero, 1      # file descriptor to write to
    addi         a7, zero, 64     #  pk SYS_write
    ecall                         # transfer control to os

    ld           s0, 32(sp)       # restore frame pointer
    addi         sp, sp, 40       # restore stack pointer

    ret                           # return from function

exit:
    ld           ra, 0(sp)        # restore ra
    addi         sp, sp, 8        # pop stack

    addi         a0, zero, 0      # return value
    addi         a7, zero, 93     # syscall exit code
    ecall

.data
arr:
  .dword  12345670, -12345670, 0
