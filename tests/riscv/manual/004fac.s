#
# a0-a2 - parameters to linux function services
# a7 - linux function number
#

.global _start      # Provide program starting address to linker

# Setup the parameters to print hello world
# and then call Linux to do it.

fac:
        mv      a5,a0
        li      a0,1
        li      a3,1
        ble     a5,a0,.L5
.L2:
        mv      a4,a5
        addiw   a5,a5,-1
        mulw    a0,a4,a0
        bne     a5,a3,.L2
.L5:
        ret

_start: addi  a0, x0, 1      # 1 = StdOut
        la    a1, helloworld # load address of helloworld
        addi  a2, x0, 13     # length of our string
        addi  a7, x0, 64     # linux write system call
        ecall                # Call linux to output the string

        li a0, 4
        call fac

# Setup the parameters to exit the program
# and then call Linux to do it.

        #addi    a0, x0, 0  # 3rd operand is an exit code
        addi    a7, x0, 93  # Service command code 93 terminates
        ecall               # Call linux to terminate the program

.data
helloworld:      .ascii "factorial is:\n"
