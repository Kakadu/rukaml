        .global _start
        .data
message:
        .ascii  "hello, world!\n" # strlen = 14
        .text
_start:
        # write(1, message, 13)
        mov     $1, %rax                # system call 1 is write
        mov     $1, %rdi                # file handle 1 is stdout
        mov     $message, %rsi          # address of string to output
        mov     $0xe, %rdx              # number of bytes
        syscall                         # invoke operating system to do the write

        # exit(0)
        #mov     $60, %rax               # system call 60 is exit
        #xor     %rdi, %rdi              # we want return code 0
        #syscall                         # invoke operating system to exit
