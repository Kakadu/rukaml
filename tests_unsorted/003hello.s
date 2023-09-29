global _start

section .data
message: db 'hello, world!', 10

section .text
_start:
    ; system call number should be stored in rax
    mov rax, 1
    ; argument #1 in rdi: where to write (descriptor)?
    mov rdi, 1
    ; argument #2 in rsi: where does the string start?
    mov rsi, message
    ; argument #3 in rdx: how many bytes to write?
    mov rdx, 14
    ; this instruction invokes a system call
    syscall

    mov rax, 60         ; 'exit' syscall number
    xor rdi, rdi
    syscall
