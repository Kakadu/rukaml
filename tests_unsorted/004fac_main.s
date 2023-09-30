global _start
extern fac
section .text
_start:
    mov rdi, 5
    call fac
    ; 5! is stored in rax
    mov rdi, rax
    mov rax, 60   ; 'exit' syscall number
    syscall
; PITFALL

; Silence linker's warning about executable stack
section .note.GNU-stack noalloc noexec nowrite progbits
