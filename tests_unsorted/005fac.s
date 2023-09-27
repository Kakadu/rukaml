global main

extern print_int
section .text
main:
    mov rdi, 5    ; first arg
    call print_int

    push 6       ; 6! == 720
    call fac
    popq

    mov rdi, rax
    call print_int

    mov rdi, 0    ; PITFALL
    mov rax, 60   ; 'exit' syscall number
    syscall


global fac
fac:
    ;je lab_10
    mov rcx, 1
    mov rbx, [rsp+8]
loop:
    cmp rbx, 1
    je endloop
    imul rcx, rbx
    dec rbx
    jmp loop
endloop:
    mov rax, rcx
    ret
