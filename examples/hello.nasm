section .data
str_0: db 10, 0
str_1: db "Hello, World!", 0

section .bss
__dstack__ resq 65536
__global_vars__ resq 1
__local_vars__ resq 1024

section .text
global _start

__print_int__:
    push rbp
    mov rbp, rsp
    sub rsp, 48
    mov DWORD [rbp-36], edi
    mov DWORD [rbp-4], 1
    mov eax, DWORD [rbp-4]
    cdqe
    mov edx, 16
    sub rdx, rax
    mov BYTE [rbp-32+rdx], 10
.L2:
    mov edx, DWORD [rbp-36]
    movsx rax, edx
    imul rax, rax, 1717986919
    shr rax, 32
    mov ecx, eax
    sar ecx, 2
    mov eax, edx
    sar eax, 31
    sub ecx, eax
    mov eax, ecx
    sal eax, 2
    add eax, ecx
    add eax, eax
    mov ecx, edx
    sub ecx, eax
    mov eax, ecx
    lea ecx, [rax+48]
    mov eax, DWORD [rbp-4]
    lea edx, [rax+1]
    mov DWORD [rbp-4], edx
    cdqe
    mov edx, 15
    sub rdx, rax
    mov eax, ecx
    mov BYTE [rbp-32+rdx], al
    mov eax, DWORD [rbp-36]
    movsx rdx, eax
    imul rdx, rdx, 1717986919
    shr rdx, 32
    mov ecx, edx
    sar ecx, 2
    cdq
    mov eax, ecx
    sub eax, edx
    mov DWORD [rbp-36], eax
    cmp DWORD [rbp-36], 0
    jne .L2
    mov eax, DWORD [rbp-4]
    cdqe
    mov edx, 16
    sub rdx, rax
    lea rax, [rbp-32]
    lea rcx, [rax+rdx]
    mov eax, DWORD [rbp-4]
    mov edx, eax
    mov rsi, rcx
    mov edi, 1
    mov eax, 0
    mov rax, 1
    syscall
    nop
    leave
    ret

irchar!!:
    push r13
    lea r13, [__local_vars__]
    sub r12, 8
    mov rbx, [r12]

    sub r12, 8
    mov rax, [r12]

    add rax, rbx
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rax, [r12]

    mov rbx, 0
    mov bl, [rax]
    mov [r12], rbx
    add r12, 8

    pop r13
    ret
ichar!!:
    push r13
    lea r13, [__local_vars__]
    sub r12, 8
    mov rbx, [r12]

    sub r12, 8
    mov rax, [r12]

    add rax, rbx
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rax, [r12]

    mov rbx, 0
    mov bl, [rax]
    mov [r12], rbx
    add r12, 8

    pop r13
    ret
strlen:
    push r13
    lea r13, [__local_vars__]
    sub r12, 8
    mov rax, [r12]

    mov [r13 + 0], rax
    mov rax, 0
    mov [r12], rax
    add r12, 8

.while_0:
    sub r12, 8
    mov rax, [r12]

    mov [r12], rax
    add r12, 8

    mov [r12], rax
    add r12, 8

    mov rax, [r13 + 0]
    mov [r12], rax
    add r12, 8

    call irchar
    mov rax, 0
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rcx, [r12]

    sub r12, 8
    mov rbx, [r12]

    cmp rbx, rcx
    setne al
    movzx rax, al
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rax, [r12]

    test rax, rax
    jz .endwhile_1
    mov rax, 1
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rbx, [r12]

    sub r12, 8
    mov rax, [r12]

    add rax, rbx
    mov [r12], rax
    add r12, 8

    jmp .while_0
.endwhile_1:
    pop r13
    ret
SYS_READ:
    push r13
    lea r13, [__local_vars__]
    mov rax, 0
    mov [r12], rax
    add r12, 8

    pop r13
    ret
SYS_WRITE:
    push r13
    lea r13, [__local_vars__]
    mov rax, 1
    mov [r12], rax
    add r12, 8

    pop r13
    ret
SYS_OPEN:
    push r13
    lea r13, [__local_vars__]
    mov rax, 2
    mov [r12], rax
    add r12, 8

    pop r13
    ret
SYS_CLOSE:
    push r13
    lea r13, [__local_vars__]
    mov rax, 3
    mov [r12], rax
    add r12, 8

    pop r13
    ret
SYS_EXIT:
    push r13
    lea r13, [__local_vars__]
    mov rax, 60
    mov [r12], rax
    add r12, 8

    pop r13
    ret
STDIN:
    push r13
    lea r13, [__local_vars__]
    mov rax, 0
    mov [r12], rax
    add r12, 8

    pop r13
    ret
STDOUT:
    push r13
    lea r13, [__local_vars__]
    mov rax, 1
    mov [r12], rax
    add r12, 8

    pop r13
    ret
puts:
    push r13
    lea r13, [__local_vars__]
    sub r12, 8
    mov rax, [r12]

    mov [r13 + 0], rax
    mov rax, [r13 + 0]
    mov [r12], rax
    add r12, 8

    call strlen
    sub r12, 8
    mov rax, [r12]

    mov [r13 + 8], rax
    call SYS_WRITE
    call STDOUT
    mov rax, [r13 + 0]
    mov [r12], rax
    add r12, 8

    mov rax, [r13 + 8]
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rdx, [r12]

    sub r12, 8
    mov rsi, [r12]

    sub r12, 8
    mov rdi, [r12]

    sub r12, 8
    mov rax, [r12]

    syscall
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rax, [r12]

    call SYS_WRITE
    call STDOUT
    mov rax, str_0
    mov [r12], rax
    add r12, 8

    mov rax, 1
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rdx, [r12]

    sub r12, 8
    mov rsi, [r12]

    sub r12, 8
    mov rdi, [r12]

    sub r12, 8
    mov rax, [r12]

    syscall
    mov [r12], rax
    add r12, 8

    sub r12, 8
    mov rax, [r12]

    pop r13
    ret

_start:
    mov r12, __dstack__
    mov r13, __global_vars__
    mov rax, str_1
    mov [r12], rax
    add r12, 8

    call puts

    mov rax, 60
    mov rdi, 0
    syscall
