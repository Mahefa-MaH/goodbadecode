section .data
    good_data dw 10, 20, 30, 40, 50
    bad_data times 10 dw 0  ;Uninitialized data

section .bss
    good_result resd 1
    bad_result resd 1

section .text
    global _start

_start:
    ; Good code: Calculate sum of initialized data
    mov esi, good_data
    mov edi, good_result
    mov ecx, 5
    mov eax, 0

good_loop:
    add eax, [esi]
    add esi, 2
    loop good_loop
    mov [edi], eax


    ; Bad code: Attempt to use uninitialized data
    mov esi, bad_data
    mov edi, bad_result
    mov ecx, 10
    mov eax, 0

bad_loop:
    add eax, [esi]
    add esi, 2
    loop bad_loop
    mov [edi], eax


    ; Exit program
    mov eax, 1
    xor ebx, ebx
    int 0x80
