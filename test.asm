global _start

section .text
_start:
	call main

	mov rax, 60
	mov rdi, 0
	syscall

main:
	mov rax, 1
	mov rdi, 0
	mov rsi, msg
	mov rdx, 14
	syscall

	ret

section .data
msg: db "Hello, World!", 10
