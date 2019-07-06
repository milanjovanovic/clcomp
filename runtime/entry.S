	.globl _init_lisp		
	.align   8, 0x90
_init_lisp:
	push %rbp
	movq %rsp, %rbp
	push %rbx
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsp, _cs(%rip)
	movq %rdi, %rsp
	movq %rsi, %r15
	movq $536870927, %rax
	movq _cs(%rip), %rsp
	pop %r15
	pop %r14
	pop %r13
	pop %r12
	pop %rbx
	pop %rbp
	ret

	.globl _run_lisp_test
	.align 8, 0x90
_run_lisp_test:
	push %rbp
	movq %rsp, %rbp
	push %rbx
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsp, _cs(%rip)
	movq %rdi, %rsp
	movq %rsi, %r15
	movq %rdx, %rax
	movq $0, %rcx
	callq *%rax
	movq %rbx, %rax
	movq _cs(%rip), %rsp
	pop %r15
	pop %r14
	pop %r13
	pop %r12
	pop %rbx
	pop %rbp
	ret
	
	
	.section __DATA,__data
	.globl   _cs
_cs:
	.quad 0
