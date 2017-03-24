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
	movq %rdi, %rax 		
	//	callq *%rdx
	movq $536870927, %rax
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
