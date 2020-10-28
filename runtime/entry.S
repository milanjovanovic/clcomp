	.globl _init_lisp		
	.align   8, 0x90
_init_lisp:
	movq %rdi, _lisp_stack(%rip)
	movq %rsi, _lisp_heap(%rip)
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
	movq _lisp_stack(%rip), %rsp
	movq _lisp_heap(%rip), %r15
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

	.globl _eval_lisp_fixup
	.align 8, 0x90
_eval_lisp_fixup:
	push %rbp
	movq %rsp, %rbp
	push %rbx
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsp, _cs(%rip)
	movq _lisp_stack(%rip), %rsp
	movq _lisp_heap(%rip), %r15
	movq %rdi, %rax
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
	.globl   _lisp_heap
	.globl   _lisp_stack
_cs:
	.quad 0
_lisp_heap:
	.quad 0
_lisp_stack:
	.quad 0
