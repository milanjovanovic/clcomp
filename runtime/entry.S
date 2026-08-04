#ifdef __APPLE__
#define CNAME(x) _##x
#else
#define CNAME(x) x
#endif

	.globl CNAME(init_lisp)
	.align   8, 0x90
CNAME(init_lisp):
	movq %rdi, CNAME(lisp_stack)(%rip)
	movq %rsi, CNAME(lisp_heap)(%rip)
	ret

	.globl CNAME(run_lisp_test)
	.align 8, 0x90
CNAME(run_lisp_test):
	push %rbp
	movq %rsp, %rbp
	push %rbx
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsp, _cs(%rip)
	movq CNAME(lisp_stack)(%rip), %rsp
	movq CNAME(lisp_heap)(%rip), %r15
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

	.globl CNAME(eval_lisp_fixup)
	.align 8, 0x90
CNAME(eval_lisp_fixup):
	push %rbp
	movq %rsp, %rbp
	push %rbx
	push %r12
	push %r13
	push %r14
	push %r15
	movq %rsp, _cs(%rip)
	movq CNAME(lisp_stack)(%rip), %rsp
	movq CNAME(lisp_heap)(%rip), %r15
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

	.section .data
	.globl   _cs
	.globl   CNAME(lisp_heap)
	.globl   CNAME(lisp_stack)
_cs:
	.quad 0
CNAME(lisp_heap):
	.quad 0
CNAME(lisp_stack):
	.quad 0
