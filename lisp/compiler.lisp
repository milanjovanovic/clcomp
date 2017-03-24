(in-package #:clcomp)

#+nil
(progn
 (load-shared-object "/Users/milan/projects/clcomp/c/runtime.so")
 (load "/Users/milan/projects/clcomp/lisp/utils.lisp")
 (load "/Users/milan/projects/clcomp/lisp/ffi.lisp")
 (load "/Users/milan/projects/clcomp/lisp/assembler.lisp")
 (load "/Users/milan/projects/clcomp/lisp/instructions.lisp")
 (load "/Users/milan/projects/clcomp/lisp/tests.lisp")
 (load "/Users/milan/projects/clcomp/lisp/vops.lisp"))


#+nil
(defparameter *example-code-1* (make-array
				10
				:element-type '(unsigned-byte 8)
				:initial-contents))

(defparameter *stack-pointer-reg* :RSP)
(defparameter *base-pointer-reg* :RBP)
(defparameter *fun-address-reg* :RAX)
(defparameter *return-value-reg* :RBX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *heap-header-reg* :R15)
(defparameter *fun-arguments-regs* '(:RDX :RSI :RDI :R8 :R9 :R10))
(defparameter *free-regs* '(:R11 :R12 :R13 :R14))
