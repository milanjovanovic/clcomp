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

(defparameter *fun-address-reg* :RAX)
(defparameter *fun-number-of-arguments-reg* :RCX)
(defparameter *fun-arguments-registers #(:RDX :RBX :RSI :RDI))
(defparameter *caller-save-registers* #(:R8 :R9 :R10 :R11))
(defparameter *callee-save-registers* #(:R11 :R12 :R13 :R14))


(defun clcomp-compile (name definition)
  (print (list name definition)))
