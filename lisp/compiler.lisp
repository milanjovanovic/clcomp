(load-shared-object "/Users/milan/projects/clcomp/c/runtime.so")
(load "/Users/milan/projects/clcomp/lisp/utils.lisp")
(load "/Users/milan/projects/clcomp/lisp/ffi.lisp")
(load "/Users/milan/projects/clcomp/lisp/assembler.lisp")
(load "/Users/milan/projects/clcomp/lisp/instructions.lisp")
(load "/Users/milan/projects/clcomp/lisp/tests.lisp")


#+nil
(defparameter *example-code-1* (make-array
				10
				:element-type '(unsigned-byte 8)
				:initial-contents))
