(asdf:defsystem #:clcomp
  :description "Toy lisp compiler"
  :author "Milan Jovanovic <milanj@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on ()
  :components ((:file "clcomp")
	       (:file "compiler/constants")
	       (:file "compiler/utils")
	       (:file "compiler/assembler")
	       (:file "compiler/instructions")
	       (:file "compiler/tests")
	       (:file "compiler/vops")
	       (:file "compiler/x8664/cons")
	       (:file "compiler/x8664/arith")
	       (:file "compiler/x8664/array")
	       (:file "compiler/x8664/call")
	       (:file "compiler/x8664/char")
	       (:file "compiler/transformations")
	       (:file "compiler/ir")
	       (:file "compiler/assembling")
	       (:file "compiler/compiler")
	       (:file "runtime/ffi")
	       (:file "code/macros")
	       (:file "code/macros-tests")
	       (:file "runtime/rt")
	       (:file "compiler/eval")
	       (:file "compiler/fcomp")))

