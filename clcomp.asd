(asdf:defsystem #:clcomp
  :description "Toy lisp compiler"
  :author "Milan Jovanovic <milanj@gmail.com>"
  :license "BSD"
  :serial t
  :depends-on ()
  :components ((:file "lisp/clcomp")
	       (:file "lisp/constants")
	       (:file "lisp/utils")
	       (:file "lisp/ffi")
	       (:file "lisp/assembler")
	       (:file "lisp/instructions")
	       (:file "lisp/tests")
	       (:file "lisp/vops")
	       (:file "lisp/transformations")
	       (:file "lisp/ir")
	       (:file "lisp/irutils")
	       (:file "lisp/assembling")
	       (:file "lisp/compiler")
	       (:file "lisp/eval")))
