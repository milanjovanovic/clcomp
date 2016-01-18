(in-package #:clcomp)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *tests*
    '(((inst :add :r15 :RAX) (#x49 #x01 #xC7))
      ((inst :add :eax (@ :ecx)) (#x67 #x03 #x01))
      ((inst :add :rcx (@ 10000)) (#x48 #x03 #x0C #x25 #x10 #x27 #x00 #x00))
      ((inst :mov :RAX :RCX) (#x48 #x89 #xC8))
      ((inst :add :rcx 10000) (#x48 #x81 #xC1 #x10 #x27 #x00 #x00))
      ((inst :mov (@ nil :RAX 2 nil) :RCX) (#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
      ((inst :mov (@ :RAX 2) :RCX) (#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
      ((inst :mov (@ :R11) :RAX) (#x49 #x89 #x03))
      ((inst :mov (@ :R11 nil nil nil) :RAX) (#x49 #x89 #x03))
      ((inst :mov (@ :RAX :RBX 4 20) :RCX) (#x48 #x89 #x4C #x98 #x14))
      ((inst :mov (@ :RAX :RBX nil 127) :RBX) (#x48 #x89 #x5C #x18 #x7F))
      ((inst :mov (@ nil :RAX 8 nil) :RBX) (#x48 #x89 #x1C #xC5 #x00 #x00 #x00 #x00))
      ((inst :lea :RAX (@ :RCX nil nil nil)) (#x48 #x8D #x01))
      ((inst :lea :RBX (@ :RAX :RCX nil nil)) (#x48 #x8D #x1C #x08))
      ((inst :lea :RBX (@ :RAX :RCX 8 127)) (#x48 #x8D #x5C #xC8 #x7F))
      ((inst :lea :RBX (@ :RAX :RCX 8 128)) (#x48 #x8D #x9C #xC8 #x80 #x00 #x00 #x00))
      ((inst :mov :ECX (@ :EAX 2)) (#x67 #x8B #x0C #x45 #x00 #x00 #x00 #x00))
      )))

(defmacro test-instr (tests)
  `(progn
     ,@(loop for (test expected) in `,tests
	     collect `(let ((res ,test))
			(if (equal res ',expected)
			    (print (list :OK ',test))
			    (print (list :ERROR ',test :res 'res :expected ',expected)))))))

(defun run-tests ()
  (test-instr #.*tests*)
  (values))
