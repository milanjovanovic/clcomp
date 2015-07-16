(defparameter *tests*
  '(
    ((inst :mov (@ nil :RAX 2 nil) :RCX) (#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
    ((inst :mov (@ :R11 nil nil nil) :RAX) (#x49 #x89 #x03))
    ((inst :mov (@ :RAX :RBX 4 20) :RCX) (#x48 #x89 #x4C #x98 #x14))
    ((inst :mov :RCX (@ :RAX :RBX 4 20)) (#x48 #x8B #x4C #x98 #x14))    
    ))

(defmacro test-instr (tests)
  `(progn
     ,@(loop for (test expected) in `,tests
	     collect `(let ((res ,test))
			(when (not (equal res ',expected))
			  (print res))))))

(defun run-tests ()
  (test-instr #.*tests*))

