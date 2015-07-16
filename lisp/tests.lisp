(defparameter *tests*
  '(
    ((inst :mov (@ nil :RAX 2 nil) :RCX) (#x48 #x89 #x0C #x45 #x00 #x00 #x00 #x00))
    ((inst :mov (@ :R11 nil nil nil) :RAX) (#x49 #x89 #x03))
    ((inst :mov (@ :RAX :RBX 4 20) :RCX) (#x48 #x89 #x4C #x98 #x14))
    ((inst :mov (@ :RAX :RBX nil 127) :RBX) (#x48 #x89 #x5C #x18 #x7F))
    ((inst :mov (@ nil :RAX 8 nil) :RBX) (#x48 #x89 #x1C #xC5 #x00 #x00 #x00 #x00))    
    ))

(defmacro test-instr (tests)
  `(progn
     ,@(loop for (test expected) in `,tests
	     collect `(let ((res ,test))
			(when (not (equal res ',expected))
			  (print (list ',test :res 'res :expected ',expected)))))))

(defun run-tests ()
  (test-instr #.*tests*)
  (values))
