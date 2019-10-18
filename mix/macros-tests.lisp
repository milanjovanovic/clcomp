(in-package :clcomp)

(defun t1 ()
  (let ((l '(lambda (x)
	     (let ((c 0))
	       (dotimes (i x)
		 (dolist (l (list 1 2 3 4))
		   (setf c (+ i l))))
	       c))))
    (dotimes (i 10)
      (let ((r (random 1000)))
	(unless
	 (= (funcall (eval l) r)
	    (funcall (eval (clcomp-macroexpand l)) r))
	  (error "Test t1 failed !!!"))))
    t))

(defun t2 ()
  (and
   (= (funcall (eval (clcomp-macroexpand '(lambda (x)
					   (let ((r 0))
					     (dolist (l x)
					       (dotimes (i l)
						 (setf r (+ r i))))
					     r))))
	       (list 1 2 3))
      4)
   (null (funcall (eval (clcomp-macroexpand '(lambda (x)
					      (dotimes (i 10)
						(let ((r 0))
						  (dolist (l x)
						    (dotimes (i l)
						      (setf r (+ r i))))
						  r)))))
		  (list 1 2 3)))))


(defun t3 ()
  (=  1 (funcall (eval (clcomp-macroexpand
		     (lambda ()
		      (block foo
			(tagbody bla
			   (tagbody
			      (go bla)
			    bla
			      (return-from foo 1))))))))))

(defun t4 ()
  (=  2 (funcall (eval (clcomp-macroexpand
			(lambda ()
			  (block foo
			    (block foo
			      (tagbody bla
				 (tagbody
				    (go bla)
				  bla
				    (return-from foo 1))))
			    2)))))))


(defun run-macros-tests ()
  (or
   (and (t1) (t2) (t3) (t4))
   (error "Failed tests !!!")))
