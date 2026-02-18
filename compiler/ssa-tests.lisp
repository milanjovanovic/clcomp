(in-package :clcomp.ssa)

(defparameter *live-vars-tests* '(("simple-1" (LAMBDA (A B C)
						(WHEN C
						  (IF A
						      (SETF B (+ 1 B))
						      (SETF B (+ 2 B))))
						B)
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-8 V-9))))
				    (2 ((LIVE-IN (V-8 V-9)) (LIVE-OUT (V-9))))
				    (4 ((LIVE-IN (V-9)) (LIVE-OUT (V-13))))
				    (5 ((LIVE-IN (V-9)) (LIVE-OUT (V-14))))
				    (3 ((LIVE-IN ()) (LIVE-OUT ()) (PHIS ((PHI-PLACE-0 (V-14 V-13 V-9))))))))
				  
				  ("simple-2" (LAMBDA (A B C)
						(WHEN C
						  (IF A
						      (SETF B (+ 1 B))
						      (SETF B (+ 2 B))))
						(IF 1
						    (FOO B)
						    (BAR B)))
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-11 V-12))))
				    (2 ((LIVE-IN (V-11 V-12)) (LIVE-OUT (V-12))))
				    (4 ((LIVE-IN (V-12)) (LIVE-OUT (V-16))))
				    (5 ((LIVE-IN (V-12)) (LIVE-OUT (V-18))))
				    (3 ((LIVE-IN ()) (LIVE-OUT (PHI-PLACE-0)) (PHIS ((PHI-PLACE-0 (V-12 V-16 V-18))))))
				    (8 ((LIVE-IN (PHI-PLACE-0)) (LIVE-OUT ())))))
				  ("simple-3" (LAMBDA (A B C)
						(WHEN C
						  (IF A
						      (SETF B (+ 1 B))
						      (PROGN
							(SETF B (+ 2 B))
							(IF 1
							    (PRINT 2)
							    (PRINT 3)))))
						B)
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-12 V-13))))
				    (2 ((LIVE-IN (V-12 V-13)) (LIVE-OUT (V-13))))
				    (4 ((LIVE-IN (V-13)) (LIVE-OUT (V-17))))
				    (7 ((LIVE-IN (V-17)) (LIVE-OUT (V-17))))
				    (8 ((LIVE-IN (V-17)) (LIVE-OUT (V-17))))
				    (5 ((LIVE-IN (V-13)) (LIVE-OUT (V-19))))
				    ;; FIXME, don't duplicate operands in PHI
				    (3 ((LIVE-IN ()) (LIVE-OUT ()) (PHIS ((PHI-PLACE-0 (V-17 V-17 V-19 V-13))))))))

				  ("early-return-from" (LAMBDA (A B C)
							 (BLOCK OUT
							   (LET ((X B))
							     (IF C
								 (RETURN-FROM OUT 99)
								 (PROGN
								   (IF A
								       (SETF X (+ X 1))
								       (SETF X (+ X 2)))
								   X)))))
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-8 V-11))))
				    (2 ((LIVE-IN (V-8 V-11)) (LIVE-OUT (V-11))))
				    (3 ((LIVE-IN ()) (LIVE-OUT ())))
				    (5 ((LIVE-IN (V-11)) (LIVE-OUT (V-15))))
				    (6 ((LIVE-IN (V-11)) (LIVE-OUT (V-14))))
				    (7 ((LIVE-IN ()) (LIVE-OUT ()) (PHIS ((PHI-PLACE-0 (V-14 V-15))))))))
				  ;; ("old-test" (LAMBDA (X)
				  ;; 		(TAGBODY
				  ;; 		 START
				  ;; 		   (IF (> X 0)
				  ;; 		       (PROGN
				  ;; 			 (SETF X (- X 1))
				  ;; 			 (GO MID))
				  ;; 		       (GO END))

				  ;; 		 MID
				  ;; 		   (IF (ODDP X)
				  ;; 		       (PROGN
				  ;; 			 (SETF X (+ X 2))
				  ;; 			 (GO START))
				  ;; 		       (SETF X (* X 2)))
    
				  ;; 		 END
				  ;; 		   (PRINT X)))
				  ;;  ((0 ((LIVE-IN ()) (LIVE-OUT (V-8 V-11))))
				  ;;   (2 ((LIVE-IN (V-8 V-11)) (LIVE-OUT (V-11))))
				  ;;   (3 ((LIVE-IN ()) (LIVE-OUT ())))
				  ;;   (5 ((LIVE-IN (V-11)) (LIVE-OUT (V-15))))
				  ;;   (6 ((LIVE-IN (V-11)) (LIVE-OUT (V-14))))
				  ;;   (7 ((LIVE-IN ()) (LIVE-OUT ()) (PHIS ((PHI-PLACE-0 (V-14 V-15))))))))
				  ("irreducible-1" (LAMBDA (X)
						     (LET ((SAVED (* X 2)))
						       (TAGBODY
							  (IF (> X 0) (GO A) (GO B))
							A
							  (SETF X (+ X SAVED))
							  (WHEN (< X 50) (GO B))
							  (GO END)
							B
							  (SETF X (- X SAVED))
							  (WHEN (> X 0) (GO A))
							END)
						       (+ X SAVED)))
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-15 V-16))))
				    (1 ((LIVE-IN (V-16)) (LIVE-OUT (V-16 V-18)) (PHIS ((PHI-PLACE-0 (V-20 V-15))))))
				    (2 ((LIVE-IN (V-16)) (LIVE-OUT (V-16 V-20)) (PHIS ((PHI-PLACE-2 (V-18 V-15))))))
				    (3 ((LIVE-IN (V-16)) (LIVE-OUT ())(PHIS ((PHI-PLACE-4 (V-20 V-18))))))))
				  ("irreducible-3way" (LAMBDA (X Y)
							(LET ((K (+ X Y)))
							  (TAGBODY
							     (COND ((> X 10) (GO A))
								   ((> Y 10) (GO B))
								   (T        (GO C)))
							   A (SETF X (+ X K)) (WHEN (< X 100) (GO B)) (GO END)
							   B (SETF Y (+ Y K)) (WHEN (< Y 100) (GO C)) (GO END)
							   C (SETF X (- X K)) (WHEN (> X 0) (GO A))
							   END)
							  (+ X Y K)))
				   ((0 ((LIVE-IN ()) (LIVE-OUT (V-30 V-29 V-28))))
				    (5 ((LIVE-IN (V-30 V-28 V-29)) (LIVE-OUT (V-30 V-29 V-28))))
				    (9 ((LIVE-IN (V-29 V-28 V-30)) (LIVE-OUT (V-30 V-28 V-29))))
				    (1 ((LIVE-IN (V-30)) (LIVE-OUT (V-30 V-34 PHI-PLACE-3))
					(PHIS ((PHI-PLACE-0 (V-28 V-38 V-28))
					       (PHI-PLACE-3 (V-29 PHI-PLACE-10 V-29))))))
				    (2 ((LIVE-IN (V-30)) (LIVE-OUT (V-30 PHI-PLACE-6 V-36))
					(PHIS ((PHI-PLACE-6 (V-28 V-34))
					       (PHI-PLACE-2 (V-29 PHI-PLACE-3))))))
				    (3 ((LIVE-IN (V-30)) (LIVE-OUT (V-30 PHI-PLACE-10 V-38))
					(PHIS ((PHI-PLACE-10 (V-29 V-36))
					       (PHI-PLACE-5 (V-28 PHI-PLACE-6))))))
				    (4 ((LIVE-IN (V-30)) (LIVE-OUT ())
					(PHIS ((PHI-PLACE-8 (V-38 PHI-PLACE-6 V-34))
					       (PHI-PLACE-9 (PHI-PLACE-10 V-36 PHI-PLACE-3))))))))))

(defun execute-test-compute-form (form)
  (let ((ssa (make-lssa form)))
    (compute-local-live-sets ssa)
    (compute-global-live-sets ssa)
    ssa))

(defun test-assert-vars (res block-results block-index error)
  (declare (optimize debug))
  (unless (= (length res)
	     (length block-results))
    (format t "~%")
    (format t "Different number of places, block-index: ~A, res: ~A and block-result: ~A~%" block-index res block-results)
    (when error
      (error "Different count of places")))
  (dolist (r res)
    (unless (find (symbol-name r) block-results :test #'equalp)
      (format t "Can't find place, block-index: ~A, ~A in ~A~%" block-index r block-results)
      (when error
	(error "Can't find place")))))

(defun test-assert-phis (phis block-res)
  #.*fun-optimize-level*
  (let ((block-phis (ssa-block-phis block-res)))
    (setf block-phis (remove-if (lambda (phi) (get-phi-place-reduced-value (phi-place phi))) block-phis))
    (assert (= (length phis)
	       (length block-phis)))
    (let ((block-phi-map (make-hash-table :test #'equalp)))
      (dolist (p block-phis)
	(let ((name (symbol-name (get-place-name (phi-place p))))
	      (ops (mapcar #'get-place-name (mapcar #'get-maybe-reduced-place (phi-operands p)))))
	  (setf (gethash  name block-phi-map) ops)))
      (dolist (phi phis)
	(let* ((phi-place-name (symbol-name (first phi)))
	       (operands (mapcar #'symbol-name (second phi)))
	       (res-phi-operands (mapcar #'symbol-name (gethash phi-place-name block-phi-map))))
	  (assert res-phi-operands)
	  (setf res-phi-operands (sort res-phi-operands #'string<))
	  (setf operands (sort operands #'string<))
	  (assert (equalp operands res-phi-operands))	)))))


(defun test-compute-live-sets (&optional throw-error)
  (dolist (test *live-vars-tests*)
    (let* ((form-name (first test))
	   (form (second test))
	   (ssa (execute-test-compute-form form))
	   (results (third test)))
      (format t "Running test for form ~A~%~%" form-name)
      (dolist (block-res results)
	(format t ".")
	(let* ((block-index (first block-res))
	       (b (ssa-find-block-by-index ssa block-index))
	       (live-in (second (assoc 'LIVE-IN (second block-res))))
	       (live-out (second (assoc 'LIVE-OUT (second block-res))))
	       (phis (second (assoc 'PHIS (second block-res)))))
	  (test-assert-vars live-in (mapcar (lambda (x) (symbol-name (get-place-name x)))
					    (ssa-block-live-in b))
			    block-index throw-error)
	  (test-assert-vars live-out (mapcar (lambda (x) (symbol-name (get-place-name x)))
					     (ssa-block-live-out b))
			    block-index throw-error)
	  (test-assert-phis phis b)))
      (format t "~%"))))


#+nil
(make-lssa '(lambda (x y)
	     (let ((z (if (< x y) x y))
		   (w (if (< x y) x y)))
	       (+ z w))))
;;; Test case that currently doesn't work
#+nil
(test-ssa '(lambda (a)
	    (let ((c 0))
	      (tagbody 
	       bar
		 (setf c (+ c 1))
		 (when a (go bar)))
	      c)))
;;; this one triggers redundant phi's optimization
#+nil
(test-ssa '(lambda (a)
	    (tagbody
	       (when 1 (go third))
	     second
	       (print 1)
	     third
	       (when 2 (go second)))
	    a))
#+nil
(test-ssa '(lambda (a b)
	    (tagbody
	     start
	       (setf a 1)
	       (when b
		 (go end))
	     baz
	       (read a)
	       (if b
		   (go start)
		   (go end))
	     end)
	    a))

;;; maybe we can trigger reduced PHI here ?
#+nil
(lambda (x)
  (tagbody
   bla
     (if x
	 (progn
	   (setf x (+ x 20))
	   (go while))
	 (go exit))
   while
     (tagbody
      start
	(when (> x 1)
	  (setf x (+ x 10))
	  (go bla)))
   exit)
  x)


;;; triggets stack overflow
;;; fixed with WHEN macro bug fix but this will be triggered somewhere else
#+nil
(generate  (make-lssa  '(lambda (a b)
			 (tagbody
			  start
			    (setf a 1)
			    (when b (print 10)
				  (go end))
			  baz
			    (read a)
			    (if b
				(go start)
				(go end))
			  end)
			 a)))

;;; notes
;;; * kad se interval zavrsava negde u istoj tacki moze da pocne drugi interval ako se tu definise nova varijabla, samo mora da seobrati paznja na redosled
;;; * kad resavamo phi, insertujemo move na kraju prethodnog bloka, mozda treba da napravimo novi blok 

;;; FIXME
;; 
;;; triggers endless loop
#+nil
(test-ssa '(lambda (a)
            (dotimes (i a)
              (dotimes (c i)
                (print 1)))))

;;; sometimes we have COND-JUMP that jumps to BLOCK that is next in order

;; throws error
#+nil
(test-ssa '(lambda (a)
	    (dolist (l a)
	      (dolist (g l)
		(print l)))))


;;; SSA, blocks order
;;; sometimes we have COND-JUMP that jumps to BLOCK that is next in order (when emiting assembly code we can do IF-NOT and in that way just emit one JUMP instead of TWO)
;; 
;;; sometimes we have UNCOND-JUMP (in SSA-IF) form that jumps to next BLOCK in order


;;; cl-dot, we are drawing this incorrectly, order is not accurate
#+nil
(test-ssa '(lambda (x a)
	    (tagbody 
	       (go end)
	     x
	       (setf x (+ 1 x))
	       (go real-end)
	     y
	       (setf x (+ 2 x))
	       (go real-end)
	     end
	       (if a
		   (go x)
		   (go y))
	     real-end)
	    x))

;;; FIXME
;;; there is bug when removing redundant blocks, we are removing necessary blocks
;;; There is error in BUILD-INTERVALS here

#+nil
(test-ssa '(lambda (x a)
	    (tagbody foo
	       (tagbody 
		  (go end)
		x
		  (setf x (+ 1 x))
		  (go real-end)
		y
		  (setf x (+ 2 x))
		  (go real-end)
		end
		  (if a
		      (go x)
		      (go y))
		real-end)
	       (go foo))))
#+nil
(test-ssa '(lambda (x a)
	    (tagbody foo
	       (print x)
	       (go foo))))


#+nil
(make-lssa '(lambda (a)
	     (tagbody 
		(go foo)
	      a1
		(print 1)
	      a2 
		(print 2) 
	      a3
		(print 3)
	      foo
		(if a 
		    (go a1)
		    (go a2))
	      z)) "default")


;;; this doesn't work
#+nil(test-ssa '(lambda (a)
		 (block foo
		   (tagbody
		      (go bla)
		      exit
		      (return-from foo 1)
		      bla
		      (when a
			(go exit))))))

;;; UNCOND-JUMP is not translated to SUCC for next block
;;; should it be translated ??
#+nil
(test-ssa '(lambda (a)
		 (multiple-value-bind (x y)
		     (block foo
		       (when a
			 (return-from foo (values 1 2)))
		       (values 3 4))
		   (list x y))))
