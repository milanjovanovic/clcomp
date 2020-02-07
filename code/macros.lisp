(in-package :clcomp)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *macros* (make-hash-table))
(defparameter *defsetfs* nil)


(defun macro-defparameter (form)
  (list 'progn
	(list 'eval-when (list ':compile-toplevel)
	      (list '%compiler-defparameter (list 'quote (second form))))
	(list '%defparameter (list 'quote (second form)) (third form))))
(setf (gethash 'defparameter *macros*) 'macro-defparameter)

(defun macro-defsetf (form)
  (list 'eval-when (list :compile-toplevel :execute)
	(list 'push `(cons ',(second form) ',(third form)) '*defsetfs*)))
(setf (gethash 'defsetf *macros*) 'macro-defsetf)

(defun macro-dotimes (form)
  (let ((limit (gensym "LIMIT-"))
	(tag1 (gensym "TAG1-"))
	(vsym (first (second form))))
    (list 'block nil
	  (list 'let* (list (list vsym 0)
			   (list limit (second (second form))))
		(list 'tagbody tag1
		      (list 'progn
			    (cons 'progn (cddr form))
			    (list 'setf vsym (list '+ 1 vsym))
			    (list 'if (list '>= vsym limit)
				  (list 'return-from nil nil)
				  (list 'go tag1))))))))
(setf (gethash 'dotimes *macros*) 'macro-dotimes)


;; fixme, use endp for testing istead of null
(defun macro-dolist (form)
  (let ((olist (gensym "LIST-"))
	(list-cdr (gensym "LIST-CDR"))
	(tag1 (gensym "TAG1-"))
	(vsym (first (second form))))
    (list 'block nil
	  (list 'let* (list (list olist (second (second form)))
			    (list list-cdr olist)
			    (list vsym (list 'car list-cdr)))
		(list 'tagbody tag1
		      (list 'progn
			    (list 'if (list 'null list-cdr)
				  (list 'return-from nil nil))
			    (cons 'progn (cddr form))
			    (list 'setf list-cdr (list 'cdr list-cdr))
			    (list 'setf vsym (list 'car list-cdr))
			    (list 'go tag1)))))))
(setf (gethash 'dolist *macros*) 'macro-dolist)


(defun macro-return (form)
  (list 'return-from nil (second form)))
(setf (gethash 'return *macros*) 'macro-return)


(defun macro-or (form)
  (if (= (length form) 2)
      (second form)
   (let ((f (gensym "F-")))
     (list 'let (list (list f (second form)))
	   (list 'if f f (cons 'or (cddr form)))))))
(setf (gethash 'or *macros*) 'macro-or)

(defun macro-named-lambda (form)
  (list 'lambda (third form)
	(fourth form)
	(cons 'block (cons (second form) (cddddr form)))))
(setf (gethash 'named-lambda *macros*) 'macro-named-lambda)


(defun macro-and (form)
  (if (= 2 (length form))
      (second form)
      (cons 'and (cons (list 'if (second form) (third form))
		       (cdddr form)))))
(setf (gethash 'and *macros*) 'macro-and)


;;; only simple form for now (defstruct name a b c)
(defun macro-defstruct (form)
  (let* ((name (second form))
	 (slots (cddr form))
	 (constructor-sym (intern (concatenate 'string "MAKE-" (symbol-name name)))))
    (append (list 'progn
		  (list '%defstruct name (length slots))
		  (list 'defun constructor-sym slots
			(list 'make-array (length slots) (cons 'list slots))))
	    (let ((slot-form nil)
		  (index 0))
	      (dolist (slot slots)
		(let ((reader-sym (intern (concatenate 'string (symbol-name name) "-" (symbol-name slot))) )
		      (writer-sym (intern (concatenate 'string "SET-" (symbol-name name) "-" (symbol-name slot)))))
		  (push (list 'defun reader-sym
			      (list 'instance)
			      (list 'aref 'instance index))
			slot-form)
		  (push (list 'defun writer-sym
			      (list 'instance 'value)
			      (list 'setf (list 'aref 'instance index) 'value))
			slot-form)
		  (push (list 'defsetf reader-sym writer-sym) slot-form))
		(setf index (+ 1 index)))
	      (reverse slot-form)))))
(setf (gethash 'defstruct *macros*) 'macro-defstruct)

(defun macro-unless (form)
  (list 'if (second form)
	nil
	(third form)))
(setf (gethash 'unless *macros*) 'macro-unless)


(defun macro-when (form)
  (list 'if (second form)
	(third form)))
(setf (gethash 'when *macros*) 'macro-when)

(defun macro-do-parse-step-setq-form (bindings)
  (let ((f nil))
    (dolist (b bindings)
      (when (third b)
	(push (list 'setf (first b) (third b)) f)))
    (cons 'progn (reverse f))))

(defun macro-do-parse-bindings (bindings)
  (let ((b nil))
    (dolist (bin bindings)
      (push (list (first bin) (second bin)) b))
    (reverse b)))

;; FIXME, bug when second binding form is missing
(defun macro-do (form)
  (let ((is-do* (eq (first form) 'do*))
	(bindings (second form))
	(test-form (third form))
	(step-form (cdddr form))
	(test-tag (gensym "TEST-TAG"))
	(step-tag (gensym "STEP-TAG")))
    (list 'block nil
	  (list (if is-do* 'let* 'let) (macro-do-parse-bindings bindings)
		(list 'tagbody
		      (list 'go test-tag)
		      step-tag
		      (cons 'progn step-form)
		      (macro-do-parse-step-setq-form bindings)
		      test-tag
		      (list 'if (first test-form)
			    (list 'return (cons 'progn (cdr test-form)))
			    (list 'go step-tag)))))))
(setf (gethash 'do *macros*) 'macro-do)
(setf (gethash 'do* *macros*) 'macro-do)

(defun macro-cond (form)
  (let ((clauses (cdr form)))
    (list 'if (first (first clauses))
	  (cons 'progn (cdr (first clauses)))
	  (when (cdr clauses)
	    (cons 'cond  (cdr clauses))))))
(setf (gethash 'cond *macros*) 'macro-cond)



;;(defun macro-typecase (form))


;;(defun macro-case (form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct macros-env blocks)

(defun get-block-or-return-from-symbol (env block-symbol base-name)
  (let* ((hash-key (concatenate 'string (symbol-name block-symbol) base-name))
	 (sym (gethash hash-key (macros-env-blocks env))))
    (if sym
	sym
	(let ((sym (gensym base-name)))
	  (setf (gethash hash-key (macros-env-blocks env)) sym)
	  sym))))

(defun block-to-value-name (env block-name)
  (get-block-or-return-from-symbol env block-name "BLOCK-LET-VAR-"))

(defun block-to-label-name (env block-name)
  (get-block-or-return-from-symbol env block-name "BLOCK-TAGBODY-"))

(defun %clcomp-macroexpand-let-bindings (bindings env)
  (mapcar (lambda (b)
	    (list (first b) (clcomp-macroexpand (second b) env)))
	  bindings))

(defun %clcomp-macroexpand-let (form env)
  (list (first form) (%clcomp-macroexpand-let-bindings (second form) env)
	(if (> (length (cddr form)) 1)
	 (cons 'progn (mapcar (lambda (f)
				(clcomp-macroexpand f env))
			      (cddr form)))
	 (clcomp-macroexpand (first (cddr form)) env))))

(defun %clcomp-macroexpand-block (form env)
  (let ((tagbody-symbol (block-to-label-name env (second form)))
	(ret-value-symbol (block-to-value-name env (second form))))
    (clcomp-macroexpand
     (list 'let (list (list ret-value-symbol nil))
	   (list 'progn
		 (list 'tagbody
		       (list 'setq ret-value-symbol
			     (append (list 'progn) (nthcdr 2 form)))
		       tagbody-symbol)
		 ret-value-symbol))
     env)))

(defun %clcomp-macroexpand-return-from (form env)
  (let ((tagbody-symbol (block-to-label-name env (second form)))
	(ret-value-symbol (block-to-value-name env (second form))))
    (clcomp-macroexpand
     (list 'progn
	   (list 'setq ret-value-symbol (third form))
	   (list 'go tagbody-symbol))
     env)))

(defun %clcomp-macroexpand-all (form env)
  (mapcar (lambda (f) (clcomp-macroexpand f env))
	  form))

(defun %clcomp-macroexpand-progn (form env)
  (if (= 1 (length form))
      (list 'progn nil)
      (cons 'progn
	    (mapcar (lambda (f)
		      (clcomp-macroexpand f env))
		    (cdr form)))))

(defun %clcomp-macroexpand (macro-form env)
  (let* ((macro-fun (gethash (first macro-form) *macros*))
	 (expanded (funcall macro-fun macro-form)))
    (if (consp expanded)
	(clcomp-macroexpand expanded env)
	expanded)))

(defun %parse-defun-to-lambda (form)
  (let ((got-declarations (and (listp (fourth form))
			       (eq 'declare (first (fourth form))))))
    (append (list 'named-lambda (second form)
		  (third form)
		  (if got-declarations (fourth form) (list 'declare)))
	    (if got-declarations (cddddr form) (cdddr form)))))

;; FIXME, %rt-defun ?!?!
(defun %clcomp-macroexpand-defun (form env)
  (list 'progn (list 'eval-when '(:compile-toplevel) (list '%compiler-defun (clcomp-macroexpand (list 'quote (second form)))))
	(list '%defun (second form)
	      (clcomp-macroexpand (%parse-defun-to-lambda form) env))))

(defun %clcomp-macroexpand-eval-when (form env)
  (list 'eval-when (second form)
	(if (> (length (cddr form)) 1)
	    (clcomp-macroexpand (cons 'progn (cddr form)) env)
	    (clcomp-macroexpand (first (cddr form)) env))))

(defun %lambda-has-declarations (form)
  (eq 'declare (and (listp (third form))
		    (first (third form)))))


;;; 
;;; &KEY implementation, rewrite lambda list so when there is &key we rewrite it with &REST
;;; and then bind key variables with LET
;; FIXME, this is easiest and slowest solution for now
;; at least generate code that will resolve arguments in one iteration over &REST

(defun %lambda-list-&key-keywords (lambda-list)
  (do ((lambda-list lambda-list (cdr lambda-list)))
      ((or (eq (car lambda-list) '&key)
	   (null lambda-list))
       (cdr lambda-list))))

(defun %lambda-list-get-&rest-var (lambda-list)
  (do* ((ll lambda-list (cdr ll))
	(lc (car ll) (car ll)))
       ((or (eq lc '&rest)
	    (null ll))
	(car (cdr ll)))))

(defun %lambda-list-get-last-fixed-param (lambda-list)
  (let ((k nil))
    (dolist (p lambda-list)
      (if (eq p '&rest)
	  (when k
	    (return-from %lambda-list-get-last-fixed-param k))
	  (setf k p)))))

(defun %rerarange-lambda-list (lambda-list)
  (let ((new-lambda-list nil))
    (do* ((ll lambda-list (cdr ll))
	  (lc (car ll) (car ll)))
	 (nil)
      (cond ((eq '&rest lc)
	     (let ((rest-sym (car (cdr ll))))
	       (return (reverse (cons rest-sym (cons '&rest new-lambda-list))))))
	    ((eq '&key lc)
	     (return (reverse (cons (gensym "rest-arg") (cons '&rest new-lambda-list)))))
	    (t (push lc new-lambda-list))))))

(defun %rest-or-key-args-let-form (last-fixed-var rest-var key-keywords)
  (let ((rest-var-sym (gensym "rest-var-sym-")))
    (list 'let*
	  (let ((b nil))
	    (push (list rest-var-sym last-fixed-var) b)
	    (when (not (eq last-fixed-var rest-var))
	      (push (list last-fixed-var (list 'car rest-var-sym)) b)
	      (push (list rest-var (list 'cdr rest-var-sym)) b))
	    (dolist (k key-keywords)
	      (if (atom k)
		  (push (list k (list 'getf rest-var-sym (list 'quote k))) b)
		  (push (list (first k)
			      (list 'or
				    (list 'getf rest-var-sym (list 'quote (first k)))
				    (second k))) b)))
	    (reverse b)))))

(defun %clcomp-macroexpand-lambda (form env)
  (let* ((has-declarations (%lambda-has-declarations form))
	 (key-keywords (%lambda-list-&key-keywords (second form)))
	 (rearanged-llist (or (and key-keywords
				   (%rerarange-lambda-list (second form)))
			      (second form)))
	 (rest-var (%lambda-list-get-&rest-var rearanged-llist))
	 (last-fixed-param (or (%lambda-list-get-last-fixed-param rearanged-llist) rest-var))
	 (key-args-let-form (and (or key-keywords rest-var)
				 (%rest-or-key-args-let-form last-fixed-param rest-var key-keywords))))
    (list 'lambda
	  (substitute '&compiler-rest '&rest rearanged-llist)
	  (if has-declarations (third form) (list 'declare))
	  (clcomp-macroexpand
	   (let ((body (if (> (length (cddr form)) 1)
			   (cons 'progn
				 (if has-declarations
				     (cdddr form)
				     (cddr form)))
			   (first (if has-declarations
				      (cdddr form)
				      (cddr form))))))
	     (if rest-var
		 (append key-args-let-form (list body))
		 body)) env))))

;; FIXME, implement setf macro expanders
(defun %clcomp-macroexpand-setf (form env)
  (let ((where (second form))
	(what (third form)))
    (if (symbolp where)
	(list 'setq where (clcomp-macroexpand what env))
	(let ((accessor (first where)))
	  (cond
	    ((eq accessor 'car)
	     (list 'rplaca (clcomp-macroexpand (second where) env) (clcomp-macroexpand what env)))
	    ((eq accessor 'cdr)
	     (list 'rplacd (clcomp-macroexpand (second where) env) (clcomp-macroexpand what env)))
	    ((eq accessor 'aref)
	     (list 'setf-aref (clcomp-macroexpand (second where) env) (clcomp-macroexpand (third where) env)
		   (clcomp-macroexpand what env)))
	    ((eq accessor 'char)
	     (list 'setf-char (clcomp-macroexpand (second where) env) (clcomp-macroexpand (third where) env)
		   (clcomp-macroexpand what env))))))))

(defun clcomp-macroexpand-1 (macro-form)
  (let* ((macro-fun (gethash (first macro-form) *macros*)))
    (funcall macro-fun macro-form)))

(defun clcomp-macroexpand-string (string)
  (let ((f nil)
	(i (- (length string) 1)))
    (dotimes (c (+ 1 i))
      (push (char string i) f)
      (decf i))
    (list 'make-string (length string) (cons 'list  f))))


;;; QUOTE expanding
;;; we are moving INTERN to runtime so we can bootstrap READER
(defun clcomp-macroexpand-quote-obj (obj)
  (if (consp obj)
      (cons 'list  (mapcar 'clcomp-macroexpand-quote-obj obj))
      (cond ((stringp obj) (clcomp-macroexpand-string obj))
	    ((symbolp obj) (list 'intern (clcomp-macroexpand-string (symbol-name obj))))
	    (t obj))))

(defun clcomp-macroexpand (form &optional env)
  (unless env
    (setf env (make-macros-env :blocks (make-hash-table :test 'equalp))))
  (if (atom form)
      (cond ((stringp form) (clcomp-macroexpand-string form))
	    (t form))
      (let ((first (first form)))
	(let ((macro-fun (gethash first *macros*)))
	  (if macro-fun
	      (%clcomp-macroexpand form env)
	      (case first
		;; FIXME, transform let* to let
		((let let*) (%clcomp-macroexpand-let form env))
		(block (%clcomp-macroexpand-block form env))
		(return-from (%clcomp-macroexpand-return-from form env))
		(progn (%clcomp-macroexpand-progn form env))
		(defun (%clcomp-macroexpand-defun form env))
		(eval-when (%clcomp-macroexpand-eval-when form env))
		(lambda (%clcomp-macroexpand-lambda form env))
		(setf (%clcomp-macroexpand-setf form env))
		;; FIXME, if lambda form is first need to macroexpand
		(quote (clcomp-macroexpand-quote-obj (second form)))
		(otherwise (cons first
				 (%clcomp-macroexpand-all (rest form) env)))))))))


