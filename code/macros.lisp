(in-package :clcomp)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defparameter *macros* (make-hash-table))

(defun macro-defparameter (form)
  (list 'progn
	(list 'eval-when (list ':compile-toplevel)
	      (list '%%compiler-defparameter (list 'quote (second form))))
	(list '%defparameter (list 'quote (second form)) (third form))))
(setf (gethash 'defparameter *macros*) 'macro-defparameter)

;; (defun macro-defsetf (form)
;;   (list 'eval-when (list :compile-toplevel :load-toplevel :execute)
;; 	(list 'push `(cons ',(second form) ',(third form)) '*defsetfs*)))
;; (setf (gethash 'defsetf *macros*) 'macro-defsetf)

(defun macro-dotimes (form)
  (let ((limit (gensym "LIMIT-"))
	(tag1 (gensym "TAG1-"))
	(vsym (first (second form))))
    (list 'block nil
	  (list 'let* (list (list vsym 0)
			    (list limit (second (second form))))
		(list 'tagbody tag1
		      (list 'progn
			    (list 'when (list '>= vsym limit)
				  (list 'return-from nil nil))
			    (cons 'progn (cddr form))
			    (list 'setf vsym (list '+ 1 vsym))
			    (list 'go tag1)))))))
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

(defun macro-unless (form)
  (list 'if (second form)
	nil
	(cons 'progn (cddr form))))
(setf (gethash 'unless *macros*) 'macro-unless)

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

(defun get-parent-struct (form)
  (when (consp (second form))
    (getf (second (second form)) :include)))

(defun get-struct-name (form)
  (if (consp (second form))
      (first (second form))
      (second form)))

(defun %slot-def-name (slot)
  (if (consp slot)
      (first slot)
      slot))

(defun create-%make-struct-body (struct slots parent-struct)
  (let ((constructor-name (intern (concatenate 'string "MAKE-" (symbol-name struct)))))
    (list 'defun constructor-name (cons '&key slots)
	  (list 'let (list (list 's (list '%allocate-struct (length slots)
					  (list 'quote struct)
					  (list 'load-time-value (list '%%get-struct-parents (list 'quote struct))))))
		(cons 'progn (reverse (let ((set-form nil)
					    (index 0))
					(dolist (slot slots)
					  (push (list '%set-struct-slot 's index (%slot-def-name slot)) set-form)
					  (incf index))
					set-form)))
		's))))

(defun create-struct-type-predicate (struct)
  (let ((type-predicate-name (intern (concatenate 'string (symbol-name struct) "-P"))))
    ;; FIXME, %structp can return NIL or struct type cons
    (list 'defun type-predicate-name (list 'obj)
	  (list 'and (list '%structp 'obj)
		(list 'or
		      (list 'eq (list 'quote struct) (list '%struct-type 'obj))
		      (list '%%struct-layout-has-type (list 'quote struct) (list '%struct-layout 'obj)))
		t))))

(defun macro-defstruct (form)
  (let* ((name (get-struct-name form))
	 (parent (get-parent-struct form))
	 (parent-slots (when parent
			 (%%get-struct-slots parent)))
	 (slots (append parent-slots (cddr form))))
    (when (and parent
	       (not (%%get-struct-info parent)))
      (error (concatenate 'string "Unknown parent struct " (symbol-name parent))))
    (append (list 'progn
		  (list 'eval-when (list :compile-toplevel :load-toplevel :execute)
			(list '%%define-struct name parent slots))
		  (create-%make-struct-body name slots parent))
	    (list
	     (create-struct-type-predicate name))
	    (let ((slot-form nil)
		  (index 0))
	      (dolist (slotf slots)
		(let ((slot (if (consp slotf)
				(first slotf)
				slotf)))
		  (let ((reader-sym (intern (concatenate 'string (symbol-name name) "-" (symbol-name slot))) )
			(writer-sym (intern (concatenate 'string "SET-" (symbol-name name) "-" (symbol-name slot)))))
		    (push (list 'defun reader-sym
				(list 'instance)
				(list '%check-struct-type 'instance (list 'quote name))
				(list '%get-struct-slot 'instance index))
			  slot-form)
		    (push (list 'defun writer-sym
				(list 'instance 'value)
				(list '%check-struct-type 'instance (list 'quote name))
				(list '%set-struct-slot 'instance index 'value))
			  slot-form)
		    (push (list 'eval-when (list :compile-toplevel :load-toplevel :execute)
				(list 'defsetf reader-sym writer-sym)) slot-form)))
		(setf index (+ 1 index)))
	      (reverse slot-form)))))
(setf (gethash 'defstruct *macros*) 'macro-defstruct)

(defun macro-when (form)
  (list 'if (second form)
	(append (list 'progn)
		(nthcdr 2 form))))
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

(defstruct macros-env blocks compile-or-load-time time)

(defun create-macros-env (compile-or-load-time time)
  (make-macros-env :blocks (make-hash-table :test 'equalp)
		   :compile-or-load-time compile-or-load-time
		   :time time))

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
  (list 'progn (list 'eval-when '(:compile-toplevel) (list '%%compiler-defun
							   (clcomp-macroexpand (second form)
									       env)))
	(list '%defun (second form)
	      (clcomp-macroexpand (%parse-defun-to-lambda form) env))))


(defun %clcomp-macroexpand-$defun (form env)
  (list '%defun (second form)
	(clcomp-macroexpand (%parse-defun-to-lambda form) env)))

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

(defun %lambda-list-&optional-vars (lambda-list)
  (do ((lambda-list lambda-list (cdr lambda-list)))
      ((or (eq (car lambda-list) '&optional)
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
	    ((or (eq '&key lc)
		 (eq '&optional lc))
	     (return (reverse (cons (gensym "rest-arg") (cons '&rest new-lambda-list)))))
	    (t (push lc new-lambda-list))))))

(defun %rest-or-key-or-optional-args-let-form (last-fixed-var rest-var key-keywords optional-variables)
  (let ((rest-var-sym (gensym "rest-var-sym-")))
    (list 'let*
	  (let ((b nil))
	    (push (list rest-var-sym last-fixed-var) b)
	    (when (not (eq last-fixed-var rest-var))
	      (push (list last-fixed-var (list 'car rest-var-sym)) b)
	      (push (list rest-var (list 'cdr rest-var-sym)) b))
	    (if key-keywords
		(dolist (k key-keywords)
		  (let ((bootstraped-sym (maybe-get-bootstraped-symbol-keyword k)))
		    (if (atom k)
			(push (list k (list 'getf rest-var (if bootstraped-sym
							       (list '%compile-time-constant bootstraped-sym)
							       (list 'load-time-value
								     (list 'intern (symbol-name k) "KEYWORD"))))) b)
			(push (list (first k)
				    (list 'or
					  (list 'getf rest-var
						(if bootstraped-sym
						    (list '%compile-time-constant bootstraped-sym)
						    (list 'load-time-value
							  (list 'intern (symbol-name (first k)) "KEYWORD"))))
					  (second k))) b))))
		(let ((oindex 0))
		  (dolist (ovar optional-variables)
		    (if (atom ovar)
			(push (list ovar (list 'nth oindex rest-var)) b)
			(push (list (first ovar)
				    (list 'or
					  (list 'nth oindex rest-var)
					  (second ovar))) b))
		    (incf oindex))))
	    (reverse b)))))

(defun %clcomp-macroexpand-lambda (form env)
  (let* ((has-declarations (%lambda-has-declarations form))
	 (key-keywords (%lambda-list-&key-keywords (second form)))
	 (optional-variables (%lambda-list-&optional-vars (second form)))
	 (rearanged-llist (or (and (or key-keywords
				       optional-variables)
				   (%rerarange-lambda-list (second form)))
			      (second form)))
	 (rest-var (%lambda-list-get-&rest-var rearanged-llist))
	 (last-fixed-param (or (%lambda-list-get-last-fixed-param rearanged-llist) rest-var))
	 (key-args-let-form (and (or key-keywords rest-var optional-variables)
				 (%rest-or-key-or-optional-args-let-form last-fixed-param
									 rest-var key-keywords optional-variables))))
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
		   (clcomp-macroexpand what env)))
	    (t (error "Unknown SETF expander !!")))))))

(defun clcomp-macroexpand-1 (macro-form)
  (let* ((macro-fun (gethash (first macro-form) *macros*)))
    (funcall macro-fun macro-form)))

(defun clcomp-macroexpand-string (string env)
  (if (macros-env-compile-or-load-time env)
      (let ((f nil)
	    (i (- (length string) 1)))
	(dotimes (c (+ 1 i))
	  (push (char string i) f)
	  (decf i))
	(list '%char-list-to-string (length string) (cons 'list  f)))
      string))

(defun clcomp-macroexpand-keyword (obj env)
  (if (macros-env-compile-or-load-time env)
      (list 'intern (clcomp-macroexpand-string (symbol-name obj) env) "KEYWORD")
      obj))


;;; QUOTE expanding
;;; we are moving INTERN to runtime so we can bootstrap READER
(defun clcomp-macroexpand-quote-obj (obj env)
  (if (macros-env-compile-or-load-time env)
      (if (consp obj)
	  (cons 'list  (mapcar (lambda (ex)
				 (clcomp-macroexpand-quote-obj ex env)) obj))
	  (cond ((stringp obj) (clcomp-macroexpand-string obj env))
		((symbolp obj) (list 'intern (clcomp-macroexpand-string (symbol-name obj) env)))
		(t obj)))
      (list 'quote obj)))

(defun clcomp-macroexpand (form &optional env)
  (unless env
    (setf env (create-macros-env nil nil)))
  (if (atom form)
      (cond ((stringp form) (clcomp-macroexpand-string form env))
	    ((keywordp form) (clcomp-macroexpand-keyword form env))
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
		($defun (%clcomp-macroexpand-$defun form env))
		(eval-when (%clcomp-macroexpand-eval-when form env))
		(lambda (%clcomp-macroexpand-lambda form env))
		(setf (%clcomp-macroexpand-setf form env))
		;; FIXME, if lambda form is first need to macroexpand
		(quote (clcomp-macroexpand-quote-obj (second form) env))
		(otherwise (cons first
				 (%clcomp-macroexpand-all (rest form) env)))))))))
