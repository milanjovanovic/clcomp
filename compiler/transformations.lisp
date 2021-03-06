(in-package #:clcomp)

(declaim (optimize (debug 3) (safety 3) (speed 0)))


;;; FIXME, think some better way for source transformation
;;; for now to make compiler works we just want to transform function call to two arg version if any

(defparameter *two-arg-transformation* '((+ two-args-+)
					 (- two-args--)
					 (= two-args-=)
					 (> two-args->)
					 (< two-args-<)
					 (>= two-args->=)
					 (<= two-args-<=)
					 (char= two-args-char=)
					 (char-equal two-args-char-equal)
					 (logxor two-args-logxor)
					 (logand two-args-logand)))


;;; this symbols need to be internet
;;; look at code/global.lisp
(defparameter *bootstrap-symbols* '(simple-array character "CL" "KEYWORD" *package* :element-type :initial-element))

(defun bootstraped-object-p (object)
  (find object *bootstrap-symbols* :test 'equal))

(defparameter *boostraped-keyword-symbols* '((element-type :element-type)
					     (initial-element :initial-element)))

(defun maybe-get-bootstraped-symbol-keyword (k)
  (second (assoc k *boostraped-keyword-symbols*)))


(defun get-two-arg-version (fun)
  (second (assoc fun *two-arg-transformation*)))

(defun maybe-transform-to-two-args-fun (form)
  (let* ((two-arg-version (get-two-arg-version (first form))))
    (if (and two-arg-version (= 2 (length (cdr form))))
	(cons two-arg-version (cdr form))
	form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; transform sexp expression to structures tree

(defstruct fun-rip-relative-node form)
(defstruct compile-time-constant-node form)
(defstruct immediate-constant-node value)
(defstruct ref-constant-node form node)
(defstruct lexical-var-node name form rest) ; FIXME, make-fun-argument-node
(defstruct if-node test-form true-form false-form)
(defstruct let-node bindings form sequential)
(defstruct progn-node forms)
(defstruct call-node function arguments)
(defstruct block-node name form)
(defstruct lambda-node name arguments declarations body)
(defstruct tagbody-node forms)
(defstruct go-node label-node)
(defstruct label-node label)
(defstruct setq-node var form)

(defun lexical-binding-exist (environment var)
  (dolist (bindings environment)
    (when (find var bindings)
      (return t))))

(defun create-lambda-arguments-nodes (arguments)
  (let ((nodes nil)
	(rest-node nil))
    (dolist (argument arguments)
      (cond ((eq '&compiler-rest argument) (setf rest-node t))
	    (rest-node (push (make-lexical-var-node :name argument :form nil :rest t) nodes)
		       (setf rest-node nil))
	    (t (push (make-lexical-var-node :name argument :form nil) nodes))))
    (reverse nodes)))

(defun parse-declarations (form)
  (cdr form))

(defun get-lambda-new-bindings (lambda-list)
  (filter lambda-list '&compiler-rest))

(defun create-lambda-node (form environment)
  (let ((environment (cons (get-lambda-new-bindings (second form))
			   environment)))
    (make-lambda-node :name nil
		      :declarations (parse-declarations (third form))
		      :arguments (create-lambda-arguments-nodes (second form))
		      :body (create-node (fourth form) environment))))

(defun create-if-node (form environment)
  (make-if-node :test-form (create-node (second form) environment)
		:true-form (create-node (third form) environment)
		:false-form (create-node (fourth form) environment)))

(defun create-lexical-or-dynamic-node (form environment)
  (if (lexical-binding-exist environment (first form))
      (make-lexical-var-node :name (first form) :form (create-node (second form) environment))
      (make-lexical-var-node :name (first form) :form (create-node (second form) environment))
      ;; FIXME, need dynamic environment here to know if variable already has dynamic binding
      ;; (make-dynamic-var-node :name (first form) :form (create-node (second form)))
      ))

(defun create-let-binding-nodes (bindings sequential environment)
  (let ((binstruct nil)
	(current-bin nil))
    (dolist (bind bindings)
      (let ((env (if sequential
		     (cons current-bin environment)
		     environment)))
	(push (create-lexical-or-dynamic-node bind env) binstruct)
	(push (first bind) current-bin)))
    (reverse binstruct)))

;;; FIXME, form in LET binding can consist of symbol that can be lexical or dynamic scoope
;;; see FIXME in CREATE-LEXICAL-OR-DYNAMIC-NODE
(defun create-let-node (form environment)
  (let ((sequential (eq (first form) 'let*)))
    (make-let-node :bindings (create-let-binding-nodes (second form) sequential environment)
		   :form (create-node (third form)
				      (cons (mapcar #'first (second form))
					    environment))
		   :sequential sequential)))

(defun create-progn-node (form environment)
  (make-progn-node :forms (mapcar (lambda (f)
				    (create-node f environment))
				  (rest form))))

(defun create-call-node (form environment)
  (let ((form (maybe-transform-to-two-args-fun form)))
    (make-call-node :function (first form) :arguments (mapcar (lambda (f)
								(create-node f environment))
							      (rest form)))))

(defun create-constant-node (form)
  (cond ((eq form nil)
	 (make-immediate-constant-node :value *nil*))
	((eq form t)
	 (make-immediate-constant-node :value *t*))
	((integerp form)
	 (make-immediate-constant-node :value (fixnumize form)))
	((characterp form)
	 (make-immediate-constant-node :value (characterize form)))
	((stringp form)
	 (create-ref-constant-node form))
	((keywordp form)
	 (create-ref-constant-node form))
	(t (error "Unknown constant form"))))

(defun create-tagbody-node (forms environment)
  (make-tagbody-node :forms (mapcar (lambda (exp)
				      (if (symbolp exp)
					  (make-label-node :label exp)
					  (create-node exp environment)))
				    (rest forms))))

(defun create-setq-node (form environment)
  (let ((var (second form)))
    (if (find var *dynamic-variables*)
	(create-node (list '%set-symbol-value (list 'quote var) (third form)) environment)
	(make-setq-node :var (make-lexical-var-node :name (second form)) :form (create-node (third form) environment)))))

(defun create-go-node (form)
  (make-go-node :label-node (make-label-node :label (second form))))

(defun create-load-time-value-node (form)
  (make-ref-constant-node
   :form form
   :node (create-node (list 'lambda nil
			    (list 'declare)
			    (cons 'progn (cdr (clcomp-macroexpand form (create-macros-env t t))))))))

(defun create-compile-time-constant-node (form)
  (make-compile-time-constant-node :form (second form)))

(defun create-lexical-or-symbol-value-node (form environment)
  (if (lexical-binding-exist environment form)
      (make-lexical-var-node :name form :form nil)
      (if (find form *dynamic-variables*)
	  (make-call-node :function 'symbol-value
			  :arguments (if (bootstraped-object-p form)
					 (list (make-compile-time-constant-node :form form))
					 (list (make-ref-constant-node
						:form form
						:node (create-node (clcomp-macroexpand (list 'lambda nil
											     (list 'quote form))
										       (create-macros-env t t)))))))
	  (error "Missing binding"))))


(defun parse-and-create-quoted-node (form)
  (if (consp form)
      (if (constantp (second form))
	  (create-node (second form))
	  (if (bootstraped-object-p (second form))
	      (make-compile-time-constant-node :form (second form))
	      (create-ref-constant-node form)))
      (error "Should't happen !")))


(defun create-ref-constant-node (form)
  (make-ref-constant-node
   :form form
   :node (create-node (clcomp-macroexpand (list 'lambda nil
						form)
					  (create-macros-env t t)))))

(defun create-fun-rip-relative-node (form)
  (make-fun-rip-relative-node :form (second form)))


(defun create-node (form &optional environment)
  (if (atom form)
      (cond ((constantp form)
	     (create-constant-node form))
	    ((symbolp form)
	     (create-lexical-or-symbol-value-node form environment))
	    (t (error "Unknown atom form ")))
      (let ((first (first form)))
	(cond ((eq first '%compile-constant)
	       (make-immediate-constant-node :value (second form)))
	      ((eq first 'quote)
	       (parse-and-create-quoted-node form))
	      ((eq first 'lambda)
	       (create-lambda-node form environment))
	      ((eq first 'load-time-value)
	       (create-load-time-value-node form))
	      ((eq first '%compile-time-constant) ; this is used for VMM allocated objects
	       (create-compile-time-constant-node form))
	      ((eq first '%function) ; using this to set SYMBOL-VALUE at load time when defining function
	       (create-fun-rip-relative-node form))
	      ((eq first 'if)
	       (create-if-node form environment))
	      ((or (eq first 'let)
		   (eq first 'let*))
	       (create-let-node form environment))
	      ((eq first 'progn)
	       (create-progn-node form environment))
	      ((eq first 'tagbody)
	       (create-tagbody-node form environment))
	      ((eq first 'setq)
	       (create-setq-node form environment))
	      ((eq first 'go)
	       (create-go-node form))
	      (t (create-call-node form environment))))))
