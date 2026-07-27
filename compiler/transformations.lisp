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

(defparameter *node-id* 0)

(defstruct tnode (id (incf *node-id*)))
(defstruct (declaration-node (:include tnode)) safety optimize debug type)
(defstruct (rip-relative-node (:include tnode)))
(defstruct (fun-rip-relative-node (:include rip-relative-node)) form)
(defstruct (compile-time-constant-node (:include rip-relative-node)) form)
(defstruct (lambda-node (:include rip-relative-node)) name arguments declarations body)
(defstruct (immediate-constant-node (:include tnode)) value)
(defstruct (load-time-value-node (:include rip-relative-node)) form node)
(defstruct (lexical-var-node (:include tnode)) lambda-id name)
(defstruct (lexical-binding-node (:include tnode)) name bin-node lambda-id form rest shared)
(defstruct (if-node (:include tnode)) test-form true-form false-form)
(defstruct (let-node (:include tnode)) bindings form sequential)
(defstruct (progn-node (:include tnode)) forms)
(defstruct (call-node (:include tnode)) function arguments)
(defstruct (vop-node (:include tnode)) vop arguments)
(defstruct (block-node (:include tnode)) name form)
(defstruct (return-from-node (:include tnode)) name form)
(defstruct (tagbody-node (:include tnode)) forms)
(defstruct (go-node (:include tnode)) label-node)
(defstruct (label-node (:include tnode)) label)
(defstruct (setq-node (:include tnode)) var form)
(defstruct (values-node (:include tnode)) forms)
(defstruct (m-v-b-binding-node (:include tnode)) name lambda-id value-index bin-node)
(defstruct (m-v-b-node (:include tnode)) bindings form declaration body)

;; (defstruct irnode)
;; (defstruct (move-node (:include irnode)) to from)
;; (defstruct (cmp-node (:include irnode)) arg1 arg2 )
;; (defstruct (jmp-node (:include irnode)) target zf cf)

(defstruct cenv lambda-id lambda-declarations bindings declaration)

(defun make-constant-nil-node ()
  (make-immediate-constant-node :value *nil*))

;;; FIXME
(defun parse-declaration-form (form)
  (declare (ignorable form))
  (make-declaration-node))

(defun binding-node-name (node)
  (etypecase node
    (lexical-var-node (lexical-var-node-name node))
    (lexical-binding-node (lexical-binding-node-name node))
    (m-v-b-binding-node (m-v-b-binding-node-name node))))

(defun get-lexical-node (node)
  (etypecase node
    (lexical-var-node node)
    (lexical-binding-node (lexical-binding-node-bin-node node))
    (m-v-b-binding-node (m-v-b-binding-node-bin-node node))))

(defun lexical-binding-exist (environment var)
  (dolist (cenv environment)
    (let ((bin (find var (cenv-bindings cenv) :key #'binding-node-name)))
      (when bin
	(return-from lexical-binding-exist bin)))))

(defun get-current-lambda-id (environment)
  (dolist (cenv environment)
    (when (cenv-lambda-id cenv)
      (return (cenv-lambda-id cenv)))))

;; FIXME - slow
(defun fun-inlined-p (environment fun)
  (dolist (cenv environment)
    (dolist (declr (cenv-declaration cenv))
      (when (and (eq (car declr) 'notinline)
		 (find fun (cdr declr)))
	(return-from fun-inlined-p nil))
      (when (and (eq (car declr) 'inline)
		 (find fun (cdr declr)))
	(return-from fun-inlined-p t)))))

;; FIXME, for now we just check does number of arguments match with vop arguments count
;; not sure about this
(defun does-vop-match (arguments-nodes vop)
  (= (length arguments-nodes) 
     (length (vop-arguments vop))))

(defun use-vop-p (environment fun argument-nodes)
  (and (get-vop fun)
       (fun-inlined-p environment fun)
       (not *dont-inline*)
       (does-vop-match argument-nodes (get-vop fun))))

(defun create-lambda-arguments-nodes (arguments lambda-id)
  (let ((nodes nil)
	(rest-node nil))
    (assert lambda-id)
    (dolist (argument arguments)
      (cond ((eq '&compiler-rest argument) (setf rest-node t))
	    (rest-node (push (make-lexical-binding-node :name argument
							:rest t
							:lambda-id lambda-id
							:bin-node (make-lexical-var-node :name argument  :lambda-id lambda-id))
			     nodes)
		       (setf rest-node nil))
	    (t (push (make-lexical-binding-node :name argument
						:lambda-id lambda-id
						:bin-node (make-lexical-var-node :name argument :lambda-id lambda-id))
		     nodes))))
    (reverse nodes)))

;;; FIXME, create struct object that is easy to query
(defun parse-declarations (form)
  (cdr form))

(defun get-lambda-new-bindings (lambda-list)
  (filter lambda-list '&compiler-rest))

(defun create-lambda-node (form environment)
  (let* ((declarations (parse-declarations (third form)))
	 (lambda-node (make-lambda-node)))
    (setf (lambda-node-declarations lambda-node) declarations)
    (let* ((argument-nodes (create-lambda-arguments-nodes (second form) (tnode-id lambda-node)))
	   (environment (cons (make-cenv :lambda-id (tnode-id lambda-node)
					 :bindings argument-nodes
					 :declaration declarations)
			      environment)))
      (setf (lambda-node-arguments lambda-node) argument-nodes)
      (setf (lambda-node-body lambda-node)  (create-node (fourth form) environment))
      lambda-node)))

(defun create-if-node (form environment)
  (make-if-node :test-form (create-node (second form) environment)
		:true-form (create-node (third form) environment)
		:false-form (create-node (fourth form) environment)))

(defun create-lexical-or-dynamic-node (form lambda-id environment)
  (let ((binding-info (lexical-binding-exist environment (first form))))
    (if binding-info
	(make-lexical-binding-node :name (first form) :form (create-node (second form) environment) :lambda-id lambda-id
				   :bin-node (make-lexical-var-node :name (first form) :lambda-id lambda-id))
	(make-lexical-binding-node :name (first form) :form (create-node (second form) environment) :lambda-id lambda-id
				   :bin-node (make-lexical-var-node :name (first form) :lambda-id lambda-id))
	;; FIXME, need dynamic environment here to know if variable already has dynamic binding
	;; (make-dynamic-var-node :name (first form) :form (create-node (second form)))
	)))

;; FIXME (let ((a 1) (a 2))) should throw error, (let* ((a 1) (a 2))) should 
(defun create-let-binding-nodes (bindings lambda-id sequential environment)
  (let ((binstruct nil)
	(current-bin nil))
    (dolist (bind bindings)
      (let ((env (if sequential
		     (cons (make-cenv :bindings current-bin) environment)
		     environment)))
	(let ((node (create-lexical-or-dynamic-node bind lambda-id env)))
	  (push node binstruct)
	  (push node current-bin))))
    ;; we need to reverse because of order of evaluatin
    (reverse binstruct)))

;;; FIXME, form in LET binding can consist of symbol that can be lexical or dynamic scoope
;;; see FIXME in CREATE-LEXICAL-OR-DYNAMIC-NODE
(defun create-let-node (form environment)
  (let ((sequential (eq (first form) 'let*))
	(this-lambda-id (get-current-lambda-id environment))
	(let-node (make-let-node)))
    (assert this-lambda-id)
    (setf (let-node-bindings let-node) (create-let-binding-nodes (second form) this-lambda-id sequential environment))
    (setf (let-node-form let-node)
	  (create-node (third form)
		       (cons (make-cenv :bindings (reverse (let-node-bindings let-node)))
			     environment)))
    (setf (let-node-sequential let-node) sequential)
    let-node))

(defun create-progn-node (form environment)
  (make-progn-node :forms (mapcar (lambda (f)
				    (create-node f environment))
				  (rest form))))

(defun create-call-node (form environment)
  (let* ((form (maybe-transform-to-two-args-fun form))
	 (arg-nodes (mapcar (lambda (f)
			      (create-node f environment))
			    (rest form)))
	 (use-vop (use-vop-p environment (first form) arg-nodes)))
    (if use-vop
	(make-vop-node :vop (first form) :arguments arg-nodes)
	(make-call-node :function (first form) :arguments arg-nodes))))

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
  (let* ((var (second form))
	 (binding (lexical-binding-exist environment var)))
    (if (find var *dynamic-variables*)
	(create-node (list '%set-symbol-value (list 'quote var) (third form)) environment)
	(if binding
	    (make-setq-node :var (get-lexical-node binding) :form (create-node (third form) environment))
	    ;; SETF/SETQ without DEFPARAMETER do sets SYMBOL-VALUE to the value
	    ;; but variable is not proclaimed as SPECIAL so no special binding will be established
	    ;; TODO, inspect how to implement DEFPARAMETER
	    (error "To be implement")))))

(defun create-go-node (form)
  (make-go-node :label-node (make-label-node :label (second form))))

(defun create-load-time-value-node (form)
  (make-load-time-value-node
   :form form
   :node (create-node (list 'lambda nil
			    (list 'declare)
			    (cons 'progn (cdr (clcomp-macroexpand form (create-macros-env t t))))))))

(defun create-values-node (form environment)
  (make-values-node :forms (mapcar (lambda (f)
				     (create-node f environment))
				   (cdr form))))

(defun create-m-v-b-node (form environment)
  (let* ((current-lambda-id (get-current-lambda-id environment))
	 (bindings nil)
	 (index 0)
	 (form-node (create-node (third form) environment))
	 (declaration (when (eq 'declare (first (fourth form)))
			(parse-declaration-form (fourth form)))))
    (assert current-lambda-id)
    (dolist (s (second form))
      (push (make-m-v-b-binding-node :name s :value-index index :lambda-id current-lambda-id
				     :bin-node (make-lexical-var-node :name s :lambda-id current-lambda-id ))
	    bindings)
      (incf index))
    (let* ((new-environment (cons (make-cenv :bindings bindings)
				  environment))
	   (body (create-node  (if declaration
				   (fifth form)
				   (fourth form))
			       new-environment)))
      (make-m-v-b-node :bindings (reverse bindings)
		       :form form-node
		       :declaration declaration
		       :body body))))

;;; FIXME, block name is nil, does this works ?
(defun create-block-node (form environment)
  (let ((block-name (second form)))
    (unless (symbolp block-name)
      (error "block name is not a symbol"))
    (make-block-node :name block-name :form (if (> (length (cddr form)) 1)
						(make-progn-node :forms
								 (mapcar (lambda (f)
									   (create-node f environment))
									 (cddr form)))
						(create-node (caddr form) environment)))))

(defun create-return-from (form environment)
  (if (/= (length form) 3)
      (error "Error while parsing arguments to special operator RETURN-FROM")
      (let ((block-name (second form))
	    (return-form (third form)))
	(unless (symbolp block-name)
	  (error "Block name in RETURN-FROM form need to be a symbol"))
	(make-return-from-node :name block-name :form (create-node return-form environment)))))

(defun create-compile-time-constant-node (form)
  (make-compile-time-constant-node :form (second form)))

(defun create-lexical-or-symbol-value-node (form environment)
  (declare (optimize debug))
  (let ((binding (lexical-binding-exist environment form))
	(current-lambda-id (get-current-lambda-id environment)))
    (assert current-lambda-id)
    (if binding
	(progn
	  ;; FIXME, uncomment this, if we want to set :shared field to true
	  ;; (when (/= current-lambda-id (getf binding-info :lambda-id))
	  ;;   (let ((binding-node (get-binding-node (getf binding-info :node-id))))
	  ;;     (assert binding-node)
	  ;;     (setf (lexical-binding-node-shared binding-node) t)))
	  ;; (make-lexical-var-node :name form :form nil :source-lambda-id (getf binding-info :lambda-id) :source-node (getf binding-info :node-id))
	  (get-lexical-node binding))
	(if (find form *dynamic-variables*)
	    (make-call-node :function 'symbol-value
			    :arguments (if (bootstraped-object-p form)
					   (list (make-compile-time-constant-node :form form))
					   (list (make-load-time-value-node
						  :form form
						  :node (create-node (clcomp-macroexpand (list 'lambda nil
											       (list 'quote form))
											 (create-macros-env t t)))))))
	    (error "Missing binding")))))


(defun parse-and-create-quoted-node (form)
  (if (consp form)
      (if (constantp (second form))
	  (create-node (second form))
	  (if (bootstraped-object-p (second form))
	      (make-compile-time-constant-node :form (second form))
	      (create-ref-constant-node form)))
      (error "Should't happen !")))


(defun create-ref-constant-node (form)
  (make-load-time-value-node
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
	      ((eq first 'values)
	       ;; missing (SETF (VALUES ... form, but maybe we don't need it
	       (create-values-node form environment))
	      ((eq first 'multiple-value-bind)
	       (create-m-v-b-node form environment))
	      ((eq first 'block)
	       (create-block-node form environment))
	      ((eq first 'return-from)
	       (create-return-from form environment))
	      (t (create-call-node form environment))))))


(defun map-to-nodes (form)
  (let ((*node-id* 0))
    (create-node form)))
