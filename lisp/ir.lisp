(in-package #:clcomp)
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code code-blocks constants fixups)

(defparameter *specials* nil)

(defun special? (symbol)
  (member symbol *specials*))

(defun reset-temp-location-counter ()
  (setf *temp-counter* 0))

(defun make-temp-location ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))


;;; environments

(defparameter *env-counter* 0)

(defun make-env-holder ()
  (list 'envs nil))

(defun make-env ()
  (prog1
   (list *env-counter* (make-hash-table))
   (incf *env-counter*)))

(defun add-env (env env-holder)
  (push env (second env-holder))
  env-holder)

(defun remove-env (all-env)
  (pop (second all-env))
  (decf *envs-counter*))

(defun add-var (var-node environments)
  (let* ((env (first (second environments)))
	 (env-number (first env))
	 (env-map (second env)))
    (setf (gethash (lexical-var-node-name var-node) env-map) env-number)))


(defun get-var-ir-symbol (var-name env-holder)
  (dolist (env (second env-holder))
    (let* ((env-map (second env))
	   (var-env-number (gethash var-name env-map)))
      (when var-env-number
	(return-from get-var-ir-symbol
	  (intern (concatenate 'string (symbol-name var-name)
			       (concatenate 'string "-" (write-to-string var-env-number))))))
      ;; FIXME, unboud vars threat as special vars
      )))


;;; IR code generators
;;; FIXME, use structures

(defun add-ir (component ir)
  (setf (ir-component-code component)
	(append (ir-component-code component) ir)))

(defun make-label-ir (component label)
  (add-ir component (list (list 'label label))))

(defun make-go-ir (component label)
  (add-ir component (list (list 'go label))))

(defun make-load-ir (component to from)
  (add-ir component (list (list 'load to from))))

(defun make-if-ir (component location label)
  (add-ir component (list (list 'if location 'go label))))

;;; FIXME, this is wrong
(defun make-quoted-ir (component to from)
  (add-ir component (list (list 'load-quoted to from))))

(defun make-call-ir (component location fun)
  (add-ir component (list (list 'load location 'call fun))))

(defun make-return-ir (component location)
  (add-ir component (list (list 'return location))))

(defun make-lambda-entry-ir (component number-of-arguments)
  (add-ir component (list (list 'lambda-entry) (list 'arg-check number-of-arguments))))

(defun make-lambda-arguments-ir (component arguments environments)
  (dolist (arg arguments)
    (add-var arg environments)
    (add-ir component (list (list 'receive-param (get-var-ir-symbol (lexical-var-node-name arg) environments))))))

(defun make-fun-params-ir (component arg-locations)
  (dolist (arg-loc arg-locations)
    (add-ir component (list (list 'param arg-loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emiters

;; FIXME, numbers that are larger than 32bit
(defun emit-constant-ir (component node)
  (declare (ignore component))
  (immediate-constant-node-value node))

;;; FIXME, this is not right
;;; quick solution: transform this to runtime list
(defun emit-quoted-form-ir (component node)
  (let ((location (make-temp-location)))
    (make-quoted-ir component location (quoted-node-form node))
    location))

(defun emit-lexical-var-node (node environments)
  (get-var-ir-symbol (lexical-var-node-name node) environments))

(defun emit-progn-ir (component node environments)
  (let ((nodes (progn-node-forms node))
	(location (make-temp-location))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir component node environments)))
    (make-load-ir component location last-location)
    location))

(defun emit-let-binding (component binding environments)
  (let* ((l-value (lexical-var-node-name binding))
	(r-form (lexical-var-node-form binding))
	(load-from (emit-node-ir component r-form environments)))
    (add-var binding environments)
    (make-load-ir component (get-var-ir-symbol l-value environments) load-from)))

(defun emit-let-ir (component node environments)
  (let ((env (make-env))
	(bindings (let-node-bindings node))
	(form (let-node-form node))
	(location (make-temp-location)))
    (add-env env environments)
    (dolist (binding bindings)
      (emit-let-binding component binding environments))
    (make-load-ir component location (emit-node-ir component form environments))
    (remove-env environments)
    location))

(defun emit-if-ir (component node environments)
  (let ((test-node (if-node-test-form node))
	(true-node (if-node-true-form node))
	(false-node (if-node-false-form node))
	(true-label (make-label))
	(exit-label (make-label))
	(location (make-temp-location)))
    (let ((test-location (emit-node-ir component test-node environments)))
      (make-if-ir component test-location true-label)
      (make-load-ir component location (emit-node-ir component false-node environments))
      (make-go-ir component exit-label)
      (make-label-ir component true-label)
      (make-load-ir component location (emit-node-ir component true-node environments))
      (make-label-ir component exit-label)
      location)))

(defun emit-call-ir (component node environments)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(location (make-temp-location)))
    (let ((arg-locations))
      (dolist (argument arguments)
	(push (emit-node-ir component argument environments) arg-locations))
      (make-fun-params-ir component (reverse arg-locations)))
    (make-call-ir component location fun)
    location))

(defun emit-setq-ir (component node environments)
  (let ((location (emit-node-ir component (setq-node-form node) environments)))
    (make-load-ir component (get-var-ir-symbol (setq-node-symbol node) environments) location)
    (get-var-ir-symbol (setq-node-symbol node) environments)))

(defun emit-tagbody-ir (component node environments)
  (dolist (tnode (tagbody-node-forms node))
    (typecase tnode
      (lexical-var-node (make-label-ir component (lexical-var-node-name tnode)))
      (go-node (emit-go-ir component node))
      (t (emit-node-ir component tnode environments))))
  ;; tagbody returns nil
  nil)

(defun emit-go-ir (component node)
  (make-go-ir component (lexical-var-node-name (go-node-tag node)))
  nil)

;;; lambda will create new component
(defun emit-lambda-ir (component node)
  (let ((lambda-comp (make-ir-component))
	(temp-loc (make-temp-location)))
    (push (cons temp-loc lambda-comp) (ir-component-constants component))
    (make-ir node lambda-comp temp-loc)))

(defun emit-node-ir (component node environments)
  (typecase node
    (lexical-var-node (emit-lexical-var-node node environments))
    (progn-node (emit-progn-ir component node environments))
    (call-node (emit-call-ir component node environments))
    (if-node (emit-if-ir component node environments))
    (let-node (emit-let-ir component node environments))
    (lambda-node (emit-lambda-ir component node))
    (quoted-node (emit-quoted-form-ir component node))
    (immediate-constant-node (emit-constant-ir component node))
    (setq-node (emit-setq-ir component node environments))
    (tagbody-node (emit-tagbody-ir component node environments))
    ;; FIXME, error -> go-node returns nil now, should not return anything
    ;; anyway, dead code elimination in block analyzing solves this
    (go-node (emit-go-ir component node))))

;;;;; MISSING
;;; block
;;; return (return-from nil)
;;; return-from

;;; entry node need to be lambda
(defun make-ir (lambda-node &optional component name)
  (let ((component (or component (make-ir-component))))
    (let ((arguments (lambda-node-arguments lambda-node))
	  (body (lambda-node-body lambda-node))
	  (environments (make-env-holder))
	  (env (make-env)))
      (add-env env environments)
      (make-lambda-entry-ir component (length arguments))
      (make-lambda-arguments-ir component arguments environments)
      (make-return-ir component (emit-node-ir component body environments))
      (remove-env environments))
    (or name component)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IR blocks

(defstruct ir-block ir from-blocks to-blocks)

(defun new-blockp (inst counter)
  (or (= 0 counter)
      (eq (car inst) 'label)))

(defun jump-ir-p (inst)
  (and inst
   (or (eq (first inst) 'if)
       (eq (first inst) 'go))))

(defun branch-inst-p (inst)
  (eq 'if (first inst)))

(defun get-label (jump-ir)
  (and (eq 'label (first jump-ir))
       (second jump-ir)))

(defun get-jump-label (jump-ir)
  (let ((mnemonic (first jump-ir)))
    (and (or (eq 'if mnemonic)
	     (eq 'go mnemonic))
	 (if (eq (first jump-ir) 'if)
	     (fourth jump-ir)
	     (second jump-ir)))))

(defun make-blocks (ir)
  (let ((counter 0)
	(all nil)
	(block nil)
	(previous-was-jump nil))
    (dolist (inst ir)
      (if (or previous-was-jump (new-blockp inst counter))
	  (progn
	    (when block (push (make-ir-block :ir (reverse block)) all))
	    (setf block nil)
	    (push inst block))
	  (progn
	    (push inst block)))
      (incf counter)
      (setf previous-was-jump (jump-ir-p inst)))
    (push (make-ir-block :ir (reverse block)) all)
    (reverse all)))

(defun find-next-block-index (block-index last-ir-inst blocks)
  (let ((branch-inst (branch-inst-p last-ir-inst))
	(jump-label (get-jump-label last-ir-inst))
	(bl (length blocks))
	(to-indexes nil))
    (cond ((= block-index (- bl 1)) nil)
	  ((null jump-label) (push (+ 1 block-index) to-indexes))
	  (t (progn
	       (when branch-inst
		 (push (+ 1 block-index) to-indexes))
	       (dotimes (index bl)
		 (let* ((block (nth index blocks))
			(block-start-label (get-label (first (ir-block-ir block)))))
		   (when (and block-start-label (eq jump-label block-start-label))
		     (push index to-indexes)))))))
    (reverse to-indexes)))

(defun connect-blocks (blocks)
  (dotimes (index (length blocks))
    (let* ((block (nth index blocks))
	   (last-ir-inst (car (last (ir-block-ir block)))))
      (setf (ir-block-to-blocks block) (find-next-block-index index last-ir-inst blocks))))
  (dolist (block blocks)
    (let ((to-block-indexes (ir-block-to-blocks block)))
      (dolist (index to-block-indexes)
	(push index (ir-block-from-blocks (nth index blocks))))))
  blocks)

;;; remove blocks to which no blocks connect to
(defun remove-dead-blocks (blocks)
  (let (live-blocks)
    (dotimes (index (length blocks))
      (let ((block (nth index blocks)))
	(when (or (zerop index)
		  (ir-block-from-blocks block))
	  (push block live-blocks))))
    (reverse live-blocks)))

(defun test-print-ir (exp)
  (let ((ir (make-ir (create-node (expand exp)))))
    (dolist (block (remove-dead-blocks (connect-blocks (make-blocks (ir-component-code (cdr (first (ir-component-constants ir))))))))
      (dolist (inst (ir-block-ir block))
	(print inst))
      (print '-----------------------------------))))

