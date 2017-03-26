(in-package #:clcomp)
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code constants fixups)

(defparameter *special* nil)

(defun special? (symbol)
  (member symbol *special*))

;;; FIXME, use structures
(defun make-temp-location ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))


;;; IR code generators

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

(defun make-quoted-ir (component to from)
  (add-ir component (list (list 'load-quoted to from))))

(defun make-call-ir (component location fun)
  (add-ir component (list (list 'load location 'call fun))))

(defun make-return-ir (component location)
  (add-ir component (list (list 'return location))))

(defun make-lambda-entry-ir (component number-of-arguments)
  (add-ir component (list (list 'lambda-entry) (list 'arg-check number-of-arguments))))

(defun make-lambda-arguments-ir (component arguments)
  (dolist (arg arguments)
    (add-ir component (list (list 'receive-param (lexical-var-node-name arg))))))

(defun make-fun-params-ir (component arg-locations)
  (dolist (arg-loc arg-locations)
    (add-ir component (list (list 'param arg-loc)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emiters

(defun emit-constant-ir (component node)
  (declare (ignore component))
  (immediate-constant-node-value node))

;;; FIXME, this is not right
;;; quick solution: transform this to runtime list
(defun emit-quoted-form-ir (component node)
  (let ((location (make-temp-location)))
    (make-quoted-ir component location (quoted-node-form node))
   location))

(defun emit-progn-ir (component node)
  (let ((nodes (progn-node-forms node))
	(location (make-temp-location))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir component node)))
    (make-load-ir component location last-location)
    location))

(defun emit-let-binding (component binding)
  (let ((l-value (lexical-var-node-name binding))
	(r-form (lexical-var-node-form binding)))
    (make-load-ir component l-value (emit-node-ir component r-form))))

(defun emit-let-ir (component node)
  (let ((bindings (let-node-bindings node))
	(form (let-node-form node))
	(location (make-temp-location)))
    (dolist (binding bindings)
      (emit-let-binding component binding))
    (make-load-ir component location (emit-node-ir component form))
    location))

(defun emit-if-ir (component node)
  (let ((test-node (if-node-test-form node))
	(true-node (if-node-true-form node))
	(false-node (if-node-false-form node))
	(true-label (make-label))
	(exit-label (make-label))
	(location (make-temp-location)))
    (let ((test-location (emit-node-ir component test-node)))
      (make-if-ir component test-location true-label)
      (make-load-ir component location (emit-node-ir component false-node))
      (make-go-ir component exit-label)
      (make-label-ir component true-label)
      (make-load-ir component location (emit-node-ir component true-node))
      (make-label-ir component exit-label)
      location)))

(defun emit-call-ir (component node)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(location (make-temp-location)))
    (let ((arg-locations))
      (dolist (argument arguments)
	(push (emit-node-ir component argument) arg-locations))
      (make-fun-params-ir component (reverse arg-locations)))
    (make-call-ir component location fun)
    location))

(defun emit-setq-ir (component node)
  (let ((location (emit-node-ir component (setq-node-form node))))
    (make-load-ir component (setq-node-symbol node) location)
    (setq-node-symbol node)))

(defun emit-tagbody-ir (component node)
  (dolist (tnode (tagbody-node-forms node))
    (typecase tnode
      (lexical-var-node (make-label-ir component (lexical-var-node-name tnode)))
      (go-node (emit-go-ir component node))
      (t (emit-node-ir component tnode))))
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

(defun emit-node-ir (component node)
  (typecase node
    (lexical-var-node (lexical-var-node-name node))
    (progn-node (emit-progn-ir component node))
    (call-node (emit-call-ir component node))
    (if-node (emit-if-ir component node))
    (let-node (emit-let-ir component node))
    (lambda-node (emit-lambda-ir component node))
    (quoted-node (emit-quoted-form-ir component node))
    (immediate-constant-node (emit-constant-ir component node))
    (setq-node (emit-setq-ir component node))
    (tagbody-node (emit-tagbody-ir component node))
    ;; FIXME, error -> go-node returns nil now, should not return anything
    ;; anyway, dead code elimination in block analyzing solves this
    (go-node (emit-go-ir component node))))

;;;;; MISSING
;;; block
;;; return (return-from nil)
;;; return-from

(defun make-ir (lambda-node &optional component name)
  (let ((component (or component (make-ir-component))))
    (let ((arguments (lambda-node-arguments lambda-node))
	  (body (lambda-node-body lambda-node)))
      (make-lambda-entry-ir component (length arguments))
      (make-lambda-arguments-ir component arguments)
      (make-return-ir component (emit-node-ir component body)))
    (or name component)))

(defun print-ir (component)
  (dolist (inst (ir-component-code component))
    (print inst)))


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

