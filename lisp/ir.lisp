(in-package #:clcomp)

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code constants)

(defparameter *current-component* nil)

(defun normalize (ir-component)
  (make-ir-component :code (reverse (ir-component-code ir-component))
		     :constants (ir-component-constants ir-component)))

(defun special? (symbol)
  (declare (ignore symbol))
  nil)

;;; FIXME, use structures
(defun make-temp-location ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))

(defun emit-ir (ir)
  (push ir (ir-component-code *current-component*)))

(defun emit-constant-ir (node location)
  (list 'load location (list 'constant (constant-node-value node))))

#+nil
(defun emit-quoted-form-ir (node location)
  (list 'load))

(defun emit-progn-ir (location node)
  (let ((nodes (progn-node-forms node))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir (make-temp-location) node)))
    (emit-ir (list 'load location last-location))
    location))

(defun emit-let-binding (binding)
  (let ((l-value (lexical-var-node-name binding))
	(r-form (lexical-var-node-form binding)))
    (emit-ir (list 'load l-value (emit-node-ir l-value r-form)))))

(defun emit-let-ir (location node)
  (let ((bindings (let-node-bindings node))
	(form (let-node-form node)))
    (dolist (binding bindings)
      (emit-let-binding binding))
    (emit-node-ir location form)
    location))

(defun emit-if-ir (location node)
  (let ((test-node (if-node-test-form node))
	(true-node (if-node-true-form node))
	(false-node (if-node-false-form node))
	(test-temp-location (make-temp-location))
	(true-temp-location (make-temp-location))
	(false-temp-location (make-temp-location))
	(true-label (make-label))
	(exit-label (make-label)))
    (let ((test-location (emit-node-ir test-temp-location test-node)))
      (emit-ir (list 'if test-location 'go true-label))
      (emit-ir (list 'load location (emit-node-ir false-temp-location false-node)))
      (emit-ir (list 'go exit-label))
      (emit-ir (list 'label true-label))
      (emit-ir (list 'load location (emit-node-ir true-temp-location true-node)))
      (emit-ir (list 'label exit-label))
      location)))

(defun emit-argument-ir (argument)
  (emit-ir
   (list 'param 
	 (emit-node-ir (make-temp-location) argument))))

(defun emit-call-ir (location node)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node)))
    (dolist (argument arguments)
      (emit-argument-ir argument))
    (emit-ir (list 'load location 'call fun))
    location))

(defun emit-node-ir (location node)
  (typecase node
    (lexical-var-node (lexical-var-node-name node))
    (progn-node (emit-progn-ir location node))
    (call-node (emit-call-ir location node))
    (if-node (emit-if-ir location node))
    (let-node (emit-let-ir location node))))

(defun emit-lambda-ir (lambda)
  (let ((*current-component* (make-ir-component)))
    (let ((arguments (lambda-node-arguments lambda))
	  (body (lambda-node-body lambda)))
      (emit-ir (list 'lambda-entry))
      (emit-ir (list 'arg-check (length arguments)))
      (dolist (arg arguments)
	(emit-ir (list 'receive-param (lexical-var-node-name arg))))
      (let ((location (make-temp-location)))
	(emit-ir (list 'return (emit-node-ir location body)))))
    (normalize *current-component*)))

(defun lambda-ir (lambda)
  (emit-lambda-ir (transform-to-nodes (expand-form lambda))))

(defun print-ir (component)
  (dolist (inst (ir-component-code component))
    (print inst)))

