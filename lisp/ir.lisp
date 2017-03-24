(in-package #:clcomp)

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code constants fixups)

(defparameter *special* nil)

(defun normalize (ir-component)
  (make-ir-component :code (reverse (ir-component-code ir-component))
		     :constants (ir-component-constants ir-component)))

(defun special? (symbol)
  (member symbol *special*))

;;; FIXME, use structures
(defun make-temp-location ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))

(defun emit-ir (component ir)
  (push ir (ir-component-code component)))

(defun emit-constant-ir (node location)
  (list 'load location (list 'constant (immediate-constant-node-value node))))

;;; FIXME, this is not right
(defun emit-quoted-form-ir (component node location)
  (list 'load location (quoted-node-form node)))

(defun emit-progn-ir (component location node)
  (let ((nodes (progn-node-forms node))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir component (make-temp-location) node)))
    (emit-ir component (list 'load location last-location))
    location))

(defun emit-let-binding (component binding)
  (let ((l-value (lexical-var-node-name binding))
	(r-form (lexical-var-node-form binding)))
    (emit-ir component (list 'load l-value (emit-node-ir component l-value r-form)))))

(defun emit-let-ir (component location node)
  (let ((bindings (let-node-bindings node))
	(form (let-node-form node)))
    (dolist (binding bindings)
      (emit-let-binding component binding))
    (emit-node-ir component location form)
    location))

(defun emit-if-ir (component location node)
  (let ((test-node (if-node-test-form node))
	(true-node (if-node-true-form node))
	(false-node (if-node-false-form node))
	(test-temp-location (make-temp-location))
	(true-temp-location (make-temp-location))
	(false-temp-location (make-temp-location))
	(true-label (make-label))
	(exit-label (make-label)))
    (let ((test-location (emit-node-ir component test-temp-location test-node)))
      (emit-ir component (list 'if test-location 'go true-label))
      (emit-ir component (list 'load location (emit-node-ir component false-temp-location false-node)))
      (emit-ir component (list 'go exit-label))
      (emit-ir component (list 'label true-label))
      (emit-ir component (list 'load location (emit-node-ir component true-temp-location true-node)))
      (emit-ir component (list 'label exit-label))
      location)))

(defun emit-argument-ir (component argument)
  (emit-ir component
   (list 'param 
	 (emit-node-ir component (make-temp-location) argument))))

(defun emit-call-ir (component location node)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node)))
    (dolist (argument arguments)
      (emit-argument-ir component argument))
    (emit-ir component (list 'load location 'call fun))
    location))

(defun emit-node-ir (component location node)
  (typecase node
    (lexical-var-node (lexical-var-node-name node))
    (progn-node (emit-progn-ir component location node))
    (call-node (emit-call-ir component location node))
    (if-node (emit-if-ir component location node))
    (let-node (emit-let-ir component location node))
    (lambda-node (let ((lambda-comp (make-ir-component))
		       (temp-loc (make-temp-location)))
		   (push (cons temp-loc lambda-comp) (ir-component-constants component))
		   (make-ir node lambda-comp temp-loc)))
    ;; FIXME, this is not right for quoted forms, we should call list ?
    (quoted-node (emit-quoted-form-ir component node location))))

(defun make-ir (lambda-node &optional component name)
  (let ((component (or component (make-ir-component))))
    (let ((arguments (lambda-node-arguments lambda-node))
	  (body (lambda-node-body lambda-node)))
      (emit-ir component (list 'lambda-entry))
      (emit-ir component (list 'arg-check (length arguments)))
      (dolist (arg arguments)
	(emit-ir component (list 'receive-param (lexical-var-node-name arg))))
      (let ((location (make-temp-location)))
	(emit-ir component (list 'return (emit-node-ir component location body)))))
    (or name (normalize component))))

(defun print-ir (component)
  (dolist (inst (ir-component-code component))
    (print inst)))
