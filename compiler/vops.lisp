(in-package :clcomp)

(declaim (optimize (speed 0) (safety 3) (debug 3)))


(defparameter *segment-instructions* nil)


(defun inst (&rest rest)
  (push rest *segment-instructions*))

(defparameter *known-vops* (make-hash-table))

(defparameter *c-call-save-registers* '(:rax :rbx :rcx :rdx :rsi :rdi :r8
					:r9 :r10 :r11 :r12 :r13 :r14 :r15))

(defstruct vop name arguments res fun)

(defun get-vop (name)
  (gethash name *known-vops*))

(defun get-args-count (vop)
  (length (vop-arguments vop)))

(defun get-args-types (vop)
  (mapcar 'second (vop-arguments vop)))

(defun get-res-type (vop)
  (second (vop-res vop)))

#+nil
(defmacro define-vop (name res
		      (&rest arguments)
		      &body body)
  `(setf (gethash ',name *known-vops*)
	 (make-vop :name ',name
		   :res ',res
		   :arguments ',arguments
		   :fun (lambda ,(cons (first res) (append (mapcar 'car arguments) (list '$stack-top-operand$)))
			  (declare (ignorable $stack-top-operand$))
			  (progn
			    ,@body)))))


(defun generate-alias-proof-vop-body (body arguments res)
  `(let ,(loop for arg in arguments
	       collect `(,(first arg) (if (eq ,(first arg) ,(first res))
					  (progn
					    (inst :mov *tmp-reg-2* ,(first arg))
					    *tmp-reg-2*)
					  ,(first arg))))
     (progn ,@body)))

(defmacro define-vop (name res
		      (&rest arguments)
		      &body body)
  `(setf (gethash ',name *known-vops*)
	 (make-vop :name ',name
		   :res ',res
		   :arguments ',arguments
		   :fun (lambda ,(cons (first res) (append (mapcar 'car arguments) (list '$stack-top-operand$)))
			  (declare (ignorable $stack-top-operand$))
			  ,(generate-alias-proof-vop-body body arguments res)))))

(defun make-vop-label (name)
  (gensym name))

(defun inline-vop (vops &rest args)
  (dolist (i (get-vop-code (get-vop vops) args))
    (push i *segment-instructions*)))

(defun get-vop-code (vop  args)
  (let ((*segment-instructions* nil))
    (apply (vop-fun vop) args)
    (reverse *segment-instructions*)))
