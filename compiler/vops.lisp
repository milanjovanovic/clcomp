(in-package :clcomp)

(declaim (optimize (speed 0) (safety 3) (debug 3)))


(defparameter *segment-instructions* nil)


(defun inst (&rest rest)
  (push rest *segment-instructions*))

(defun add-instructions (instructions)
  (setf *segment-instructions* (append (reverse instructions)
				       *segment-instructions*)))

(defparameter *known-vops* (make-hash-table))

(defparameter *c-call-save-registers* '(:rax :rbx :rcx :rdx :rsi :rdi :r8
					:r9 :r10 :r11 :r12 :r13 :r14 :r15))

(defstruct vop name arguments arguments-metadata res clobber alias-regs fun version)

(defun get-vop (name)
  (gethash name *known-vops*))

(defun get-args-count (vop)
  (length (vop-arguments vop)))

(defun get-args-types (vop)
  (mapcar 'second (vop-arguments vop)))

(defun get-res-type (vop)
  (second (vop-res vop)))

(defun get-res-types (vop)
  (mapcar #'second (vop-res vop)))

(defun vop-argument-metadata-bcell (metadata)
  (find :bcell metadata))

(defun generate-alias-proof-vop-body (body arguments res)
  `(let ,(loop for arg in arguments
	       collect `(,(first arg) (if (eq ,(first arg) ,(first res))
					  (progn
					    (inst :mov *tmp-reg-2* ,(first arg))
					    *tmp-reg-2*)
					  ,(first arg))))
     (progn ,@body)))

(defun generate-alias-proof-vop-body-mv (body arguments res alias-regs)
  (declare (optimize debug))
  (let ((args (mapcar #'first arguments))
	(res (mapcar #'first res))
	(res-list (gensym "res-list-"))
	(aregs (gensym "alias-regs-"))
	(ar (gensym "alias-register-")))
    `(let ((,res-list (list ,@res))
	   (,aregs ,alias-regs))
       (let ,(loop for arg in args
		   collect `(,arg (if (find ,arg ,res-list)
				      (progn
					(let ((,ar (first ,aregs)))
					  (when (null ,ar)
					    (error "Missing alias reg"))
					  (setf ,aregs (cdr ,aregs))
					  (inst :mov ,ar ,arg)
					  ,ar))
				      ,arg)))
	 (progn ,@body)))))

(defun operand-argument (operand)
  (let ((res (list (first operand))))
    (dolist (od (rest operand))
      (when  (member od '(:register :stack :immediate))
	(push od res)))
    (reverse res)))

(defun operand-metadata (operand)
  (let ((res nil))
    (dolist (od (rest operand))
      (when  (not (member od '(:register :stack :immediate)))
	(push od res)))
    (reverse res)))

(defun parse-operands (operands fun)
  (let ((ops nil))
    (dolist (op operands)
      (push (funcall fun op) ops))
    (reverse ops)))

(defun operands-arguments-storage (operands)
  (parse-operands operands #'operand-argument))

(defun operands-arguments-metadata (operands)
  (parse-operands operands #'operand-metadata))

;;; FIXME, look in original DEFINE-VOP and (&rest res), this is for multiple values
(defmacro define-vop (name
		      (&rest res)
		      (&rest arguments)
		      &body body)
  `(setf (gethash ',name *known-vops*)
	 (make-vop :name ',name
		   :version :v1
		   :res ,(when res `'(,res))
		   :arguments ',(operands-arguments-storage arguments)
		   :arguments-metadata ',(operands-arguments-metadata arguments)
		   :fun (lambda ,(if res (cons (first res) (append (mapcar 'car arguments) (list '$stack-top-operand$)))
				     (append (mapcar 'car arguments) (list '$stack-top-operand$)))
			  (declare (ignorable $stack-top-operand$))
			  ,(generate-alias-proof-vop-body body arguments res)))))

(defmacro define-mv-vop (name
			 (&rest res)
			 (&rest arguments)
			 (&rest info)
			 &body body)
  `(setf (gethash ',name *known-vops*)
	 (make-vop :name ',name
		   :version :v2
		   :res ',res
		   :arguments ',(operands-arguments-storage arguments)
		   :arguments-metadata ',(operands-arguments-metadata arguments)
		   :clobber ',(second (assoc :clobber info))
		   :alias-regs (list ,@(second (assoc :alias-regs info)))
		   :fun (lambda ,(append (mapcar 'car res) (append (mapcar 'car arguments) (list '$stack-top-operand$)))
			  (declare (ignorable $stack-top-operand$))
			  ,(generate-alias-proof-vop-body-mv body arguments res (cons 'list (second (assoc :alias-regs info))))))))


(defun make-vop-label (name)
  (gensym name))

(defun inline-vop (vops &rest args)
  (dolist (i (get-vop-code (get-vop vops) args))
    (push i *segment-instructions*)))

(defun get-vop-code (vop  args)
  (let ((*segment-instructions* nil))
    (apply (vop-fun vop) args)
    (reverse *segment-instructions*)))

(defun operand-types-match (caller-operands vop-operands)
  (and
   (= (length caller-operands) (length vop-operands))
   (do ((cops caller-operands (cdr cops))
	(vops vop-operands (cdr vops)))
       ((null cops) t)
     (unless (find (car cops) (car vops))
       (return)))))

(defun find-vop (name result arguments)
  (let ((vop (get-vop name)))
    (when vop
      (let ((vop-args (vop-arguments vop))
	    (vop-result (vop-res vop)))
	(when (and (operand-types-match result vop-result)
		   (operand-types-match arguments vop-args))
	  vop)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code stubs
(defparameter *stubs* (make-hash-table))

(defmacro define-stub (name &body body)
  `(let ((*segment-instructions* nil))
     (progn ,@body)
     (setf (gethash ',name *stubs*)
	   (reverse *segment-instructions*))))
