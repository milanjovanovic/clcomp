(in-package :clcomp)
(declaim (optimize (speed 0) (debug 3)))

(defstruct place)
(defstruct (arg-place (:include place)) index)
(defstruct (named-place (:include place)) name)

(defstruct (rcv-argument-place (:include arg-place)))
(defstruct (argument-place (:include arg-place)))
(defstruct (return-value-place (:include place)) index)

(defstruct (temp-place (:include named-place)))
(defstruct (var-place (:include named-place)))

(defstruct slabels alist)
(defstruct ssa-env labels)
(defstruct lambda-ssa blocks (env (make-ssa-env)))
(defstruct ssa-block index ir ssa succ jump defined)

(defstruct ssa-form index)
(defstruct (ssa-load (:include ssa-form)) to from)
(defstruct (ssa-go (:include ssa-form)) label)
(defstruct (ssa-label (:include ssa-form)) label)
(defstruct (ssa-value (:include ssa-form)) value)

(defstruct (ssa-multiple-return (:include ssa-form)))
(defstruct (ssa-single-return (:include ssa-form)) value)

(defstruct (ssa-fun-call (:include ssa-form)) fun)
(defstruct (ssa-unknown-values-fun-call (:include ssa-form)) fun)
(defstruct (ssa-known-values-fun-call (:include ssa-form)) fun)
(defstruct (ssa-if (:include ssa-form)) test true-block)

(defparameter *ssa-symbol-counter* 0)
(defun generate-temp-place (&optional (s "T-"))
  (prog1
      (make-temp-place :name (make-symbol (concatenate 'string s (write-to-string *ssa-symbol-counter*))))
    (incf *ssa-symbol-counter*)))

(defparameter *if-label-counter* 0)
(defun generate-if-label-symbol ()
  (prog1
      (make-symbol (concatenate 'string "IF-NEXT-BLOCK-LABEL-" (write-to-string *ssa-symbol-counter*)))
    (incf *ssa-symbol-counter*)))

(defparameter *ssa-block-counter* 0)
(defun make-new-ssa-block ()
  (prog1
      (make-ssa-block :index *ssa-block-counter*)
    (incf *ssa-block-counter*)))

(defun ssa-add-block (lambda-ssa block)
  (push block (lambda-ssa-blocks lambda-ssa)))

(defun ssa-connect-blocks (b1 b2)
  (setf (ssa-block-succ b1) (ssa-block-index b2)))

(defun insert-block-jump (block jump-block-index)
  (setf (ssa-block-jump block) jump-block-index))

(defun pop-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (pop (ssa-env-labels ssa-env))))

(defun push-labels-env (lambda-ssa)
  (let ((ssa-env (lambda-ssa-env lambda-ssa)))
    (push (make-slabels) (ssa-env-labels ssa-env))))

(defun ssa-add-block-label (lambda-ssa block label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (first (ssa-env-labels env))))
    (push (cons label (ssa-block-index block))
	  (slabels-alist slabels))))

(defun ssa-find-block-index-by-label (lambda-ssa label)
  (let* ((env (lambda-ssa-env lambda-ssa))
	 (slabels (ssa-env-labels env)))
    (dolist (slabel slabels)
      (let ((cons (assoc label (slabels-alist slabel))))
	(when cons
	  (return-from ssa-find-block-index-by-label (cdr cons) ))))))

(defparameter *ir-index-counter* 0)
(defun emit (lambda-ssa ssa block)
  (declare (ignore lambda-ssa)
	   (optimize (debug 3)))
  (when (ssa-form-p ssa)
    (setf (ssa-form-index ssa) *ir-index-counter*)
    (incf *ir-index-counter*))
  (push ssa (ssa-block-ir block)))

(defun emit-if-node-ssa (if-node lambda-ssa leaf place block)
  (let* ((test-node (if-node-test-form if-node))
	 (test-place (generate-temp-place))
	 (false-block (make-new-ssa-block))
	 (true-block (make-new-ssa-block))
	 (if-label-symbol (generate-if-label-symbol))
	 (next-block (unless leaf (make-new-ssa-block))))
    (unless leaf
      (ssa-connect-blocks true-block next-block))
    (ssa-connect-blocks block false-block)
    (ssa-add-block lambda-ssa false-block)
    (ssa-add-block lambda-ssa true-block)
    (unless leaf
      (ssa-add-block lambda-ssa next-block))
    (emit-ssa test-node lambda-ssa leaf test-place block)
    (emit-ssa (if-node-true-form if-node) lambda-ssa leaf place true-block)
    (emit-ssa (if-node-false-form if-node) lambda-ssa leaf place false-block)
    (insert-block-jump block (ssa-block-index true-block))
    (unless leaf
      (emit lambda-ssa (make-ssa-label :label if-label-symbol) next-block)
      (emit lambda-ssa (make-ssa-go :label if-label-symbol) false-block)
      (insert-block-jump false-block (ssa-block-index next-block)))
    (emit lambda-ssa
	  (make-ssa-if
	   :test test-place
	   :true-block (ssa-block-index true-block))
	  block)
    next-block))

(defun maybe-emit-direct-load (node lambda-ssa leaf place block)
  (etypecase node
    (immediate-constant-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from node) block)
     (when leaf
       (emit lambda-ssa (make-ssa-single-return :value place) block))
     block)
    (lexical-var-node
     (emit lambda-ssa (make-ssa-load :to place
    				     :from (make-var-place :name (lexical-var-node-name node))) block)
     (when leaf (emit lambda-ssa (make-ssa-single-return :value place) block))
     block)
    (t (emit-ssa node lambda-ssa leaf place block))))

;;; FIXME, there is more here
(defun make-direct-place-or-nil (node)
  (etypecase node
    (immediate-constant-node node)
    (lexical-var-node (make-var-place :name (lexical-var-node-name node)))
    (t nil)))

;;; FIXME, fun can be CLOSURE or LAMBDA
(defun emit-call-node-ssa (node lambda-ssa leaf place block)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(args-places nil))
    (dolist (arg arguments)
      (let ((direct-place (make-direct-place-or-nil arg)))
	(if direct-place
	    (push direct-place args-places)
	    (let* ((place (generate-temp-place))
		   (new-block (maybe-emit-direct-load arg lambda-ssa nil place block)))
	      (setf block new-block)
	      (push place args-places)))))
    (let ((arg-index 0))
      (dolist (p (reverse args-places))
	(emit lambda-ssa (make-ssa-load :to (make-argument-place :index arg-index) :from p) block)
	(incf arg-index)))
    (if (null place)
	(progn
	  (emit lambda-ssa
		(make-ssa-unknown-values-fun-call :fun fun)
		block)
	  (when leaf
	    (emit lambda-ssa (make-ssa-multiple-return) block))
	  block)
	(progn
	  (emit lambda-ssa (make-ssa-fun-call :fun fun) block)
	  (emit lambda-ssa (make-ssa-load :to place :from (make-return-value-place :index 0)) block)
	  block))))

(defun emit-lexical-binding-node-ssa (node lambda-ssa leaf block)
  (declare (ignore leaf))
  (let ((lvar (lexical-binding-node-name node))
	(form (lexical-binding-node-form node)))
    (maybe-emit-direct-load form lambda-ssa nil (make-var-place :name lvar) block)))

(defun emit-let-node-ssa (node lambda-ssa leaf place block)
  (dolist (n (let-node-bindings node))
    (let ((new-block (emit-lexical-binding-node-ssa n lambda-ssa nil block)))
      (setf block new-block)))
  (emit-ssa (let-node-form node) lambda-ssa leaf place block))

(defun emit-immediate-node-ssa (node lambda-ssa leaf place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from node) block)
      (if leaf
	  (emit lambda-ssa (make-ssa-single-return :value node) block)
	  (emit lambda-ssa (make-ssa-value :value node) block)))
  block)

(defun emit-lexical-var-node-ssa (node lambda-ssa leaf place block)
  (if place
      (emit lambda-ssa (make-ssa-load :to place :from (make-var-place :name (lexical-var-node-name node))) block )
      (if leaf
	  (emit lambda-ssa (make-ssa-single-return :value (make-var-place :name (lexical-var-node-name node))) block)
	  (emit lambda-ssa (make-ssa-value :value
					   (make-var-place :name (lexical-var-node-name node))) block)))
  block)

(defun emit-progn-node-ssa (node lambda-ssa leaf place block)
  (do* ((forms (progn-node-forms node) (cdr forms))
	(form-node (car forms) (car forms)))
       ((null forms) block)
    (if (cdr forms)
	(setf block (emit-ssa form-node lambda-ssa nil nil block))
	(setf block (emit-ssa form-node lambda-ssa leaf place block)))))

(defun emit-tagbody-node-ssa (node lambda-ssa leaf place block)
  (push-labels-env lambda-ssa)
  (let ((labels-blocks (make-hash-table))
	(current-block block))
    (dolist (node (tagbody-node-forms node))
      (when (label-node-p node)
	(let ((label-block (make-new-ssa-block)))
	  (ssa-add-block-label lambda-ssa label-block (label-node-label node))
	  (setf (gethash (label-node-label node) labels-blocks) label-block))))
    (dolist (form-node (tagbody-node-forms node))
      (if (label-node-p form-node)
	  (let ((lblock (gethash (label-node-label form-node) labels-blocks)))
	    ;; connect previus block with this label block only if there is no direct jump from previous to other block
	    (ssa-add-block lambda-ssa lblock)
	    (when (null (ssa-block-jump current-block))
	      (ssa-connect-blocks current-block lblock))
	    (setf current-block lblock)
	    (emit lambda-ssa (make-ssa-label :label (label-node-label form-node)) current-block))
	  (setf current-block (emit-ssa form-node lambda-ssa nil nil current-block))))
    (pop-labels-env lambda-ssa)
    (if place
	(emit lambda-ssa (make-ssa-load :to place :from (make-immediate-constant :constant *nil*)) block)
	(if leaf
	    (emit lambda-ssa (make-ssa-single-return :value (make-immediate-constant :constant *nil*)) block)
	    (emit lambda-ssa (make-immediate-constant :constant *nil*) block)))
    current-block))

(defun emit-go-node-ssa (node lambda-ssa leaf place block)
  (declare (ignore leaf place))
  (let ((label-name (label-node-label (go-node-label-node node)) ))
    (insert-block-jump block (ssa-find-block-index-by-label lambda-ssa label-name))
    (emit lambda-ssa (make-ssa-go :label label-name) block)
    ;; reset successor to nil, if we have GO direct successor in this block is never active
    (setf (ssa-block-succ block) nil)
    block))

(defun emit-setq-node-ssa (node lambda-ssa leaf place block)
  (let ((new-block (maybe-emit-direct-load (setq-node-form node)
					   lambda-ssa leaf
					   (make-var-place
					    :name (lexical-var-node-name
						   (setq-node-var node)))
					   block)))
    (if place
	(maybe-emit-direct-load (setq-node-var node) lambda-ssa leaf place new-block))
    new-block))

(defun emit-lambda-arguments-ssa (arguments lambda-ssa block)
  (let ((index 0))
    (dolist (argument arguments)
      (etypecase argument
	(lexical-var-node
	 (emit lambda-ssa (make-ssa-load :to (make-var-place :name (lexical-var-node-name argument))
					 :from (make-rcv-argument-place :index index))
	       block)))
      (incf index))))

(defun emit-ssa (node lambda-ssa leaf place block)
  (etypecase node
    (if-node (emit-if-node-ssa node lambda-ssa leaf place block))
    (call-node (emit-call-node-ssa node lambda-ssa leaf place block))
    (let-node (emit-let-node-ssa node lambda-ssa leaf place block))
    (progn-node (emit-progn-node-ssa node lambda-ssa leaf place block))
    (lexical-var-node (emit-lexical-var-node-ssa node lambda-ssa leaf place block))
    (immediate-constant-node (emit-immediate-node-ssa node lambda-ssa leaf place block))
    (tagbody-node (emit-tagbody-node-ssa node lambda-ssa leaf place block))
    (go-node (emit-go-node-ssa node lambda-ssa leaf place block))
    (setq-node (emit-setq-node-ssa node lambda-ssa leaf place block))))

(defun ssa-normalize-lambda-ssa (lambda-ssa)
   (let ((blocks nil))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (setf (ssa-block-ir b)
	    (reverse (ssa-block-ir b)))
      (push b blocks))
    (setf (lambda-ssa-blocks lambda-ssa) blocks))
  lambda-ssa)

;;; value numbering

;;; TESTING
(defstruct transform-env predecessors blocks-map)
(defstruct phi operands)

(defun phi-add-operand (phi operand)
  (push operand (phi-operands phi)))

(defun get-block-def (block symbol)
  (cdr (assoc symbol (ssa-block-defined block))))

(defun set-block-def (block var value)
  (let ((cons (assoc var (ssa-block-defined block))))
    (if cons
	(setf (cdr cons) value)
	(setf (ssa-block-defined block)
	      (acons var value (ssa-block-defined block))))
    value))

(defun get-block-predecessors (block env)
  (second (assoc (ssa-block-index block) (transform-env-predecessors env))))

(defun get-block-by-index (index env)
  (gethash index (transform-env-blocks-map env)))

(defun ssa-add-predecessor (predecessors block-index predecessor)
  (let ((p (assoc block-index predecessors)))
    (cond (p (if (find predecessor (second p))
		 predecessors
		 (progn
		   (setf (second p)
			 (cons predecessor (second p)))
		   predecessors)))
	  (t (cons (list block-index (list predecessor)) predecessors)))))

(defun ssa-make-value-numbering-env (lambda-ssa)
  (let ((predecessors nil)
	(blocks-map (make-hash-table)))
    (dolist (block (lambda-ssa-blocks lambda-ssa))
      (setf (gethash (ssa-block-index block) blocks-map) block)
      (let ((succ (ssa-block-succ block))
	    (jump (ssa-block-jump block)))
	(when succ
	  (setf predecessors
		(ssa-add-predecessor predecessors succ (ssa-block-index block))))
	(when jump
	  (setf predecessors
		(ssa-add-predecessor predecessors jump (ssa-block-index block))))))
    (make-transform-env :predecessors predecessors
			:blocks-map blocks-map)))

(defun ssa-write-variable (place block env)
  (declare (ignore env))
  (let ((vplace (generate-temp-place "V-")))
    (set-block-def block (named-place-name place) vplace)))

(defun try-remove-trivial-phi (phi)
  (declare (optimize debug))
  (let ((same nil))
    (dolist (operand (phi-operands phi))
      (when (not (or (eq same operand)
		     (eq operand phi)))
	(if (not (eq nil operand))
	    (return-from try-remove-trivial-phi phi)
	    (setf same operand))))
    ;; instead of list operands will be atom value
    (setf (phi-operands phi) same)))

(defun phi-add-operands (place phi predecessors env)
  (declare (optimize (speed 0) (debug 3)))
  (dolist (pblock predecessors)
    (phi-add-operand phi (ssa-read-variable place (get-block-by-index pblock env) env)))
  (try-remove-trivial-phi phi))

(defun read-variable-recursive (place block env)
  (let ((predecessors (get-block-predecessors block env)))
    (when predecessors
      (if (= (length predecessors) 1)
	  (set-block-def block (named-place-name place)
			 (ssa-read-variable place (get-block-by-index (first predecessors) env) env))
	  (let ((phi (make-phi)))
	    (set-block-def block (named-place-name place) phi)
	    (set-block-def block (named-place-name place)
			   (phi-add-operands place phi predecessors env)))))))

(defun ssa-read-variable (place block env)
  (declare (optimize (speed 0)))
  (or (get-block-def block (named-place-name place)) 
      (read-variable-recursive place block env)))

(defun transform-write (place block env)
  (declare (optimize (speed 0)))
  (typecase place
    (named-place
     (ssa-write-variable place block env))
    (t place)))

(defun transform-read  (place block env)
  (declare (optimize (speed 0)))
  (typecase place
    (named-place (ssa-read-variable place block env))
    (t place)))

(defun ssa-transform-block-ir (b env)
  (let ((ssa-ir nil))
    (dolist (ir (ssa-block-ir b))
      (let ((irssa (etypecase ir
		     (ssa-load (make-ssa-load :index (ssa-load-index ir)
					      :to (transform-write (ssa-load-to ir) b env)
					      :from (transform-read (ssa-load-from ir) b env)))
		     (ssa-value (make-ssa-value :index (ssa-value-index ir)
						:value (transform-read (ssa-value-value ir) b env)))
		     (ssa-if (make-ssa-if :index (ssa-if-index ir)
					  :test (transform-read (ssa-if-test ir) b env)
					  :true-block (ssa-if-true-block ir)))
		     (ssa-single-return (make-ssa-single-return :index (ssa-single-return-index ir)
								:value (transform-read (ssa-single-return-value ir) b env)))
		     (t ir))))
	(push irssa ssa-ir)))
    (setf (ssa-block-ssa b) (reverse ssa-ir))))

(defun do-value-numbering (lambda-ssa)
  (declare (optimize (debug 3) (speed 0)))
  (let ((*ssa-symbol-counter* 0)
	(env (ssa-make-value-numbering-env lambda-ssa)))
    (dolist (b (lambda-ssa-blocks lambda-ssa))
      (ssa-transform-block-ir b env)))
  lambda-ssa)

(defun ssa-parse-lambda (lambda-node)
  (let* ((*ssa-block-counter* 0)
	 (*ir-index-counter* 0)
	 (*ssa-symbol-counter* 0)
	 (lambda-ssa (make-lambda-ssa))
	 (entry-block (make-new-ssa-block)))
    (ssa-add-block lambda-ssa entry-block)
    (emit-lambda-arguments-ssa (lambda-node-arguments lambda-node ) lambda-ssa entry-block)
    (emit-ssa (lambda-node-body lambda-node) lambda-ssa t nil entry-block)
    (ssa-normalize-lambda-ssa lambda-ssa)))
