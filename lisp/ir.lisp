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

(defun make-temp-location-symbol ()
  (incf *temp-counter*)
  (intern (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-temp-location ()
  (make-tmp-location :location (make-temp-location-symbol)))

(defun make-label ()
  (incf *label-counter*)
  (intern (concatenate 'string "LABEL-" (write-to-string *label-counter*))))

(defun make-return-location ()
  (make-ret-location :location (make-temp-location-symbol)))

(defstruct rip-relative-location location)
(defstruct var-location location)
(defstruct tmp-location location)
(defstruct ret-location location)
(defstruct param-location location param-number)
(defstruct immediate-constant constant)

(defun location-symbol (location)
  (typecase location
    (tmp-location (tmp-location-location location))
    (var-location (var-location-location location))
    (ret-location (ret-location-location location))
    (otherwise (error "Unknown location type"))))

;;; environments

(defparameter *env-counter* 0)

(defun reset-env-counter ()
  (setf *env-counter* 0 ))

(defun make-env-holder ()
  (list 'envs nil nil))

(defun make-env ()
  (prog1
      (list *env-counter* (make-hash-table) )
    (incf *env-counter*)))

(defun add-env (env env-holder)
  (push env (second env-holder))
  env-holder)

(defun remove-env (all-env)
  (pop (second all-env)))

(defun add-var (var-node environments)
  (let* ((env (first (second environments)))
	 (env-number (first env))
	 (env-map (second env)))
    (setf (gethash (lexical-var-node-name var-node) env-map) env-number)))


(defun get-var-ir-symbol (var-node env-holder)
  (dolist (env (second env-holder))
    (let* ((env-map (second env))
	   (var-env-number (gethash (lexical-var-node-name var-node) env-map)))
      (when var-env-number
	(return-from get-var-ir-symbol
	  (make-var-location :location
	   (intern (concatenate 'string (symbol-name (lexical-var-node-name var-node))
				(concatenate 'string "-" (write-to-string var-env-number)))))))
      ;; FIXME, unboud vars threat as special vars
      ;;      (error (concatenate 'string "The variable " (symbol-name var-name) " is unbound"))
      )))

;;; go tags environment
(defparameter *tagbody-envs-counter* 0)

(defun make-tagbody-env ()
  (prog1
      (list *tagbody-envs-counter* (make-hash-table))
    (incf *tagbody-envs-counter*)))

(defun add-tagbody-env (tagb-env environments)
  (push tagb-env (third environments))
  environments)

(defun remove-tagbody-env (environments)
  (pop (third environments)))

(defun add-go-label (label-node environments)
  (let* ((first-env (first (third environments)))
	 (env-number (first first-env))
	 (tags-hash (second first-env)))
    (setf (gethash (label-node-label label-node) tags-hash) env-number)))

(defun get-label-ir-symbol (label-node environments)
  (dolist (env (third environments))
    (let* ((env-map (second env))
	   (var-env-number (gethash (label-node-label label-node) env-map)))
      (when var-env-number
	(return-from  get-label-ir-symbol
	  (intern (concatenate 'string (symbol-name (label-node-label label-node))
			       (concatenate 'string "-" (write-to-string var-env-number))))))))
  (error (concatenate 'string "Go tag " (symbol-name (label-node-label label-node)) " does not exist ")))


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
  (add-ir component (list (list 'load-return location 'call fun))))

(defun make-return-ir (component location)
  (add-ir component (list (list 'load (make-return-location) location))))

(defun make-lambda-entry-ir (component number-of-arguments)
  (add-ir component (list (list 'lambda-entry) (list 'arg-check number-of-arguments))))

(defun make-lambda-arguments-ir (component arguments environments)
  (dolist (arg arguments)
    (add-var arg environments)
    (add-ir component (list (list 'receive-param (get-var-ir-symbol arg environments))))))

(defun make-fun-params-ir (component arg-locations)
  (add-ir component (list (list 'params-count (length arg-locations))))
  (let ((counter 1))
    (dolist (arg-loc arg-locations)
      (add-ir component (list (list 'load-param (make-param-location :location (make-temp-location-symbol) :param-number counter) arg-loc)))
      (incf counter))))

(defun get-ir-used-location (ir)
  (let ((mnem (first ir)))
    (cond ((eq mnem 'if)
	   (list (second ir)))
	  ((eq mnem 'load)
	   (list (second ir) (third ir)))
	  ((eq mnem 'load-return)
	   (list (second ir)))
	  ((eq mnem 'param)
	   (list (second ir)))
	  ((eq mnem 'return)
	   (list (second ir)))
	  (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Emiters

;;; FIXME, numbers that are larger than 32bit
;;; for now we will first load our immediate to register
(defun emit-constant-ir (component node)
  (let ((location (make-temp-location)))
    (make-load-ir component location (make-immediate-constant :constant (immediate-constant-node-value node)))
    location))

;;; FIXME, this is not right
;;; quick solution: transform this to runtime list
(defun emit-quoted-form-ir (component node)
  (let ((location (make-temp-location)))
    (make-quoted-ir component location (quoted-node-form node))
    location))

(defun emit-lexical-var-node (node environments)
  (get-var-ir-symbol node environments))

(defun emit-progn-ir (component node environments)
  (let ((nodes (progn-node-forms node))
	(location (make-temp-location))
	(last-location))
    (dolist (node nodes)
      (setf last-location (emit-node-ir component node environments)))
    (make-load-ir component location last-location)
    location))

(defun emit-let-binding (component binding environments)
  (let* ((r-form (lexical-var-node-form binding))
	(load-from (emit-node-ir component r-form environments)))
    (add-var binding environments)
    (make-load-ir component (get-var-ir-symbol binding environments) load-from)))

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
	(location (make-return-location)))
    (let ((arg-locations))
      (dolist (argument arguments)
	(push (emit-node-ir component argument environments) arg-locations))
      (make-fun-params-ir component (reverse arg-locations)))
    (make-call-ir component location fun)
    location))

(defun emit-setq-ir (component node environments)
  (let ((location (emit-node-ir component (setq-node-form node) environments)))
    (make-load-ir component (get-var-ir-symbol (setq-node-var node) environments) location)
    (get-var-ir-symbol (setq-node-var node) environments)))

(defun emit-label-node-ir (component node environments)
  (make-label-ir component (get-label-ir-symbol node environments)))

(defun emit-tagbody-ir (component node environments)
  (add-tagbody-env (make-tagbody-env) environments)
  ;; first get all labels
  ;; FIXME, do this when creating nodes
  (dolist (tnode (tagbody-node-forms node))
    (when (eq 'label-node (type-of tnode))
      (add-go-label tnode environments)))
  (dolist (tnode (tagbody-node-forms node))
    (typecase tnode
      (label-node (emit-label-node-ir component tnode environments))
      (go-node (emit-go-ir component tnode environments))
      (t (emit-node-ir component tnode environments))))
  (remove-tagbody-env environments)
  ;; tagbody returns nil
  nil)

(defun emit-go-ir (component node environments)
  (make-go-ir component (get-label-ir-symbol (go-node-label-node node) environments))
  nil)

;;; lambda will create new component
(defun emit-lambda-ir (component node)
  (let ((lambda-comp (make-ir-component))
	(temp-loc (make-rip-relative-location :location (make-temp-location-symbol))))
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
    (go-node (emit-go-ir component node environments))
    (otherwise (error "Unknown node type"))))

;;;;; MISSING
;;; block
;;; return / return-from

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

(defstruct ir-block num ir from-blocks to-blocks used-locations)

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
	(previous-was-jump nil)
	(block-num 0))
    (dolist (inst ir)
      (if (or previous-was-jump (new-blockp inst counter))
	  (progn
	    (when block
	      (push (make-ir-block :num block-num :ir (reverse block)) all)
	      (incf block-num))
	    (setf block nil)
	    (push inst block))
	  (progn
	    (push inst block)))
      (incf counter)
      (setf previous-was-jump (jump-ir-p inst)))
    (push (make-ir-block :num block-num :ir (reverse block)) all)
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
  ;; FIXME, setting from is bad
  (dotimes (index (length blocks))
    (let ((to-block-indexes (ir-block-to-blocks (nth index blocks))))
      (dolist (to-index to-block-indexes)
	(push index (ir-block-from-blocks (nth to-index blocks))))))
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

(defun get-used-locations (ir-block)
  (let (locations)
    (dolist (ir (ir-block-ir ir-block))
      (let ((loc (get-ir-used-location ir)))
	(dolist (l loc)
	  (when (or (typep l 'tmp-location)
		    (typep l 'var-location)
		    (typep l 'ret-location))
	   (push l locations)))))
    (setf (ir-block-used-locations ir-block) locations)))

(defun fill-used-locations (blocks)
  (dolist (ir-block blocks)
    (setf (ir-block-used-locations ir-block)
	  (get-used-locations ir-block)))
  blocks)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; horrible way to remove redundant temporary store/read operations
;;; search for temp load/stores and remove them

(defun get-load-to (ir)
  (second ir))

(defun get-load-from (ir)
  (third ir))

(defun load-ir-p (ir)
  (or (eq (first ir) 'load)
      (eq (first ir) 'load-param)))

(defun block-use-location (block location)
  (dolist (loc (ir-block-used-locations block))
    (when (eq (location-symbol location)
	      (location-symbol loc))
      (return-from block-use-location t))))

(defun other-block-use-location (block blocks location)
  (let ((block-num (ir-block-num block)))
    (dolist (ir-block blocks)
      (when (and (/= block-num (ir-block-num ir-block))
		 (block-use-location ir-block location))
	(return-from other-block-use-location t)))))

(defun real-tmp-loc-p (location reads writes block blocks)
  (and (typep location 'tmp-location)
       (not (other-block-use-location block blocks location))
       (= 1
	  (or (gethash (location-symbol location) reads) -1)
	  (or (gethash (location-symbol location) writes) -1))))

(defstruct load-pattern start current end last-temp)

(defun find-pattern (patterns location)
  (let (pattern)
    (dolist (p patterns)
      (when (and (load-pattern-current p)
		 (eq (location-symbol location) (location-symbol (load-pattern-current p))))
	(setf pattern p)))
    pattern))

(defun find-pattern-by-start (patterns location)
  (let (pattern)
    (dolist (p patterns)
      (when (and (load-pattern-start p)
		 (eq (location-symbol location) (location-symbol (load-pattern-start p))))
	(setf pattern p)))
    pattern))

(defun find-pattern-by-last-temp (patterns location)
  (let (pattern)
    (dolist (p patterns)
      (when (and (load-pattern-last-temp p)
		 (eq (location-symbol location) (location-symbol (load-pattern-last-temp p))))
	(setf pattern p)))
    pattern))

(defun start-pattern (location)
  (make-load-pattern :start location :current location :end nil))

(defun add-pattern-location (to from res)
  (if (null res)
      (error "Can't add location connection, no existing pattenrs")
      (let ((pattern (find-pattern res from)))
	(if (null pattern)
	    (error "Can't find right pattern")
	    (setf (load-pattern-current pattern) to)))))

(defun finish-pattern (to from res)
  (if (null res)
      (error "Can't finish location pattern, no existing pattenrs")
      (let ((pattern (find-pattern res from)))
	(if (null pattern)
	    (error "Can't find right pattern")
	    (progn
	      (setf (load-pattern-current pattern) nil)
	      (setf (load-pattern-end pattern) to)
	      (setf (load-pattern-last-temp pattern) from))))))

(defun find-redundant-load-patterns (block blocks reads writes)
  (let ((res nil))
    (dolist (ir (ir-block-ir block))
      (when (load-ir-p ir)
	(let* ((to (get-load-to ir))
	       (from (get-load-from ir))
	       (tmp-to (real-tmp-loc-p to reads writes block blocks))
	       (tmp-from (real-tmp-loc-p from reads writes block blocks)))
	  (cond ((and tmp-to tmp-from)
		 (add-pattern-location to from res))
		((and tmp-to (not tmp-from))
		 (setf res (append res (list (start-pattern to)))))
		((and (not tmp-to) tmp-from)
		 (finish-pattern to from res))))))
    res))

(defun get-locations-usage-count (ir-code)
  (let ((reads (make-hash-table))
	(writes (make-hash-table)))
    (dolist (ir ir-code)
      (when (load-ir-p ir)
	(let ((to (get-load-to ir))
	      (from (get-load-from ir)))
	  (when (typep to 'tmp-location)
	    (let ((r (gethash (location-symbol to) reads)))
	      (if r
		  (setf (gethash (location-symbol to) reads) (+ 1 r))
		  (setf (gethash (location-symbol to) reads) 1))))
	  (when (typep from 'tmp-location)
	    (let ((w (gethash (location-symbol from) writes)))
	      (if w
		  (setf (gethash (location-symbol from) writes) (+ 1 w))
		  (setf (gethash (location-symbol from) writes) 1)))))))
    (list reads writes)))


(defun get-forward-location (patterns location)
  (let ((pattern (find-pattern-by-start patterns location)))
    (if (null pattern)
	(error "Missing pattern")
	(let ((forward-location (load-pattern-end pattern)))
	  (setf (load-pattern-end pattern) nil) ; this is needed so we can skip last move
	  forward-location))))

(defun remove-redundant-load-patterns (block blocks)
  (let* ((usage-count (get-locations-usage-count (ir-block-ir block)))
	 (reads (first usage-count))
	 (writes (second usage-count)))
    (let ((patterns (find-redundant-load-patterns block blocks reads writes))
	  (new-code nil))
      (dolist (ir (ir-block-ir block))
	(if (load-ir-p ir)
	    (let* ((to (get-load-to ir))
		   (from (get-load-from ir))
		   (tmp-to (real-tmp-loc-p to reads writes block blocks))
		   (tmp-from (real-tmp-loc-p from reads writes block blocks)))
	      (cond ((and (not tmp-to) (not tmp-from))
		     (push ir new-code))
		    ((and tmp-to (not tmp-from))
		     (push (list (first ir)
				 (get-forward-location patterns to)
				 from)
			   new-code))
		    ((and (not tmp-to) tmp-from)
		     ;; FIXME, maybe we should insert here our first move location
		     ;; so we don't screw up instruction order
		     (unless (find-pattern-by-last-temp patterns from)
		       (push ir new-code)))))
	    (push ir new-code)))
      (reverse new-code))))
