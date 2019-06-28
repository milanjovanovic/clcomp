(in-package #:clcomp)
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct ir-component code code-blocks sub-comps rips)

(defparameter *specials* nil)

(defun special? (symbol)
  (member symbol *specials*))

(defun reset-temp-location-counter ()
  (setf *temp-counter* 0))

(defun make-temp-location-symbol ()
  (incf *temp-counter*)
  (make-symbol (concatenate 'string "TEMP-" (write-to-string *temp-counter*))))

(defun make-temp-location ()
  (make-tmp-location :location (make-temp-location-symbol)))

(defun make-label ()
  (incf *label-counter*)
  (make-symbol (concatenate 'string "LABEL-" (write-to-string *label-counter*))))

(defun make-return-location ()
  (make-ret-location :location (make-temp-location-symbol)))

(defstruct rip-relative-location location)
(defstruct var-location location)
(defstruct tmp-location location)
(defstruct ret-location location)
(defstruct param-location location param-number arguments-count)
(defstruct immediate-constant constant)

(defun location-equal (loc1 loc2)
  (and (eq (type-of loc1)
	   (type-of loc2))
       (let ((type (type-of loc1)))
	 (case type
	   (param-location (eq (param-location-param-number loc1)
			       (param-location-param-number loc2)))
	   (tmp-location (eq (tmp-location-location loc1)
			     (tmp-location-location loc2)))
	   (var-location (eq (var-location-location loc1)
			     (var-location-location loc2)))
	   (ret-location (eq (ret-location-location loc1)
			     (ret-location-location loc2)))
	   (rip-relative-location (eq (rip-relative-location-location loc1)
				      (rip-relative-location-location loc2)))
	   (immediate-constant (equalp (immediate-constant-constant loc1)
				       (immediate-constant-constant loc2)))
	   (otherwise (error "Unknown location type"))))))

(defun location-symbol (location)
  (typecase location
    (tmp-location (tmp-location-location location))
    (var-location (var-location-location location))
    (ret-location (ret-location-location location))
    (param-location (param-location-location location))
    (otherwise (error "Unknown location type"))))

;;; environments
;;; lexical vars environments

(defun make-env-holder ()
  (list 'envs nil nil))

(defun make-env ()
  (make-hash-table))

(defun add-env (env env-holder)
  (push env (second env-holder))
  env-holder)

(defun remove-env (all-env)
  (pop (second all-env)))

(defun add-var (var-node environments)
  (let* ((lexical-var-name (lexical-var-node-name var-node))
	 (new-lex-var-symbol (make-symbol (symbol-name lexical-var-name)))
	 (env-hash (first (second environments))))
    (setf (gethash lexical-var-name env-hash) (make-var-location :location new-lex-var-symbol))))

(defun get-var-ir-symbol (var-node env-holder)
  (dolist (env-map (second env-holder))
    (let ((var-sym (gethash (lexical-var-node-name var-node) env-map)))
      (when var-sym
	(return-from get-var-ir-symbol var-sym))))
  ;; FIXME, unboud vars threat as special vars
  (error (concatenate 'string "Missing lexical variable " (symbol-name (lexical-var-node-name var-node)))))

  ;;; go tags environment
(defun make-tagbody-env ()
  (make-hash-table))

(defun add-tagbody-env (tagb-env environments)
  (push tagb-env (third environments))
  environments)

(defun remove-tagbody-env (environments)
  (pop (third environments)))

(defun add-go-label (label-node environments)
  (let* ((label (label-node-label label-node))
	 (env-hash (first (third environments)))
	 (new-label-symbol (make-symbol (symbol-name label))))
    (setf (gethash label env-hash) new-label-symbol)))

(defun get-label-ir-symbol (label-node environments)
  (dolist (env-hash (third environments))
    (let ((label-sym (gethash (label-node-label label-node) env-hash)))
      (when label-sym
	(return-from  get-label-ir-symbol label-sym))))
  (error (concatenate 'string "Go tag " (symbol-name (label-node-label label-node)) " does not exist ")))


;;; IR code generators
;;; FIXME, use structures

(defstruct fun-rip-relative name)
(defstruct component-rip-relative name)
(defstruct sub-component name component)

(defun get-rip-name (rip-relative-location)
  (etypecase rip-relative-location
    (fun-rip-relative (fun-rip-relative-name rip-relative-location))
    (component-rip-relative (component-rip-relative-location rip-relative-location))))


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

(defun make-call-ir (component location fun arguments-count)
  (push (make-fun-rip-relative :name fun) (ir-component-rips component))
  (add-ir component (list (list 'load-call location 'call fun arguments-count))))

(defun make-vop-ir (component ret-location arg-locations vop-name)
  (add-ir component (list (list 'vop vop-name ret-location arg-locations))))

(defun make-return-ir (component location)
  (add-ir component (list (list 'load (make-return-location) location))))

(defun make-lambda-exit-ir (component)
  (add-ir component (list (list 'lambda-exit))))

(defun make-lambda-entry-ir (component number-of-arguments)
  (add-ir component (list (list 'lambda-entry) (list 'arg-check number-of-arguments))))

(defun make-lambda-arguments-ir (component arguments environments)
  (let ((index 1))
    (dolist (arg arguments)
      (add-var arg environments)
      (add-ir component (list (list 'receive-param (get-var-ir-symbol arg environments) index)))
      (incf index))))

(defun make-fun-params-ir (component arg-locations)
  (let ((arguments-count (length arg-locations)))
    (add-ir component (list (list 'params-count arguments-count)))
    (let ((counter 1))
      (dolist (arg-loc arg-locations)
	(add-ir component (list (list 'load-param
				      (make-param-location :location (make-temp-location-symbol)
							   :param-number counter
							   :arguments-count arguments-count)
				      arg-loc)))
	(incf counter)))))


(defun get-ir-used-location (ir)
  (let ((mnem (first ir)))
    (cond ((eq mnem 'if)
	   (list (second ir)))
	  ((eq mnem 'load)
	   (list (second ir) (third ir)))
	  ((eq mnem 'load-call)
	   (list (second ir)))
	  ((eq mnem 'param)
	   (list (second ir)))
	  ((eq mnem 'return)
	   (list (second ir)))
	  (t nil))))

(defun get-ir-vop-name (ir)
  (second ir))

(defun get-ir-vop-args (ir)
  (fourth ir))

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

(defun emit-vop-ir (component node environments)
  (let ((ret-location (make-return-location))
	(arguments (call-node-arguments node)))
    (let ((arg-locations))
      (dolist (arg arguments)
	(push (emit-node-ir component arg environments ) arg-locations))
      (make-vop-ir component ret-location (reverse arg-locations) (call-node-function node))
      ret-location)))

(defun emit-call-or-vop (component node environments)
  (let ((fun (call-node-function node)))
    (if (get-vop fun)
	(emit-vop-ir component node environments)
	(emit-call-ir component node environments))))

(defun emit-call-ir (component node environments)
  (let ((fun (call-node-function node))
	(arguments (call-node-arguments node))
	(location (make-return-location)))
    (let ((arg-locations))
      (dolist (argument arguments)
	(push (emit-node-ir component argument environments) arg-locations))
      (make-fun-params-ir component (reverse arg-locations)))
    (make-call-ir component location fun (length arguments))
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
  (make-immediate-constant :constant *nil*))

(defun emit-go-ir (component node environments)
  (make-go-ir component (get-label-ir-symbol (go-node-label-node node) environments))
  nil)

;;; lambda will create new component
(defun emit-lambda-ir (component node)
  (let ((lambda-comp (make-ir-component))
	(temp-loc (make-rip-relative-location :location (make-temp-location-symbol))))
    (push (make-sub-component :name temp-loc :component lambda-comp)
	  (ir-component-sub-comps component))
    (push (make-component-rip-relative :name (rip-relative-location-location temp-loc)) (ir-component-rips component))
    (make-ir node lambda-comp temp-loc)))

(defun emit-node-ir (component node environments)
  (typecase node
    (lexical-var-node (emit-lexical-var-node node environments))
    (progn-node (emit-progn-ir component node environments))
    (call-node (emit-call-or-vop component node environments))
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
      (make-lambda-exit-ir component)
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
      (eq (first ir) 'load-param)
      (eq (first ir) 'load-call)))

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

(defstruct load-pattern start start-from current end last-temp ir)

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

(defun start-pattern (to from)
  (make-load-pattern :start to :start-from from :current to :end nil))

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

;;; reset non-finished patterns if we see write to non-temp location from where we start creating pattern
(defun reset-broken-patterns (patterns location)
  (dolist (pattern patterns)
    (when (and (null (load-pattern-end pattern))
	       (load-pattern-start-from pattern)
	       (location-equal location (load-pattern-start-from pattern))
	       #+nil
	       (eq (location-symbol location)
		   (location-symbol (load-pattern-start-from pattern))))
      (setf (load-pattern-start pattern) nil)
      (setf (load-pattern-start-from pattern) nil)
      (setf (load-pattern-current pattern) nil))))

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
		 (setf res (append res (list (start-pattern to from)))))
		((and (not tmp-to) tmp-from)
		 (finish-pattern to from res))
		(t (reset-broken-patterns res to))))))
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


(defun save-replacement-ir (patterns location ir)
  (let ((pattern (find-pattern-by-start patterns location)))
    (if (null pattern)
	(error "Missing pattern")
	(let ((forward-location (load-pattern-end pattern)))
	  (setf (load-pattern-end pattern) nil)
	  (setf (load-pattern-ir pattern)
		(list (first ir) forward-location (third ir)))))))

(defun get-replacement-ir (patterns location)
  (let ((pattern (find-pattern-by-last-temp  patterns location)))
    (if (null pattern)
	(error "Can't find pattern !!")
	(let ((ir (load-pattern-ir pattern)))
	  (setf (load-pattern-last-temp pattern) nil)
	  ir))))

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
		     (save-replacement-ir patterns to ir))
		    ((and (not tmp-to) tmp-from)
		     (push (or (get-replacement-ir patterns from) ir)
			   new-code))))
	    (push ir new-code)))
      (reverse new-code))))
