(in-package #:clcomp)
(declaim (optimize (debug 3) (safety 3) (speed 0)))

(defparameter *temp-counter* 0)
(defparameter *label-counter* 0)

(defstruct lambda-info lambda-list-rest)
(defstruct ir-component code code-blocks sub-comps rips lambda-info eval-load-compile)

(defun get-lambda-info (lambda-node)
  (let ((lambda-info (make-lambda-info)))
    (setf (lambda-info-lambda-list-rest lambda-info)
	  (some #'lexical-var-node-rest
		(lambda-node-arguments lambda-node)))
    lambda-info))

(defun maybe-create-new-ir-component (component lambda-node)
  (or component
      (let ((comp (make-ir-component)))
	(setf (ir-component-lambda-info comp)
	      (get-lambda-info lambda-node))
	comp)))

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

(defstruct rip-relative-location location)
(defstruct var-location location)
(defstruct tmp-location location)
(defstruct ret-location location)
(defstruct param-location location param-number arguments-count)
(defstruct lambda-return-location location)
(defstruct immediate-constant constant)

(defun make-return-location ()
  (make-ret-location :location (make-temp-location-symbol)))

(defun create-lambda-return-location ()
  (make-lambda-return-location  :location (make-temp-location-symbol)))


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

(defun register-location-p (location)
  (and (member (type-of location) '(tmp-location var-location ret-location))
       t))

;;; environments
;;; lexical vars environments

(defun make-env-holder ()
  (list 'envs nil nil nil))

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


;;; declarations environment
(defun add-declarations-env (declarations environments)
  (push declarations (fourth environments)))

(defun remove-declarations-env (environments)
  (pop (fourth environments)))

;; for now if we want to find vop we need to declare it inline
(defun inline-fun-p (fun environments)
  (dolist (decls (fourth environments))
    (let ((inline (cdr (assoc 'inline decls)))
	  (notinline (cdr (assoc 'notinline decls))))
      (cond ((member fun inline) (return-from inline-fun-p t))
	    ((member fun notinline) (return-from inline-fun-p nil)))))
  nil)

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
(defstruct fixup-rip-relative name)
(defstruct fixup-rip-relative-constant name form)
(defstruct sub-component name component eval)

(defun get-rip-relative-name (rip-relative-location)
  (etypecase rip-relative-location
    (fun-rip-relative (fun-rip-relative-name rip-relative-location))
    (component-rip-relative (component-rip-relative-name rip-relative-location))
    (fixup-rip-relative (fixup-rip-relative-name rip-relative-location))
    (fixup-rip-relative-constant (fixup-rip-relative-constant-name rip-relative-location))))


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

(defun add-fun-rip-relative-location (fun ir-component)
  (unless
      (find fun (ir-component-rips ir-component)
	    :test (lambda (f rip)
		    (and (fun-rip-relative-p rip)
			 (eq f (fun-rip-relative-name rip)))))
    (push (make-fun-rip-relative :name fun) (ir-component-rips ir-component))))

(defun make-call-ir (component location fun arguments-count)
  (add-fun-rip-relative-location fun component)
  (add-ir component (list (list 'load-call location 'call fun arguments-count))))

(defun make-vop-ir (component ret-location arg-locations vop-name)
  (add-ir component (list (list 'vop vop-name ret-location arg-locations))))

(defun make-return-ir (component location)
  (add-ir component (list (list 'load (create-lambda-return-location) location))))

(defun make-lambda-exit-ir (component)
  (add-ir component (list (list 'lambda-exit))))

(defun get-minimum-number-of-args (args)
  (let ((i 0))
    (dolist (arg args)
      (unless (lexical-var-node-rest arg)
	(incf i)))
    i))

(defun contains-rest-arg (args)
  (dolist (arg args)
    (when (lexical-var-node-rest arg)
      (return-from contains-rest-arg (get-minimum-number-of-args args)))))

(defun make-lambda-entry-ir (component arguments)
  (add-ir component (list (list 'lambda-entry)))
  (add-ir component (list (if (contains-rest-arg arguments)
			      (list 'arg-minimum-check (get-minimum-number-of-args arguments))
			      (list 'arg-check (get-minimum-number-of-args arguments)))))
  (let ((min-args (contains-rest-arg arguments)))
    (when min-args
      (add-ir component (list (list 'listify-args min-args))))))

(defun make-lambda-arguments-ir (component arguments environments)
  (let ((index 1)
	(number-of-arguments (length arguments)))
    (dolist (arg arguments)
      (add-var arg environments)
      (add-ir component (list (list 'receive-param (get-var-ir-symbol arg environments) index number-of-arguments)))
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
	  ((eq mnem 'vop)
	   (cons (third ir) (fourth ir)))
	  ((eq mnem 'param)
	   (list (second ir)))
	  ((eq mnem 'return)
	   (list (second ir)))
	  (t nil))))

(defun get-ir-vop-name (ir)
  (second ir))

(defun get-ir-vop-return-loc (ir)
  (third ir))

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
  (let ((ret-location (make-temp-location))
	(arguments (call-node-arguments node)))
    (let ((arg-locations))
      (dolist (arg arguments)
	(push (emit-node-ir component arg environments ) arg-locations))
      (make-vop-ir component ret-location (reverse arg-locations) (call-node-function node))
      ret-location)))

;; FIXME, for now we just check does number of arguments match with vop arguments count
;; not sure about this
(defun does-vop-match (node vop)
  (= (length (call-node-arguments node)) 
     (length (vop-arguments vop))))

(defun emit-call-or-vop (component node environments)
  (let ((fun (call-node-function node)))
    (if (and (get-vop fun) (inline-fun-p fun environments) (not *dont-inline*) (does-vop-match node (get-vop fun)))
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

(defun emit-lexical-var-node (node environments)
  (get-var-ir-symbol node environments))

(defun emit-load-time-component (component ref-constant-node)
  (let ((temp-loc (make-rip-relative-location :location (make-temp-location-symbol)))
	(subcomp (make-ir-component :eval-load-compile t)))
    (push (make-sub-component :name temp-loc :component subcomp :eval t)
	  (ir-component-sub-comps component))
    (push (make-fixup-rip-relative :name (rip-relative-location-location temp-loc)) (ir-component-rips component))
    (make-ir (ref-constant-node-node ref-constant-node) subcomp temp-loc)))

(defun emit-compile-time-constant-ir (component node environments)
  (declare (ignore environments ))
  (let ((location (make-temp-location))
	(temp-loc (make-rip-relative-location :location (make-temp-location-symbol))))
    (push (make-fixup-rip-relative-constant :name (rip-relative-location-location temp-loc)
					    :form (compile-time-constant-node-form node))
	  (ir-component-rips component))
    (make-load-ir component location temp-loc)
    location))

(defun emit-go-ir (component node environments)
  (make-go-ir component (get-label-ir-symbol (go-node-label-node node) environments))
  nil)

;;; lambda will create new component
(defun emit-lambda-ir (component node)
  (let ((lambda-comp (make-ir-component))
	(temp-loc (make-rip-relative-location :location (make-temp-location-symbol))))
    (setf (ir-component-lambda-info lambda-comp)
	  (get-lambda-info node))
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
    (immediate-constant-node (emit-constant-ir component node))
    (setq-node (emit-setq-ir component node environments))
    (tagbody-node (emit-tagbody-ir component node environments))
    ;; FIXME, error -> go-node returns nil now, should not return anything
    ;; anyway, dead code elimination in block analyzing solves this
    (go-node (emit-go-ir component node environments))
    (ref-constant-node (emit-load-time-component component node))
    (compile-time-constant-node (emit-compile-time-constant-ir component node environments))
      ;; (symbol-raw-node (emit-raw-symbol-component component node))
    (otherwise (error "Unknown node type"))))

;;; entry node need to be lambda
(defun make-ir (lambda-node &optional component name)
  (let ((comp (maybe-create-new-ir-component component lambda-node)))
    (let ((arguments (lambda-node-arguments lambda-node))
	  (body (lambda-node-body lambda-node))
	  (environments (make-env-holder))
	  (env (make-env))
	  (declarations (lambda-node-declarations lambda-node)))
      (add-env env environments)
      (add-declarations-env declarations environments)
      (make-lambda-entry-ir comp arguments)
      (make-lambda-arguments-ir comp arguments environments)
      (make-return-ir comp (emit-node-ir comp body environments))
      (make-lambda-exit-ir comp)
      (remove-env environments)
      (remove-declarations-env environments))
    (or name comp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; IR blocks
(defstruct ir-block num ir from-blocks to-blocks used-locations use def in out)

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
(defun any-of-blocks-exist (blocks-indexes block-map)
  (dolist (block-index blocks-indexes)
    (when
	(or 
	 (zerop block-index)
	 (gethash block-index block-map))
      (return-from any-of-blocks-exist t))))

(defun remove-dead-blocks-2 (blocks block-map)
  "Second run, we already removed fake blocks, now remove those connected only to them"
  ;; FIXME
  ;; not great solution but works for now
  (let ((live-blocks nil))
    (dolist (block blocks)
      (if (or (zerop (ir-block-num block))
		(let ((from-blocks-indexes (ir-block-from-blocks block)))
		  (and from-blocks-indexes
		       (any-of-blocks-exist from-blocks-indexes block-map))))
	  (push block live-blocks)
	  (remhash (ir-block-num block) block-map)))
    (reverse live-blocks)))

(defun remove-references-from-dead-blocks (blocks block-map)
  (dolist (block blocks)
    (let ((from (ir-block-from-blocks block))
	  (new-from nil))
      (dolist (i from)
	(when (gethash i block-map)
	  (push i new-from)))
      (setf (ir-block-from-blocks block) new-from)))
  blocks)

(defun remove-dead-blocks (blocks)
  (let ((live-blocks nil)
	(block-map (make-hash-table)))
    (dotimes (index (length blocks))
      (let ((block (nth index blocks)))
	(when (or (zerop index)
		  (ir-block-from-blocks block))
	  (push block live-blocks)
	  (setf (gethash index block-map) block))))
    (let ((live-blocks (remove-dead-blocks-2 (reverse live-blocks) block-map)))
      (remove-references-from-dead-blocks live-blocks block-map))))

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
      (eq (first ir) 'load-call)
      (eq (first ir) 'vop)))

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


;;;;;;;;;;;;;;;;;

(defun check-location (location)
  (if (register-location-p location)
      (list location)
      nil))

(defun check-locations (locations)
  (let ((goodl (remove-if-not #'check-location locations)))
    (if (plusp (length goodl))
	goodl
	nil)))

(defun get-ir-locations (ir)
  "Returns list of list of defined locations and list of used locations"
  (let ((mnemonic (first ir)))
    (case mnemonic
      (receive-param (list nil (check-location (second ir))))
      (load-param (list nil (check-location (get-load-from ir))))
      (load (list (check-location (get-load-to ir))
		  (check-location (get-load-from ir))))
      (load-call (list (check-location (second ir)) nil))
      (if (list nil (check-location (second ir))))
      (vop (list (check-location (third ir))
		 (check-locations (get-ir-vop-args ir))))
      (otherwise nil))))

(defun make-block-by-index-map (blocks)
  (let ((map (make-hash-table)))
    (dolist (block blocks)
      (setf (gethash (ir-block-num block) map) block))
    map))
