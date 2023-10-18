(defpackage #:clcomp.translator
  (:use :cl :clcomp)
  (:export #:translate-to-compilation-unit))

(in-package #:clcomp.translator)

(defun translate-fixup (fixup)
  (etypecase fixup
    (clcomp.ssa::compile-function-fixup (clcomp::make-fun-rip-relative
					 :name (clcomp.ssa::compile-function-fixup-function fixup)))
    (clcomp.ssa::local-component-fixup (clcomp::make-component-rip-relative
					:name (clcomp.ssa::local-component-fixup-name fixup)))
    (clcomp.ssa::load-time-eval-fixup (clcomp::make-fixup-rip-relative
				       :name (clcomp.ssa::load-time-eval-fixup-name fixup)))))

(defun translate-subcomp (subcomp)
  (cons (clcomp::make-rip-relative-location :location (clcomp.ssa::named-place-name (car subcomp)))
	(translate-to-compile-component (cdr subcomp))))

(defun translate-to-compile-component (lambda-ssa)
  (clcomp::make-compile-component
   :code (clcomp.ssa::lambda-ssa-asm lambda-ssa)
   :rips (mapcar #'translate-fixup (clcomp.ssa::lambda-ssa-fixups lambda-ssa))
   :subcomps (mapcar #'translate-subcomp (clcomp.ssa::lambda-ssa-sub-lambdas lambda-ssa))))

(defun translate-to-compilation-unit (lambda-ssa)
  (clcomp::make-compilation-unit :compile-component
				 (translate-to-compile-component lambda-ssa)))
