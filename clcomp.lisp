(defpackage #:clcomp
  (:use #:cl)
  (:export
   #:@
   #:*base-pointer-reg*
   #:*stack-pointer-reg*
   #:*instruction-pointer-reg*
   #:*fun-address-reg*
   #:*fun-number-of-arguments-reg*
   #:*fun-number-of-ret-values-reg*
   #:*fun-arguments-regs*
   #:*scratch-regs*
   #:*tmp-reg*
   #:*tmp-reg-2*
   #:*preserved-regs*
   #:*heap-header-reg*))
