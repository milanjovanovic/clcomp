(defun list (&rest rest)
  rest)

(defun null (thing)
  (null thing))

(defun consp (thing)
  (if (null thing)
      nil
      (listp thing)))

(defun cons (car cdr)
  (cons car cdr))

(defun car (cons)
  (if (listp cons)
      (car cons)
      (error "Arguments is not cons")))

(defun cdr (cons)
  (if (listp cons)
      (cdr cons)
      (error "Arguments is not cons")))

(defun caar (cons)
  (declare (notinline car))
  (car (car cons)))

(defun cadr (cons)
  (declare (notinline car cdr))
  (car (cdr cons)))

(defun caddr (cons)
  (declare (notinline car cdr))
  (car (cdr (cdr cons))))

(defun cadddr (cons)
  (declare (notinline car cdr))
  (car (cdr (cdr (cdr cons)))))

(defun rplaca (cons e)
  (if (listp cons)
      (rplaca cons e)
      (error "Argument is not cons")))

(defun rplacd (cons e)
  (if (listp cons)
      (rplacd cons e )
      (error "Argument is not cons")))

(defun listp (thing)
  (listp thing))

(defun first (list)
  (if (listp list)
      (car list)
      (error "Argument is not cons")))

(defun second (list)
  (cadr list))

(defun third (list)
  (caddr list))

(defun fourth (list)
  (cadddr list))
