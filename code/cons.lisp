(defun cons (car cdr)
  (cons car cdr))

(defun car (cons)
  (car cons))

(defun cdr (cons)
  (cdr cons))

(defun caar (cons)
  (car (car cons)))

(defun cadr (cons)
  (car (cdr cons)))

(defun caddr (cons)
  (car (cdr (cdr cons))))

(defun cadddr (cons)
  (car (cdr (cdr (cdr cons)))))


(defun first (list)
  (car list))

(defun second (list)
  (cadr list))

(defun third (list)
  (caddr list))

(defun fourth (list)
  (cadddr list))
