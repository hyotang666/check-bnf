(in-package :cl-user)
(defpackage :check-bnf
  (:use :cl)
  (:export))
(in-package :check-bnf)

(define-condition syntax-error(program-error simple-error cell-error)
  ()
  (:report (lambda(condition stream)
	     (format stream
		     "Syntax-error in ~S~%~?"
		     (let((name(cell-error-name condition)))
		       (cons name (millet:lambda-list name)))
		     (simple-condition-format-control condition)
		     (simple-condition-format-arguments condition)))))

(defun syntax-error(name format-control &rest format-arguments)
  (error 'syntax-error
	 :name name
	 :format-control format-control
	 :format-arguments format-arguments))
