(defpackage :check-bnf.spec
  (:use :cl :jingoh :check-bnf))
(in-package :check-bnf.spec)
(setup :check-bnf)

(requirements-about CHECK-BNF :doc-type function)

;;;; Description:
; Macro argument checker like CL:CHECK-TYPE.

; In most simple case, its like CL:CHECK-TYPE.
#?(let((var 'symbol))
    (check-bnf()
      ((var symbol))))
=> NIL
#?(let((var "string"))
    (check-bnf()
      ((var symbol))))
:signals syntax-error

; You can check some place at once.
#?(let((a 'symbol)
       (b "string"))
    (check-bnf()
      ((a symbol))
      ((b string))))
=> NIL

; When type-specifier is expanded to T,
; Efficient code is generated.
#?(check-bnf()
    ((a t)))
:expanded-to
(let((check-bnf::*whole* nil))
  (let((check-bnf::*bnf* '((a t))))
    (labels((a(a)
	      (declare(ignore a))
	      nil))
      (a a))))

#?(check-bnf()
    ((a* t)))
:expanded-to
(let((check-bnf::*whole* nil))
  (let((check-bnf::*bnf* '((a* t))))
    (labels((a*(a*)
	      (if (typep a* '(and atom (not null)))
		(syntax-error 'a* "Require LIST but ~S." a*)
		nil)))
      (a* a*))))

; Also it occur in OR form.
#?(check-bnf()
    ((a (or symbol t string))))
:expanded-to
(let((check-bnf::*whole* nil))
  (let((check-bnf::*bnf* '((a (or symbol t string)))))
    (labels((a(a)
	      (declare(ignore a))
	      nil))
      (a a))))

#?(check-bnf()
    ((a (or symbol b string))
     (b t)))
:expanded-to
(let((check-bnf::*whole* nil))
  (let((check-bnf::*bnf* '((a (or symbol b string))
			   (b t))))
    (labels((a(a)
	      (declare(ignore a))
	      nil)
	    (b(b)
	      (declare(ignore b))
	      nil))
      (a a))))

#?(check-bnf()
    ((a (or b c))
     (b integer)
     (c t)))
:expanded-to
(let((check-bnf::*whole* nil))
  (let((check-bnf::*bnf* '((a (or b c))
			   (b integer)
			   (c t))))
    (labels((a(a)
	      (declare(ignore a))
	      nil)
	    (b(b)
	      (unless(typep b 'integer)
		(syntax-error 'b "but ~S, it is type-of ~S"
			      b (type-of b))))
	    (c(c)
	      (declare(ignore c))
	      nil))
      (a a))))

; When you know VAR is list, and it has 0 or more elt. (a.k.a. *)
; You can write like below.
#?(let((var* nil))
    (check-bnf()
      ((var* symbol))))
=> NIL

#?(let((var* '(symbol)))
    (check-bnf()
      ((var* symbol))))
=> NIL

#?(let((var* '("string")))
    (check-bnf()
      ((var* symbol))))
:signals syntax-error

#?(let((var* :not-list))
    (check-bnf()
      ((var* symbol))))
:signals syntax-error

; When expected T, efficient code is generated.
#?(let((option* ()))
    (check-bnf:check-bnf()
      ((option* keyword T))))
=> NIL

; If you do not like names var as XXX*, you can specify alias.
#?(let((vars '(symbol)))
    (check-bnf()
      (((var* vars)symbol))))
=> NIL

; e.g. specify for plist.
#?(let((var* '(:key "value" :key2 "value2")))
    (check-bnf()
      ((var* keyword string))))
=> NIL

#?(let((var* '(:key 2 :key2 "not integer")))
    (check-bnf()
      ((var* keyword integer))))
:signals syntax-error

#?(let((var* '(:key 1 "not-key" 2)))
    (check-bnf()
      ((var* keyword integer))))
:signals syntax-error

#?(let((var* '(not "ballanced" plist)))
    (check-bnf()
      ((var* keyword string))))
:signals syntax-error

; e.g. specify for alist.
#?(let((var* '((:key "value")(:key2 "value2"))))
    (check-bnf()
      ((var* (keyword string)))))
=> NIL

#?(let((var* '((:key "value")(:key2 :not-string))))
    (check-bnf()
      ((var* (keyword string)))))
:signals syntax-error

#?(let((var* '((:key "value")(:not "ballanced" clause))))
    (check-bnf()
      ((var* (keyword string)))))
:signals syntax-error

#?(let((var* '((:key "value")(:not-ballanced))))
    (check-bnf()
      ((var* (keyword string)))))
:signals syntax-error

; of course dotted are valid.
#?(let((var* '((:key . "value"))))
    (check-bnf()
      ((var* (keyword . string)))))
=> NIL

; When you know VAR is list, and it has 1 or more elt, (a.k.a. +)
; you can write like below.
#?(let((var+ '(1)))
    (check-bnf()
      ((var+ integer))))
=> NIL

#?(let((var+ '()))
    (check-bnf()
      ((var+ integer))))
:signals syntax-error

#?(let((var+ '("not-integer")))
    (check-bnf()
      ((var+ integer))))
:signals syntax-error

#?(let((var+ :not-cons))
    (check-bnf()
      ((var+ integer))))
:signals syntax-error

#+syntax
(CHECK-BNF (&key ((:whole whole?))) &rest def+) ; => result

;;;; Arguments and Values:

; whole := form, evaluated.
#?(check-bnf(:whole no-such-var)
    ((dummy dummy)))
:signals (or error
	     warning ; for ccl
	     )

; Expects var for &WHOLE.
; When specified, header and footer is generated in error message.
#?(let((a "not-symbol"))
    (check-bnf()
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda(condition)
	 (& (equal #.(format nil "A := SYMBOL~%~
			     but \"not-symbol\", it is type-of ~S"
			     (type-of "not-symbol"))
		   (princ-to-string condition))))

#?(let((a "not-symbol"))
    (check-bnf(:whole '(whole ("not-symbol")))
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda(condition)
	 (& (equal #.(format nil "Syntax-error in WHOLE~%~
			     A := SYMBOL~%~
			     but \"not-symbol\", it is type-of ~S~%~
			     in ~S"
			     (type-of "not-symbol")
			     '(whole ("not-symbol")))
		   (princ-to-string condition))))

; def := (clause+)+
; clause := (var-spec spec+)
; var-spec := [ name | (name name) ]
; name := SYMBOL, otherwise error.
#?(check-bnf()
    (("not-symbol" dummy)))
:signals syntax-error
,:lazy t

; Not evaluated.
#?(check-bnf()
    (((intern "not evaluated") dummy)))
:signals syntax-error
,:lazy t

; Clause require one or more, otherwise syntax error.
#?(check-bnf())
:signals syntax-error
,:lazy t

; spec := [ type-specifier | bnf ]
; bnf := [ name | list-bnf | or-form ]
; list-bnf := (spec+)
; or-form := (or spec+)

; Spec require one or more, otherwise error.
#?(check-bnf()
    ((dummy)))
:signals syntax-error
,:lazy t

#?(check-bnf()
    ((dummy+ nil)))
:signals syntax-error
,:lazy t

; result := NULL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:
; To check optional argument (e.g. &OPTIONAL or &KEY), you can write like below.
#?(let((option))
    (check-bnf()
      ((option (or null symbol)))))
=> NIL

#?(let((option 'symbol))
    (check-bnf()
      ((option (or null symbol)))))
=> NIL

#?(let((option "not-symbol"))
    (check-bnf()
      ((option (or null symbol)))))
:signals syntax-error

#?(let((function-name "not function-name"))
    (check-bnf()
      ((function-name (or name setf-name))
       (name symbol)
       (setf-name ((eql setf)name)))))
:signals syntax-error

; To check optional value in list, you can write like below.
#?(let((args '(option and others)))
    (check-bnf()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
=> NIL

#?(let((args '(and others)))
    (check-bnf()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
=> NIL

#?(let((args '("not option nor other*" and others)))
    (check-bnf()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
:signals syntax-error

;;;; Exceptional-Situations:
; Every NAME should not conflicts CL symbol.
#?(let((list nil))
    (check-bnf()
      ((list list))))
=> unspecified

;;;; Example.
#?(let((var '(symbol symbol)))
    (check-bnf()
      ((var (symbol symbol option*))
       (option* keyword string))))
=> NIL

#?(let((var '(symbol symbol)))
    (check-bnf()
      ((var (symbol symbol option?))
       (option? keyword))))
=> NIL

#?(let((var '(symbol symbol)))
    (check-bnf()
      ((var (symbol symbol required))
       (required keyword))))
:signals syntax-error

;; Right side XXX*
#?(let((var '(symbol)))
    (check-bnf()
      ((var (or string name*))
       (name symbol))))
=> NIL

#?(let((var "string"))
    (check-bnf()
      ((var (or string name*))
       (name symbol))))
=> NIL

#?(let((var ()))
    (check-bnf()
      ((var (or string name*))
       (name symbol))))
=> NIL

#?(let((var :not-list))
    (check-bnf()
      ((var (or string name*))
       (name symbol))))
:signals syntax-error

#?(let((var '("string" "list")))
    (check-bnf()
      ((var (or string name*))
       (name symbol))))
:signals syntax-error

;; Right side XXX?
#?(let((ll nil))
    (check-bnf()
      ((ll (var?))
       (var symbol))))
=> NIL

#?(let((ll '(var)))
    (check-bnf()
      ((ll (var?))
       (var symbol))))
=> NIL

#?(let((ll '(var too much)))
    (check-bnf()
      ((ll (var?))
       (var symbol))))
:signals syntax-error

#?(let((ll "not symbol"))
    (check-bnf()
      ((ll (var?))
       (var symbol))))
:signals syntax-error

(requirements-about EXPRESSION :doc-type type)
;;;; Description:
;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

