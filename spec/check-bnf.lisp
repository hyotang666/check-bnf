(defpackage :check-bnf.spec
  (:import-from #:check-bnf #:pprint-check-bnf)
  (:use :cl :jingoh :check-bnf))
(in-package :check-bnf.spec)
(setup :check-bnf)

(requirements-about CHECK-BNF :doc-type function)

;;;; Description:
; Macro argument checker like CL:CHECK-TYPE.

; In most simple case, its like CL:CHECK-TYPE.
#?(let ((var 'symbol))
    (check-bnf ()
      ((var symbol))))
=> NIL
#?(let ((var "string"))
    (check-bnf ()
      ((var symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& (string= (princ-to-string condition)
                     (format nil "VAR := SYMBOL~2%~
                             but \"string\", it is type-of ~S"
                             (type-of "string")))))

; You can check some place at once.
#?(let ((a 'symbol)
        (b "string"))
    (check-bnf ()
      ((a symbol))
      ((b string))))
=> NIL

; When type-specifier is expanded to T,
; Efficient code is generated.
#?(check-bnf ()
    ((a t)))
:expanded-to
(let ((check-bnf::*whole* nil))
  (let ((check-bnf::*bnf* '((a t))))
    (labels ((a (a)
               (declare (ignore a))
               nil))
      (a a))))

#?(check-bnf ()
    ((a* t)))
:expanded-to
(let ((check-bnf::*whole* nil))
  (let ((check-bnf::*bnf* '((a* t))))
    (labels ((a* (a*)
              (if (typep a* '(and atom (not null)))
                  (syntax-error 'a* "Require LIST but ~S." a*)
                  nil)))
      (a* a*))))

; Also it occur in OR form.
#?(check-bnf ()
    ((a (or symbol t string))))
:expanded-to
(let ((check-bnf::*whole* nil))
  (let ((check-bnf::*bnf* '((a (or symbol t string)))))
    (labels ((a (a)
               (declare (ignore a))
               nil))
      (a a))))

#?(check-bnf ()
    ((a (or symbol b string))
     (b t)))
:expanded-to
(let ((check-bnf::*whole* nil))
  (let ((check-bnf::*bnf* '((a (or symbol b string))
                             (b t))))
    (labels ((a (a)
               (declare (ignore a))
               nil)
             (b (b)
               (declare (ignore b))
               nil))
      (a a))))

#?(check-bnf ()
    ((a (or b c))
     (b integer)
     (c t)))
:expanded-to
(let ((check-bnf::*whole* nil))
  (let ((check-bnf::*bnf* '((a (or b c))
                             (b integer)
                             (c t))))
    (labels ((a (a)
               (declare (ignore a))
               nil)
             (b (b)
               (unless (typep b 'integer)
                 (syntax-error 'b "but ~S, it is type-of ~S"
                                b (type-of b))))
             (c (c)
               (declare (ignore c))
               nil))
      (a a))))

; When you know VAR is list, and it has 0 or more elt. (a.k.a. *)
; You can write like below.
#?(let ((var* nil))
    (check-bnf ()
      ((var* symbol))))
=> NIL

#?(let ((var* '(symbol)))
    (check-bnf ()
      ((var* symbol))))
=> NIL

#?(let ((var* '("string")))
    (check-bnf ()
      ((var* symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& (string= (princ-to-string condition)
                     (format nil "VAR := SYMBOL~2%~
                             but \"string\", it is type-of ~S~%  in (\"string\")"
                             (type-of "string")))))

#?(let ((var* :not-list))
    (check-bnf ()
      ((var* symbol))))
:signals syntax-error

; When expected T, efficient code is generated.
#?(let ((option* ()))
    (check-bnf:check-bnf ()
      ((option* keyword T))))
=> NIL

; If you do not like names var as XXX*, you can specify alias.
#?(let ((vars '(symbol)))
    (check-bnf ()
      (((var* vars) symbol))))
=> NIL

; e.g. specify for plist.
#?(let ((var* '(:key "value" :key2 "value2")))
    (check-bnf ()
      ((var* keyword string))))
=> NIL

#?(let ((var* '(:key 2 :key2 "not integer")))
    (check-bnf ()
      ((var* keyword integer))))
:signals syntax-error

#?(let ((var* '(:key 1 "not-key" 2)))
    (check-bnf ()
      ((var* keyword integer))))
:signals syntax-error

#?(let ((var* '(not "ballanced" plist)))
    (check-bnf ()
      ((var* keyword string))))
:signals syntax-error

; e.g. specify for alist.
#?(let ((var* '((:key "value") (:key2 "value2"))))
    (check-bnf ()
      ((var* (keyword string)))))
=> nil

#?(let ((var* '((:key "value") (:key2 :not-string))))
    (check-bnf ()
      ((var* (keyword string)))))
:signals syntax-error

#?(let ((var* '((:key "value") (:not "ballanced" clause))))
    (check-bnf ()
      ((var* (keyword string)))))
:signals syntax-error

#?(let ((var* '((:key "value") (:not-ballanced))))
    (check-bnf ()
      ((var* (keyword string)))))
:signals syntax-error

; of course dotted are valid.
#?(let ((var* '((:key . "value"))))
    (check-bnf ()
      ((var* (keyword . string)))))
=> nil

; when you know var is list, and it has 1 or more elt, (a.k.a. +)
; you can write like below.
#?(let ((var+ '(1)))
    (check-bnf ()
      ((var+ integer))))
=> nil

#?(let ((var+ '()))
    (check-bnf ()
      ((var+ integer))))
:signals syntax-error

#?(let ((var+ '("not-integer")))
    (check-bnf ()
      ((var+ integer))))
:signals syntax-error

#?(let ((var+ :not-cons))
    (check-bnf ()
      ((var+ integer))))
:signals syntax-error

#+syntax
(check-bnf (&key ((:whole whole?))) &rest def+) ; => result

;;;; arguments and values:

; whole := form, evaluated.
#?(check-bnf (:whole no-such-var)
    ((dummy dummy)))
:signals (or error
             warning ; for ccl
             )

; expects var for &whole.
; when specified, header and footer is generated in error message.
#-clisp
#?(let ((a "not-symbol"))
    (check-bnf ()
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& (equal #.(format nil "A := SYMBOL~2%~
                             but \"not-symbol\", it is type-of ~s"
                             (type-of "not-symbol"))
                   (princ-to-string condition))))

#-clisp
#?(let ((a "not-symbol"))
    (check-bnf (:whole '(whole ("not-symbol")))
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& (equal #.(format nil "~@<Syntax-error in WHOLE~:@_~2I~:@_~
                             A := SYMBOL~2%~
                             but \"not-symbol\", it is type-of ~s~2%~
                             in ~s~:>"
                             (type-of "not-symbol")
                             '(whole ("not-symbol")))
                   (princ-to-string condition))))

; note! clisp specific bug(?).
; i do not know why but clisp does not call specified report function.
; so header and footer is not generated.
; after bug is removed, guard below will be failed.
#+clisp
#?(let ((a "not-symbol"))
    (check-bnf ()
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& (equal #.(format nil "but \"not-symbol\", it is type-of ~s"
                             (type-of "not-symbol"))
                   (princ-to-string condition))))

; def := (clause+)+
; clause := (var-spec spec+)
; var-spec := [ name | (name name) ]
; name := symbol, otherwise error.
#?(check-bnf ()
    (("not-symbol" dummy)))
:signals syntax-error
,:lazy t

; not evaluated.
#?(check-bnf ()
    (((intern "not evaluated") dummy)))
:signals syntax-error
,:lazy t

; clause require one or more, otherwise syntax error.
#?(check-bnf ())
:signals syntax-error
,:lazy t

; spec := [ type-specifier | bnf ]
; bnf := [ name | list-bnf | or-form ]
; list-bnf := (spec+)
; or-form := (or spec+)

; spec require one or more, otherwise error.
#?(check-bnf ()
    ((dummy)))
:signals syntax-error
,:lazy t

#?(check-bnf ()
    ((dummy+ nil)))
:signals syntax-error
,:lazy t

; result := null

;;;; affected by:
; none

;;;; side-effects:
; none

;;;; notes:
; to check optional argument (e.g. &optional or &key), you can write like below.
#?(let ((option))
    (check-bnf ()
      ((option (or null symbol)))))
=> nil

#?(let ((option 'symbol))
    (check-bnf ()
      ((option (or null symbol)))))
=> nil

#?(let ((option "not-symbol"))
    (check-bnf ()
      ((option (or null symbol)))))
:signals syntax-error

#?(let ((function-name "not function-name"))
    (check-bnf ()
      ((function-name (or name setf-name))
       (name symbol)
       (setf-name ((eql setf) name)))))
:signals syntax-error

; to check optional value in list, you can write like below.
#?(let ((args '(option and others)))
    (check-bnf ()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
=> nil

#?(let ((args '(and others)))
    (check-bnf ()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
=> nil

#?(let ((args '("not option nor other*" and others)))
    (check-bnf ()
      ((args (option? other*))
       (option? (eql option))
       (other* symbol))))
:signals syntax-error

;;;; exceptional-situations:
; every name should not conflicts cl symbol.
#?(let ((list nil))
    (check-bnf ()
      ((list list))))
=> unspecified

;;;; example.
#?(let ((var '(symbol symbol)))
    (check-bnf ()
      ((var (symbol symbol option*))
       (option* keyword string))))
=> nil

#?(let ((var '(symbol symbol)))
    (check-bnf ()
      ((var (symbol symbol option?))
       (option? keyword))))
=> nil

#?(let ((var '(symbol symbol)))
    (check-bnf ()
      ((var (symbol symbol required))
       (required keyword))))
:signals syntax-error

;; right side xxx*
#?(let ((var '(symbol)))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
=> nil

#?(let ((var "string"))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
=> nil

#?(let ((var ()))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
=> nil

#?(let ((var :not-list))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
:signals syntax-error

#?(let ((var '("string" "list")))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
:signals syntax-error

;; right side xxx?
#?(let ((ll nil))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
=> nil

#?(let ((ll '(var)))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
=> nil

#?(let ((ll '(var too much)))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
:signals syntax-error

#?(let ((ll "not symbol"))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
:signals syntax-error

(requirements-about expression :doc-type type)
;;;; description:
;;;; compound type specifier kind:

;;;; compound type specifier syntax:

;;;; compound type specifier arguments:

;;;; compound type specifier description:

(requirements-about pprint-check-bnf :doc-type function
                    :around (let ((*print-pretty* t))
                              (call-body)))

;;;; description:

#+syntax
(pprint-check-bnf stream exp) ; => result

;;;; arguments and values:

; stream := 

; exp := 

; result := 

;;;; affected by:

;;;; side-effects:

;;;; notes:

;;;; exceptional-situations:

;;;; tests:
#?(pprint-check-bnf nil '(check-bnf))
:outputs "(CHECK-BNF)"

#?(pprint-check-bnf nil '(check-bnf nil))
:outputs "(CHECK-BNF ())"

#?(pprint-check-bnf nil '(check-bnf nil nil))
:outputs "(CHECK-BNF () ())"

#?(pprint-check-bnf nil '(check-bnf nil not-list))
=> unspecified ; depending on implementation.

;;; [clisp say](https://clisp.sourceforge.io/impnotes.html#clpp)

;;; > the lisp pretty printer implementation is not perfect yet.

;;; so two tests below are ignored in clisp.
;;; fortunately this is just printing, not check-bnf feature itself.

#-clisp
#?(pprint-check-bnf nil '(check-bnf ()
                           ((a symbol))
                           ((b string))))
:outputs "(CHECK-BNF ()
  ((A SYMBOL))
  ((B STRING)))"

#-clisp
#?(pprint-check-bnf nil '(check-bnf ()
                           ((function-name (or name setf-name))
                            (name symbol)
                            (setf-name ((eql setf) name)))))
:outputs "(CHECK-BNF ()
  ((FUNCTION-NAME (OR NAME SETF-NAME))
   (NAME SYMBOL)
   (SETF-NAME ((EQL SETF) NAME))))"
