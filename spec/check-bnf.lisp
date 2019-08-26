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
      (var symbol)))
=> NIL
#?(let((var "string"))
    (check-bnf()
      (var symbol)))
:signals check-bnf::syntax-error

; When you know VAR is list, and it has 0 or more elt. (a.k.a. *)
; You can write like below.
#?(let((var* nil))
    (check-bnf()
      (var* symbol)))
=> NIL

#?(let((var* '(symbol)))
    (check-bnf()
      (var* symbol)))
=> NIL

#?(let((var* '("string")))
    (check-bnf()
      (var* symbol)))
:signals check-bnf::syntax-error

#?(let((var* :not-list))
    (check-bnf()
      (var* symbol)))
:signals check-bnf::syntax-error

; If you do not like names var as XXX*, you can specify alias.
#?(let((vars '(symbol)))
    (declare(ignore vars))
    (check-bnf()
      ((var* vars)symbol)))
=> NIL

; e.g. specify for plist.
#?(let((var* '(:key "value" :key2 "value2")))
    (check-bnf()
      (var* keyword string)))
=> NIL

#?(let((var* '(:key 2 :key2 "not integer")))
    (check-bnf()
      (var* keyword integer)))
:signals check-bnf::syntax-error

#?(let((var* '(:key 1 "not-key" 2)))
    (check-bnf()
      (var* keyword integer)))
:signals check-bnf::syntax-error

; e.g. specify for alist.
#?(let((var* '((:key "value")(:key2 "value2"))))
    (check-bnf()
      (var* (keyword string))))
=> NIL

#?(let((var* '((:key "value")(:key2 :not-string))))
    (check-bnf()
      (var* (keyword string))))
:signals check-bnf::syntax-error

; When you know VAR is list, and it has 1 or more elt, (a.k.a. +)
; you can write like below.
#?(let((var+ '(1)))
    (check-bnf()
      (var+ integer)))
=> NIL

#?(let((var+ '()))
    (check-bnf()
      (var+ integer)))
:signals check-bnf::syntax-error

#?(let((var+ '("not-integer")))
    (check-bnf()
      (var+ integer)))
:signals check-bnf::syntax-error

#?(let((var+ :not-cons))
    (check-bnf()
      (var+ integer)))
:signals check-bnf::syntax-error

#+syntax
(CHECK-BNF (&key ((:whole whole?)) ((:name name?))) &rest clause+) ; => result

;;;; Arguments and Values:

; whole := form, evaluated.
#?(check-bnf(:whole no-such-var)) :signals error
; Expects var for &WHOLE.

; name := form, evaluated.
#?(check-bnf(:name no-such-var)) :signals error
; Expects macro name which CHECK-BNF checks.

; clause := (var-spec spec+)
; var-spec := [ name | (name name) ]
; name := SYMBOL, otherwise error.
#?(check-bnf()("not-symbol" dummy)) :signals error
; Not evaluated.
#?(check-bnf()((intern "not evaluated") dummy)) :signals error

; spec := [ type-specifier | bnf ]
; bnf := [ name | list-bnf | or-form ]
; list-bnf := (spec+)
; or-form := (or spec+)

; result := NULL

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EXPRESSION :doc-type type)
;;;; Description:
;;;; Compound Type Specifier Kind:

;;;; Compound Type Specifier Syntax:

;;;; Compound Type Specifier Arguments:

;;;; Compound Type Specifier Description:

