(defpackage :check-bnf.spec
  (:import-from #:check-bnf #:pprint-check-bnf)
  (:use :cl :jingoh :check-bnf))
(in-package :check-bnf.spec)
(setup :check-bnf)

(requirements-about pretty-printer :doc-type nil)

; CLISP specific guard [1]
#+clisp
#?(pprint-logical-block (*standard-output* nil)
    (pprint-logical-block (*standard-output* nil)
      (format t "~VA" 3 'var)))
:outputs " VAR"
,:comment "When this guard failed, spec annotated [1] should be fixed."

; ABCL has issue about mandatory newline. [2]
#+abcl
#?(format nil "~<~:@_~:>" nil) => ""
,:test equal
,:comment "When this guard failed, spec annotated [2] should be fixed."

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
         (& #-clisp
            (string= (princ-to-string condition)
                     (format nil
                             "VAR : \"string\" comes. It is type-of ~S.~%  Definition~2%  VAR := SYMBOL"
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
:expanded-to NIL

#?(check-bnf ()
    ((a* t)))
:expanded-to
(let ((check-bnf::*whole* nil)
      (check-bnf::*bnf* '((a* t))))
  (labels ((a* (a*)
               (if (typep a* '(and atom (not null)))
                 (let ((check-bnf::*default-condition* 'check-bnf::violate-list))
                   (syntax-error 'a* "~A require LIST but ~S." 'a* a*))
                 nil)))
    (a* a*)))

; Also it occur in OR form.
#?(check-bnf ()
    ((a (or symbol t string))))
:expanded-to NIL

#?(check-bnf ()
    ((a (or symbol b string))
     (b t)))
:expanded-to NIL

#?(check-bnf ()
    ((a (or b c))
     (b integer)
     (c t)))
:expanded-to NIL

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
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR* : \"string\" comes. It is type-of ~S.~%~
			     in (\"string\")~%  Definition~2%  VAR := SYMBOL*"
                             (type-of "string")))))

#?(let ((var* :not-list))
    (check-bnf ()
      ((var* symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil "VAR* require LIST but :NOT-LIST.~%  Definition~2%  VAR := SYMBOL*"))))

; When expected T, efficient code is generated.
#?(CHECK-BNF () ((OPTION* KEYWORD T)))
:expanded-to
(LET ((check-bnf::*WHOLE* NIL) (check-bnf::*BNF* '((OPTION* KEYWORD T))))
  (LABELS ((OPTION* (OPTION*)
             (IF (TYPEP OPTION* '(AND ATOM (NOT NULL)))
                 (let ((check-bnf::*default-condition* 'check-bnf::violate-list))
                   (SYNTAX-ERROR 'OPTION* "~A require LIST but ~S."
                                 'option*
                                 OPTION*))
                 (LOOP :FOR args :ON OPTION* :BY #'CDDR
                       :for (G11516 G11517)
                           := (if (typep args '(cons * (cons * *)))
                                args
                                (let ((check-bnf::*default-condition*
                                        'check-bnf::may-syntax-error))
                                  (syntax-error 'option*
                                                "~A : Length mismatch. Lack last ~{~S~^ ~} of ~S~:@_in ~S"
                                                'option*
                                                (subseq '(keyword t)
                                                        (mod (length args) 2))
                                                '(keyword t)
                                                args)))
                       :DO (FUNCALL
                               (check-bnf::RESIGNALER 'OPTION* args)
                             (check-bnf::CAPTURE-SYNTAX-ERROR
                              (UNLESS (TYPEP G11516 'KEYWORD)
                                (SYNTAX-ERROR 'OPTION*
                                              "~A : ~S comes. It is type-of ~S."
                                              'option*
                                              G11516
                                              (TYPE-OF
                                               G11516))))
                             (check-bnf::IGNORED G11517))))))
    (OPTION* OPTION*)))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil "VAR* : \"not integer\" comes. It is type-of ~S.~%~
			     in (:KEY2 \"not integer\")~%  Definition~2%  ~
			     VAR := { KEYWORD INTEGER }*"
                             (type-of "not integer")))))

#?(let ((var* '(:key 1 "not-key" 2)))
    (check-bnf ()
      ((var* keyword integer))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR* : \"not-key\" comes. It is type-of ~S.~%~
                             in (\"not-key\" 2)~%  Definition~2%  ~
			     VAR := { KEYWORD INTEGER }*"
                             (type-of "not-key")))))

#?(let ((var* '(:not "ballanced" :plist)))
    (check-bnf ()
      ((var* keyword string))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR* : Length mismatch. Lack last STRING of (KEYWORD STRING)~%~
			     in (:PLIST)~%  Definition~2%  ~
			     VAR := { KEYWORD STRING }*"))))

#?(let ((var* '(0 1)))
    (check-bnf ()
               ((var* keyword string))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR* : 0 comes. It is type-of ~S.~%~
                             VAR* : 1 comes. It is type-of ~S.~%~
                             in (0 1)~%  Definition~2%  ~
			     VAR := { KEYWORD STRING }*"
                             (type-of 0) (type-of 1)))))

; e.g. specify for alist.
#?(let ((var* '((:key "value") (:key2 "value2"))))
    (check-bnf ()
      ((var* (keyword string)))))
=> nil

#?(let ((var* '((:key "value") (:key2 :not-string))))
    (check-bnf ()
      ((var* (keyword string)))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "STRING : :NOT-STRING comes. It is type-of ~S~%~
                             in ((:KEY2 :NOT-STRING))~%  Definition~2%  ~
			     VAR := (KEYWORD STRING)*"
			     (type-of :dummy)))))

#?(let ((var* '((:key "value") (:not "ballanced" clause))))
    (check-bnf ()
      ((var* (keyword string)))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "Length mismatch. (KEYWORD STRING) but (:NOT \"ballanced\" CLAUSE)~%~
                             in ((:NOT \"ballanced\" CLAUSE))~%  Definition~2%  ~
			     VAR := (KEYWORD STRING)*"))))

#?(let ((var* '((:key "value") (:not-ballanced))))
    (check-bnf ()
      ((var* (keyword string)))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "Length mismatch. (KEYWORD STRING) but (:NOT-BALLANCED)~%~
                             in ((:NOT-BALLANCED))~%  Definition~2%  ~
			     VAR := (KEYWORD STRING)*"))))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR+ require CONS but NIL.~%  Definition~2%  ~
			     VAR := INTEGER+"))))

#?(let ((var+ '("not-integer")))
    (check-bnf ()
      ((var+ integer))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR+ : \"not-integer\" comes. It is type-of ~S.~%~
                             in (\"not-integer\")~%  Definition~2%  ~
			     VAR := INTEGER+"
                             (type-of "not-integer")))))

#?(let ((var+ :not-cons))
    (check-bnf ()
      ((var+ integer))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR+ require CONS but :NOT-CONS.~%  Definition~2%  ~
			     VAR := INTEGER+"))))

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
#?(let ((a "not-symbol"))
    (check-bnf ()
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (equal #.(format nil
                             "A : \"not-symbol\" comes. It is type-of ~S.~%  Definition~2%  ~
			     A := SYMBOL"
                             (type-of "not-symbol"))
                   (princ-to-string condition))))

#?(let ((a "not-symbol"))
    (check-bnf (:whole '(whole ("not-symbol")))
      ((a symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (equal (format nil
                           "A : \"not-symbol\" comes. It is type-of ~S.~%  ~
			   Definition~2%  ~
                           A := SYMBOL"
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

; NAME must not be a type name.
#?(deftype type-name () 'symbol) => TYPE-NAME
#?(check-bnf ()
    ((type-name 'symbol)))
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

#-allegro
#?(let ((option "not-symbol"))
    (check-bnf ()
      ((option (or null symbol)))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "OPTION : \"not-symbol\" comes. It is type-of ~S.~%  Definition~2%  ~
			     OPTION := [ NULL | SYMBOL ]"
                             (type-of "not-symbol")))))

#?(let ((function-name "not function-name"))
    (check-bnf ()
      ((function-name (or name setf-name))
       (name symbol)
       (setf-name ((eql setf) name)))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil "FUNCTION-NAME : \"not function-name\" comes.~%~
			     Last failed at SETF-NAME.~%  ~
			     Detail: SETF-NAME require CONS but \"not function-name\".~2%  ~
			     Definition~2%  ~
			     FUNCTION-NAME := [ NAME | SETF-NAME ]~%  ~
                             NAME          := SYMBOL~%  ~
                             SETF-NAME     := ((EQL SETF) NAME)"))))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-(or clisp ; [1]
		  abcl) ; [2]
            (string= (princ-to-string condition)
                     (format nil
                             "OTHER* : \"not option nor other*\" comes. It is type-of ~:_~S.~%~
                             in (\"not option nor other*\" AND OTHERS)~%  Definition~2%  ~
			     OTHER := SYMBOL*"
                             (type-of "not option nor other*")))))

#?(LET (VAR)
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
=> NIL

#?(LET ((VAR '(SYM)))
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
=> NIL

#?(LET ((VAR '(SYM SYM)))
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
=> NIL

#?(LET ((VAR '(SYM SYM 1)))
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
=> NIL
#?(LET ((VAR '(SYM SYM 1 2)))
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
=> NIL

#?(LET ((VAR '(SYM SYM "string")))
    (CHECK-BNF ()
      ((VAR (A* B*))
       (A SYMBOL)
       (B INTEGER))))
:invokes-debugger SYNTAX-ERROR
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "B : \"string\" comes. It is type-of ~S.~%~
                             in (\"string\")~%  Definition~2%  ~
                             B := INTEGER"
                             (type-of "string")))))

; Key value pair in heads.
#?(LET ((ARGS '(:KEY 1 :KEY 2 "doc")))
    (CHECK-BNF ()
      ((ARGS (OPTION* DOC?))
       (OPTION* KEYWORD INTEGER)
       (DOC? STRING))))
=> NIL

#?(LET ((ARGS '(:KEY 1 :KEY 2)))
    (CHECK-BNF ()
      ((ARGS (OPTION* DOC?))
       (OPTION* KEYWORD INTEGER)
       (DOC? STRING))))
=> NIL

#?(LET ((ARGS '(:KEY 1 :KEY 2 not-string)))
    (CHECK-BNF ()
      ((ARGS (OPTION* DOC?))
       (OPTION* KEYWORD INTEGER)
       (DOC? STRING))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "DOC? : NOT-STRING comes. It is type-of SYMBOL.~%  Definition~2%  ~
			     DOC := STRING?"))))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "Length mismatch. (SYMBOL SYMBOL REQUIRED) but (SYMBOL SYMBOL)~%  Definition~2%  ~
			     REQUIRED := KEYWORD"))))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR : :NOT-LIST comes.~%  Definition~2%  ~
			     VAR  := [ STRING | NAME* ]~%  ~
                             NAME := SYMBOL"))))

#-allegro
#?(let ((var '("string" "list")))
    (check-bnf ()
      ((var (or string name*))
       (name symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR : (\"string\" \"list\") comes.~%~
			     Last failed at NAME.~%  ~
			     Detail: NAME : \"string\" comes. It is type-of ~S.~%          ~
			     in (\"string\" \"list\")~2%  ~
			     Definition~2%  ~
			     VAR  := [ STRING | NAME* ]~%  ~
                             NAME := SYMBOL"
			     (type-of "string")))))

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
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "Length mismatch. (VAR?) but (VAR TOO MUCH)~%  Definition~2%  ~
			     VAR := SYMBOL"))))

#?(let ((ll "not list"))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
			     "LL require LIST but \"not list\".~%  Definition~2%  ~
			     LL  := (VAR?)~%  ~
                             VAR := SYMBOL"))))

#?(let ((ll '("not symbol")))
    (check-bnf ()
      ((ll (var?))
       (var symbol))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "VAR : \"not symbol\" comes. It is type-of ~S.~%  Definition~2%  ~
			     VAR := SYMBOL"
                             (type-of "not symbol")))))

;;;; Practical case examples.
;; deftype.
#?(let ((name 'name)
        (lambda-list '())
        (body '("doc" t)))
    (check-bnf ()
      ((name (and symbol (not (or keyword boolean)))))
      ((lambda-list list))
      ((body (doc? declaration* expression*))
       (doc? string)
       (declaration* ((eql declare) t*)))))
=> NIL

#?(let ((body '("doc" t)))
    (check-bnf ()
      ((body (string? declaration* expression*))
       (declaration* ((eql declare) t*)))))
=> NIL

;;;; Tests
#?(let ((ll "not-list"))
    (check-bnf ()
      ((ll <lambda-list>))))
:invokes-debugger syntax-error
,:test (lambda (condition)
         (& #-clisp ; [1]
            (string= (princ-to-string condition)
                     (format nil
                             "<LAMBDA-LIST> require LIST but \"not-list\".~%  Definition~2%  ~
			     <LAMBDA-LIST> := (LAMBDA-ELT*)~%  ~
                             LAMBDA-ELT    := [ VAR | INIT-FORM ]~%  ~
                             VAR           := (AND SYMBOL (NOT [ BOOLEAN | KEYWORD ]))~%  ~
                             INIT-FORM     := ([ SYMBOL | EXTERNAL-SPEC ] EXPRESSION SUPPLIEDP?)~%  ~
                             EXTERNAL-SPEC := (SYMBOL VAR)~%  ~
                             SUPPLIEDP     := VAR?"))))

;; Support compound type specifier.
;; ISSUE: Right side expression may be type specifier accidentally?
;; e.g. (vector spec)
#?(let ((index 3))
    (check-bnf ()
      ((index (mod #.array-total-size-limit)))))
=> NIL

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

; CLISP specific guard #2.
#+clisp ; CLISP does not remove spaces before newline.
#?(pprint-check-bnf nil '(check-bnf ()
                           ((a symbol))
                           ((b string))))
:outputs "(CHECK-BNF () 
  ((A SYMBOL))
  ((B STRING)))"

#-clisp ; #2
#?(pprint-check-bnf nil '(check-bnf ()
                           ((a symbol))
                           ((b string))))
:outputs "(CHECK-BNF ()
  ((A SYMBOL))
  ((B STRING)))"

; CLISP specific guard #3.
#+clisp ; CLISP's pprint-newline :mandatory does not work.
#?(format nil "~<~:@_~:>" nil) => ""
,:test equal

#-clisp ; #3
#?(pprint-check-bnf nil '(check-bnf ()
                           ((function-name (or name setf-name))
                            (name symbol)
                            (setf-name ((eql setf) name)))))
:outputs "(CHECK-BNF ()
  ((FUNCTION-NAME (OR NAME SETF-NAME))
   (NAME SYMBOL)
   (SETF-NAME ((EQL SETF) NAME))))"

#-clisp ; #2
#?(PPRINT-CHECK-BNF NIL
                    '(CHECK-BNF ()
                       (A
                        B)))
:outputs "(CHECK-BNF ()
  (A
   B))"

(requirements-about millet :doc-type nil)

;; millet:type-specifier-p must satisfies the test bellow.
#?(millet:type-specifier-p '(expression t)) => NIL

;; Millet does not support retrive lambda-list from c2mop:funcallable-object.
;; Some implementations can retrieve.
#-(or ccl ecl abcl allegro)
#?(millet:lambda-list #'<lambda-list>)
:satisfies (lambda (lambda-list)
	     (& (typep lambda-list '(cons symbol null))))

; When this guard failed, we may cleanup :ignore-signals bellow.
#+(or abcl ccl allegro)
#?(millet:lambda-list #'<lambda-list>) => ()

#+ecl
#?(millet:lambda-list #'<lambda-list>) :signals error
,:comment "When this guard is failed, we should cleanup test below."

(requirements-about <LAMBDA-LIST> :doc-type function)

;;;; Description:

#+syntax (<LAMBDA-LIST> <lambda-list>) ; => result

;;;; Arguments and Values:

; <lambda-list> := t as tha form that is lambda list.
#?(<LAMBDA-LIST> NIL) => NIL

; result := NULL or an error.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:
; This does not warrant the <lambda-list> is valid.
#?(<LAMBDA-LIST> '((VAR T))) => NIL
#?(<LAMBDA-LIST> '((VAR T VAR2))) => NIL

;;;; Exceptional-Situations:
; When the form is not lambda-list an error is signaled.

;;;; TESTS.
#?(<LAMBDA-LIST> :NOT-LIST) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<LAMBDA-LIST> '(:NOT-VAR)) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<LAMBDA-LIST> '(("not init-form var" :DUMMY))) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<LAMBDA-LIST> '((LESS-ELT-FOR-INIT-FORM))) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<LAMBDA-LIST> '((VAR T "not supplied var"))) :signals SYNTAX-ERROR
,:ignore-signals warning

(requirements-about <FUNCTION-TYPE> :doc-type function)

;;;; Description:

#+syntax (<FUNCTION-TYPE> <function-type>) ; => result

;;;; Arguments and Values:

; <function-type> := t as the form that is function form.

; result := NULL or an error.

;;;; Affected By:
; none.

;;;; Side-Effects:
; none.

;;;; Notes:
; This does not warrant the <function-type> is valid.
#?(<FUNCTION-TYPE> '(FUNCTION (&KEY (NOT-KEYWORD INTEGER)) *)) => NIL

;;;; Exceptional-Situations:

;;;; Tests.
#?(<FUNCTION-TYPE> 'FUNCTION) => NIL
#?(<FUNCTION-TYPE> :INVALID) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<FUNCTION-TYPE> '(FUNCTION * *)) => NIL
#?(<FUNCTION-TYPE> '(FUNCTION :INVALID-AS-LAMBDA-LIST)) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<FUNCTION-TYPE> '(FUNCTION ("not lambda elt") *)) :signals SYNTAX-ERROR
,:ignore-signals warning
#?(<FUNCTION-TYPE> '(FUNCTION NIL INTEGER)) => NIL
#?(<FUNCTION-TYPE> '(FUNCTION NIL (VALUES INTEGER))) => NIL
#?(<FUNCTION-TYPE> '(FUNCTION NIL (VALUES INTEGER &OPTIONAL))) => NIL
#?(<FUNCTION-TYPE> '(FUNCTION NIL (VALUES INTEGER FIXNUM &OPTIONAL))) => NIL
#?(<FUNCTION-TYPE> '(FUNCTION (&KEY (:NAME INTEGER)) *)) => NIL

(requirements-about <DECLARATION> :doc-type function)

;;;; Description:

#+syntax (<DECLARATION> <declaration>) ; => result

;;;; Arguments and Values:

; <declaration> := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

;;;; Examples.
#?(<DECLARATION> '(DECLARE (IGNORE VAR))) => NIL
#?(<DECLARATION>
   '(DECLARE (IGNORE VAR)
             (TYPE FIXNUM A)))
=> NIL
#?(<DECLARATION>
   '(DECLARE (IGNORE VAR)
             (:TYPE FIXNUM A)))
:signals SYNTAX-ERROR
,:ignore-signals warning
