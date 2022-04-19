(in-package :cl-user)

(defpackage :check-bnf
  (:use :cl)
  (:export ;;;; Main API
           #:check-bnf
           #:doc
           ;;;; Condition
           #:syntax-error
           ;;;; Miscellaneous TYPE.
           #:expression)
  (:export ;;;; DSL
           #:defbnf
           ;;;; Native support.
           #:<lambda-list>
           #:<function-type>
           #:<declaration>))

(in-package :check-bnf)

(declaim (optimize speed))

(eval-when (:compile-toplevel :load-toplevel)
  ;; CHECK-BNF depends on the system "millet" the wrapper of implementation dependent utilities.
  ;; Impls bellow are supported by millet.
  ;; Comment outed impls are not supported due to it has its own issues.
  #+(or sbcl ccl cmucl clisp abcl ecl #|lispworks|# allegro #|clasp|#)
  (pushnew :check-bnf *features*))

(eval-when (:load-toplevel)
  #-check-bnf
  (warn "CHECK-BNF does not support ~S. (Do nothing.)"
        (lisp-implementation-type)))

;;;; TYPES

(deftype type-specifier () '(or symbol cons))

(deftype expression () t)

;;;; SPECIAL VARIABLES

(defvar *whole* nil)

(defvar *bnf* nil)

;;;; CONDITION

(defun extended-marker (name)
  (let* ((length (length (symbol-name name)))
         (char (and (< 1 length) (char (symbol-name name) (1- length)))))
    (find char "+*?")))

(declaim
 (ftype (function (t) (values (or null symbol) (or null character) &optional))
        but-extended-marker))

(defun but-extended-marker (thing)
  (if (not (symbolp thing))
      (values nil nil)
      (let ((mark (extended-marker thing)))
        (case mark
          ((#\? #\* #\+)
           (let ((name (symbol-name thing)))
             (values (intern (subseq name 0 (1- (length name)))
                             (symbol-package thing))
                     mark)))
          (otherwise (values thing nil))))))

(defun definitions (thing bnf)
  (let ((acc))
    (labels ((rec (thing)
               (let ((definition
                      (or (assoc thing bnf)
                          (assoc (but-extended-marker thing) bnf))))
                 (cond
                   (definition
                    (pushnew definition acc :key #'car)
                    (body (cdr definition)))
                   ((typep thing '(not cons)) nil)
                   ((millet:type-specifier-p thing) nil)
                   (t (body thing)))))
             (body (list)
               (dolist (spec list)
                 (declare (type (not number) spec))
                 (when (and (not (eq spec thing)) (not (assoc spec acc)))
                   (rec spec)))))
      (declare
        (ftype (function ((or symbol list)) (values null &optional)) rec))
      (rec thing))
    (nreverse acc)))

(define-condition syntax-error (program-error simple-error cell-error)
  ((whole :initform nil :initarg :whole :reader whole-form<=syntax-error)
   (definitions :initform nil :initarg :definitions :reader bnf-definitions))
  (:report
   (lambda (condition stream)
     (let ((*print-pretty* t))
       (funcall
         (formatter
          #.(concatenate 'string "~<" ; pprint-logical-block
                         "~@[Syntax-error in ~S~:@_~2I~:@_~]" ; header.
                         "~/check-bnf:pprint-definitions/~I~:@_" ; body
                         "~?" ; format-control
                         "~@[~:@_~:@_in ~S~]" ; whole
                         "~:>"))
         stream
         (list (car (whole-form<=syntax-error condition))
               (definitions (cell-error-name condition)
                            (bnf-definitions condition))
               (simple-condition-format-control condition)
               (simple-condition-format-arguments condition)
               (whole-form<=syntax-error condition))))))
  (:default-initargs :format-control ""))

(define-condition violate-list (syntax-error) ())

(define-condition length-mismatch (syntax-error) ())

(define-condition may-syntax-error (syntax-error) ())

;;;; SIGNALER

(defvar *default-condition* 'syntax-error)

(defun syntax-error (name format-control &rest format-arguments)
  (error
    (make-condition *default-condition*
                    :name name
                    :format-control format-control
                    :format-arguments format-arguments
                    :whole *whole*
                    :definitions *bnf*)))

;;;; CHECKER

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; CHECKER-P below needs this eval-when.
  (defclass checker ()
    ((definitions :initarg :definitions :reader bnf-definitions))
    (:metaclass c2mop:funcallable-standard-class)))

(defmethod initialize-instance :after
           ((checker checker) &key function &allow-other-keys)
  (c2mop:set-funcallable-instance-function checker function))

;;; CHECKER UTILITIES.

(defun checker-p (thing) (typep thing 'checker))

(defun cboundp (symbol)
  (and (symbolp symbol) (fboundp symbol) (checker-p (symbol-function symbol))))

;;;; UTILITIES

(defun cons-equal (list1 list2) (tree-equal list1 list2 :test (constantly t)))

(defun ignored (arg) (declare (ignore arg)) nil)

(defmacro capture-syntax-error (form)
  `(handler-case ,form
     (syntax-error (c)
       c)
     (:no-error (&rest args)
       (declare (ignore args))
       nil)))

(defun at-least-n-cons (n)
  (declare (type fixnum n))
  (if (zerop n)
      '*
      `(cons * ,(at-least-n-cons (1- n)))))

;;;; SPEC-INFER

(defun t-p (thing &optional (*bnf* *bnf*))
  (let ((seen))
    (labels ((rec (thing)
               (let ((spec (assoc thing *bnf*)))
                 (if spec
                     (if (find (the symbol (car spec)) seen)
                         nil
                         (progn
                          (push (car spec) seen)
                          (every #'rec (cdr spec))))
                     (cond
                       ((millet:type-specifier-p thing)
                        (if (typep thing '(cons (eql or) *))
                            (some #'rec (cdr thing))
                            (eq t (millet:type-expand thing))))
                       ((cboundp thing)
                        (t-p thing (bnf-definitions (symbol-function thing))))
                       ((atom thing)
                        (multiple-value-bind (but mark)
                            (but-extended-marker thing)
                          (if (null (assoc but *bnf*))
                              (if (eql #\? mark)
                                  (t-p but)
                                  (error "NIY"))
                              (when mark
                                (t-p but)))))
                       ((typep thing '(cons (eql or) *))
                        (some #'rec (cdr thing)))
                       ((consp thing)
                        (do ((spec thing (cdr spec)))
                            ((atom spec)
                             (if (null spec)
                                 t
                                 (t-p spec)))
                          (unless (t-p (car spec))
                            (return nil)))))))))
      (declare
        (ftype (function ((or symbol list)) (values boolean &optional)) rec))
      (rec thing))))

;;;; CONDITION REPORTER.

(defun pprint-def-elt (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (etypecase exp
    ((and symbol (not keyword)) (princ exp stream))
    (atom (prin1 exp stream))
    ((cons (member eql) *) (prin1 exp stream))
    ((cons (member member or) *)
     (if (and exp (null (cddr exp))) ; As one-elemen-p
         (pprint-def-elt stream (cadr exp))
         (funcall
           (formatter
            #.(apply #'concatenate 'string
                     (alexandria:flatten
                       (list "~<" ; pprint-logical-block.
                             "[" ; prefix.
                             (list "~@{" ; iterate.
                                   " ~/check-bnf:pprint-def-elt/ ~_~^|" ; each
                                                                        ; elt.
                                   "~}")
                             "]" ; suffix.
                             "~:>"))))
           stream (cdr exp))))
    (cons
     (funcall (formatter "~:<~@{~/check-bnf:pprint-def-elt/~^ ~_~}~:>") stream
              exp))))

(defun pprint-def-clause (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (if (atom exp)
      (write exp :stream stream)
      (funcall
        (formatter
         #.(apply #'concatenate 'string
                  (alexandria:flatten
                    (list "~:[" ; if one elt.
                          (list "~{" ; iterate.
                                "~/check-bnf:pprint-def-elt/~^ " ; each elt.
                                "~}")
                          "~;" ; else.
                          ;; some elements.
                          (list "~@<" ; pprint-logical-block.
                                "{ " ; prefix.
                                (list "~{" ; iterate.
                                      "~/check-bnf:pprint-def-elt/ ~_~^" ; each-elt
                                      "~}")
                                "}" ; suffix.
                                "~:>")
                          "~]"))))
        stream (cdr exp) exp)))

(defun pprint-bnf (stream exp &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (if (atom exp)
      (princ exp stream)
      (funcall (formatter "~<~@{~/check-bnf:pprint-definitions/~^~:@_~}~:>")
               stream (cddr exp))))

(defmacro doc (&body forms)
  (with-output-to-string (s)
    (let ((*print-pretty* t))
      (funcall (formatter "~<~@{~/check-bnf:pprint-bnf/~^~:@_~}~:>") s forms))))

(defun pprint-definitions (stream definitions &rest noise)
  (declare (ignore noise))
  (setf stream (or stream *standard-output*))
  (pprint-logical-block (stream definitions)
    (funcall
      (formatter
       #.(apply #'concatenate 'string
                (alexandria:flatten
                  (list "~{" ; definitions
                        (list "~{" ; each line.
                              "~VA := " ; name.
                              "~/check-bnf:pprint-def-clause/" ; def-clause.
                              "~@[~A~]~:@_" ; extended marker.
                              "~}")
                        "~}"))))
      stream
      (let ((num
             (reduce #'max definitions
                     :initial-value 0
                     :key (alexandria:compose 'length 'string
                                              'but-extended-marker 'car))))
        (mapcar
          (lambda (definition)
            (multiple-value-bind (name mark)
                (but-extended-marker (car definition))
              (list num name (cdr definition) mark)))
          definitions)))))

;;;; SPEC the intermediate object.

(defstruct (spec (:constructor spec (name checker)))
  (name (error "Name is Required for spec.") :type symbol :read-only t)
  (checker (error "Checker is Required for spec.") :type function :read-only t))

(defun check-cons (actual-args specs name)
  (flet ((check (actual null-thunk spec)
           (typecase actual
             (null (funcall null-thunk))
             (atom (local-check actual spec))
             (otherwise (local-check (car actual) spec))))
         (length-mismatch ()
           (let ((*default-condition* 'length-mismatch))
             (syntax-error name "Length mismatch. ~S but ~S" name
                           actual-args))))
    (do ((actual actual-args (cdr actual))
         (specs specs (cdr specs)))
        ((atom specs)
         (matrix-case:matrix-typecase (actual specs)
           ((null null))
           ((atom t) (local-check actual specs))
           (otherwise (length-mismatch))))
      (let ((elt (car specs)))
        (if (spec-p elt)
            (case (extended-marker (spec-name elt))
              (#\?
               (handler-case (check actual (lambda ()) elt)
                 (syntax-error (c)
                   (if (cdr specs)
                       (push "dummy" actual)
                       (error (the condition c))))))
              ((#\*)
               (handler-case (local-check actual elt)
                 (may-syntax-error (condition)
                   (if (cdr specs)
                       (setf actual
                               (cons "dummy"
                                     (car
                                       (last
                                         (simple-condition-format-arguments
                                           condition)))))
                       (error (the condition condition))))
                 (:no-error (&rest _)
                   (declare (ignore _))
                   (setf actual nil))))
              ((#\+) (local-check actual elt) (setf actual nil))
              (otherwise (check actual #'length-mismatch elt)))
            (check actual #'length-mismatch elt))))))

(declaim
 (ftype (function (t t)
         (values null ; or signals an error.
                 &optional))
        local-check))

(defun local-check (name spec)
  (cond
    ((millet:type-specifier-p spec)
     (unless (eql t (millet:type-expand spec))
       (unless (locally ; due to spec is dynamic.
		 #+sbcl
                (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                (typep name spec))
         (syntax-error name "but ~S, it is type-of ~S" name (type-of name)))))
    ((spec-p spec) (funcall (spec-checker spec) name))
    ((typep spec '(cons (eql or) *))
     (loop :for (spec+ . rest) :on (cdr spec)
           :if (null rest)
             :do (handler-case (local-check name spec+)
                   (syntax-error ()
                     (syntax-error name "~S := ~S but not exhausted. ~S" name
                                   spec spec+)))
           :else
             :do (ignore-errors (local-check name spec+))))
    ((consp spec) (check-cons name spec spec))
    ((functionp spec) (funcall spec name))
    (t (error "NIY: Name: ~S, Spec ~S" name spec))))

;;;; FORMS

(defun <check-type-form> (name var type-specifier)
  `(unless (typep ,var ',type-specifier)
     (syntax-error ',name "~A: ~S comes, it is type-of ~S." ',name ,var
                   (type-of ,var))))

(defun <local-type-check-form> (name var spec)
  (cond ((t-p spec) nil)
        ((eql nil (millet:type-expand spec))
         (syntax-error 'check-bnf "NIL is invalid.~%HINT: NULL?"))
        (t (<check-type-form> name var spec))))

(defun <local-atom-check-form> (name var spec)
  (multiple-value-bind (but mark)
      (but-extended-marker spec)
    (case mark
      ((#\+ #\*)
       `(funcall
          (,(find-symbol (format nil "~C-CHECKER" mark) :check-bnf) ',name
           #',but)
          ,var))
      (otherwise `(,spec ,var)))))

;; <LOCAL-CHECK-FORM>

(defun <local-check-form> (name var spec)
  (cond
    ((millet:type-specifier-p spec) (<local-type-check-form> name var spec))
    ((atom spec) (<local-atom-check-form> name var spec))
    ((typep spec '(cons (eql or) *)) (<local-or-check-form> name var spec))
    ((consp spec) (<local-cons-check-form> name var spec))))

(defun <local-or-check-form> (name var spec)
  (if (t-p spec)
      nil
      `(block nil
         (tagbody
          ,@(loop :for (spec . rest) :on (cdr spec)
                  :collect `(handler-case ,(<local-check-form> name var spec)
                              (syntax-error ()
                                ,(if rest
                                     nil
                                     `(syntax-error ',name "~A: ~S comes."
                                                    ',name ,var)))
                              (:no-error (&rest args)
                                (declare (ignore args))
                                (return))))))))

(defun <spec-form> (spec name)
  (flet ((may-checker-form (thing)
           (if (t-p thing)
               `(constantly nil)
               `(lambda (x) ,(<check-type-form> spec 'x thing)))))
    (cond ((null spec) nil)
          ((millet:type-specifier-p spec) `',spec)
          ((atom spec)
           (multiple-value-bind (but mark)
               (but-extended-marker spec)
             `(spec ',spec
                    ,(if (find mark "+*?")
                         (if (assoc but *bnf*)
                             (ecase mark
                               (#\+ `(+-checker ',name #',but))
                               (#\* `(*-checker ',name #',but))
                               (#\? `#',but))
                             (if (millet:type-specifier-p but)
                                 (ecase mark
                                   (#\+
                                    `(+-checker ',name
                                                ,(may-checker-form but)))
                                   (#\*
                                    `(*-checker ',name
                                                ,(may-checker-form but)))
                                   (#\? (may-checker-form but)))
                                 `#',spec))
                         `#',spec))))
          ((typep spec '(cons (eql or) *))
           `(lambda (x) ,(<local-or-check-form> name 'x spec)))
          ((consp spec)
           `(cons ,(<spec-form> (car spec) name)
                  ,(<spec-form> (cdr spec) name))))))

(defun <local-cons-check-form> (name var spec)
  (if (t-p spec)
      `(unless (cons-equal ,var ',spec)
         (let ((*default-condition* 'length-mismatch))
           (syntax-error ',spec "Length mismatch. ~S but ~S" ',spec ,var)))
      `(if (typep ,var '(and atom (not null)))
           ,(if (flet ((check (s)
                         (and (symbolp s) (find (extended-marker s) "*?"))))
                  (do ((s spec (cdr s)))
                      ((atom s) (or (null s) (check s)))
                    (unless (check (car s))
                      (return nil))))
                `(syntax-error ',name "~A: Require LIST but ~S" ',name ,name)
                `(syntax-error ',name "~A: Require CONS but ~S" ',name ,name))
           (check-cons ,var ,(<spec-form> spec name) ',spec))))

;; <REQUIRE-FORM>

(defun <require-form> (name spec+)
  (if (cdr spec+)
      (error "BNF definition error: ~S must have + or * its name at last."
             name)
      (let ((form (<local-check-form> name name (car spec+))))
        `(,name (,name)
          #+sbcl (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
          ,@(if form
                (list form)
                `((declare (ignore ,name)) nil))))))

;; <+FORM>

(declaim
 (ftype (function (symbol list) (values function &optional)) resignaler))

(defun resignaler (name actual-args)
  (lambda (&rest conditions)
    (when (find-if (lambda (x) (typep x 'syntax-error)) conditions)
      (let ((*default-condition*
             (if (null (car conditions))
                 'syntax-error
                 'may-syntax-error))
            (*bnf*
             (loop :for c :in conditions
                   :when c
                     :append (bnf-definitions c) :into defs
                   :finally (return (append *bnf* defs)))))
        (syntax-error name "~{~?~^ ~:@_~}~@?"
                      (loop :for c :in conditions
                            :when c
                              :collect (simple-condition-format-control c)
                              :and :collect (simple-condition-format-arguments
                                              c))
                      "~2I~:@_in ~S~I" actual-args)))))

(defun <*form-body> (name spec+)
  (let* ((length (list-length spec+))
         (gsyms (alexandria:make-gensym-list length))
         (args (gensym "ARGS"))
         (forms
          (loop :for g :in gsyms
                :for spec :in spec+
                :for form = (<local-check-form> name g spec)
                :when form
                  :collect `(capture-syntax-error ,form)
                :else
                  :collect `(ignored ,g))))
    (if (null (remove 'ignored forms :key #'car))
        nil
        `(loop :for ,args :on ,name
                    :by ,(let ((length (list-length spec+)))
                           (case length
                             (1 '#'cdr)
                             (2 '#'cddr)
                             (3 '#'cdddr)
                             (4 '#'cddddr)
                             (otherwise
                              `(lambda (list) (nthcdr ,length list)))))
               :for ,gsyms
                    := ,(if (< 1 length)
                            `(if (typep ,args ',(at-least-n-cons length))
                                 ,args
                                 (let ((*default-condition* 'may-syntax-error))
                                   (syntax-error ',name
                                                 "~A: Length mismatch. Lack last ~{~S~^ ~} of ~S~:@_~S"
                                                 ',name
                                                 (subseq ',spec+
                                                         (mod (length ,args)
                                                              ,length))
                                                 ',spec+ ,args)))
                            args)
               :do (multiple-value-call (resignaler ',name ,args) ,@forms)))))

(defun <+form> (name spec+)
  (let ((*form (<*form-body> name spec+)))
    `(,name (,name)
      (if (atom ,name)
          (syntax-error ',name "~A: Require CONS but ~S" ',name ,name)
          ,*form))))

;; <*FORM>

(defun <*form> (name spec+)
  `(,name (,name)
    (if (typep ,name '(and atom (not null)))
        (let ((*default-condition* 'violate-list))
          (syntax-error ',name "~A: Require LIST but ~S." ',name ,name))
        ,(<*form-body> name spec+))))

;;; <LOCAL-FUN>

(defun <local-fun> (clause)
  (destructuring-bind
      (var-spec . spec+)
      clause
    (let ((name (alexandria:ensure-car var-spec)))
      (case (extended-marker name)
        (#\+ (<+form> name spec+))
        (#\* (<*form> name spec+))
        (otherwise (<require-form> name spec+))))))

;;;; CHECK-BNF

(defmacro check-bnf (&whole whole (&key ((:whole whole?))) &body def+)
  ;; THIS IS THE WHAT WE WANT TO WRITE.
  #++
  (check-bnf (:whole whole :name 'check-bnf)
    ((whole (or null expression)))
    ((def+ (clause+))
     (clause (var-spec spec+))
     (var-spec (or bnf-name alias))
     (bnf-name symbol)
     (alias (bnf-name var-name))
     (var-name symbol)
     (spec+ (or type-specifier bnf-name or-form spec+))
     (or-form ((eql or) spec+))))
  #+(not :check-bnf)
  (return-from check-bnf nil)
  ;; THIS IS THE WHAT WE WANT TO GENERATE.
  (labels ((def+ (def+)
             (if (null def+)
                 (syntax-error 'def+ "Require at least one, but null")
                 (loop :for (var-spec . spec+) :in (apply #'append def+)
                       :do (var-spec var-spec)
                           (spec+ spec+))))
           (var-spec (var-spec)
             (unless (typep var-spec
                            '(or symbol (cons symbol (cons symbol null))))
               (syntax-error 'var-spec "but ~S" var-spec)))
           (spec+ (spec+)
             (if (null spec+)
                 (syntax-error 'spec+ "Require at least one, but null"))))
    (let ((*whole* whole))
      (def+ def+)))
  ;; Body of CHECK-BNF.
  (let ((forms
         (loop :for def :in def+
               :for *bnf*
                    = (mapcar
                        (lambda (clause)
                          (cons (alexandria:ensure-car (car clause))
                                (cdr clause)))
                        def)
               :for (fun-name var-name) = (alexandria:ensure-list (caar def))
               :if (or (and (cddar def) (not (every #'t-p (cdar def))))
                       (find (extended-marker fun-name) "*+")
                       (and (null (cddar def)) (not (t-p (cadar def)))))
                 :collect `(let ((*whole* ,whole?) (*bnf* ',*bnf*))
                             (labels ,(loop :for clause :in def
                                            :collect (<local-fun> clause))
                               (,fun-name ,(or var-name fun-name)))))))
    (typecase forms
      (null forms)
      ((cons * null) (car forms))
      (otherwise `(progn ,@forms)))))

(declaim
 (ftype (function (symbol function) (values function &optional)) *-checker))

(defun *-checker (name cont)
  (lambda (arg)
    (if (typep arg '(and atom (not null)))
        (let ((*default-condition* 'violate-list))
          (syntax-error name "~A: Require LIST but ~S." name arg))
        (loop :for rest :on arg
              :do (handler-case (funcall cont (car rest))
                    (syntax-error (c)
                      (let ((*default-condition* 'may-syntax-error))
                        (apply #'syntax-error name
                               (concatenate 'string
                                            (simple-condition-format-control c)
                                            "~@?")
                               (append (simple-condition-format-arguments c)
                                       (list "~:@_in ~S" rest))))))))))

(defun +-checker (name cont)
  (declare (type function cont))
  (lambda (arg)
    (if (atom arg)
        (syntax-error name "~A: Require CONS but ~S" name arg)
        (handler-case (mapc cont arg)
          (syntax-error (c)
            (apply #'syntax-error name
                   (concatenate 'string (simple-condition-format-control c)
                                "~@?")
                   (append (simple-condition-format-arguments c)
                           (list "~:@_in ~S" arg))))
          (:no-error (&rest args)
            (declare (ignore args))
            nil)))))

;;;; PRETY-PRINTER.

(defun pprint-check-bnf (stream exp)
  (setf stream (or stream *standard-output*))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~:<" ; logical-block for check-bnf
                      "~W~^ ~1I~@_" ; operator
                      (list "~:<" ; options
                            "~@{~W~^ ~@_~}" ; option body.
                            "~:>~^ ~_")
                      (list "~@{" ; def+
                            (list "~:<~^" ; each definition
                                  (list "~@{" ; clauses
                                        (list "~:<~^" ; each clause.
                                              "~@{~W~^ ~:_~}" ; clause body.
                                              "~:>~^ ~:@_")
                                        "~}")
                                  "~:>~^~:@_")
                            "~}")
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (eql check-bnf)) 'pprint-check-bnf)

;;;; DSL

(defmacro defbnf (definition)
  (let ((fun-name (caar definition))
        (*bnf* definition)
        (function (gensym "FUNCTION")))
    #+(not :check-bnf)
    (return-from defbnf
      `(defun ,fun-name (,fun-name) (declare (ignore ,fun-name)) nil))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,function
              (lambda (,fun-name)
                ,(if (or (and (cddar definition)
                              (not (every #'t-p (cdar definition))))
                         (find (extended-marker fun-name) "*+")
                         (and (null (cddar definition))
                              (not (t-p (cadar definition)))))
                     `(let ((*bnf* ',*bnf*))
                        (labels ,(mapcar #'<local-fun> definition)
                          (,fun-name ,fun-name)))
                     nil))))
         (setf (symbol-function ',fun-name)
                 (make-instance 'checker
                                :definitions ',definition
                                :function ,function))
         ',fun-name))))

;;;; PRETTY-PRINTER

(defun pprint-defbnf (stream exp)
  (funcall
    (formatter
     #.(concatenate 'string "~:<" ; pprint-logobal-block.
                    "~W ~1I~:_" ; operator.
                    "~@{~:/pprint-linear/~^ ~_~}" ; form.
                    "~:>"))
    stream exp))

(set-pprint-dispatch '(cons (member defbnf)) 'pprint-defbnf)