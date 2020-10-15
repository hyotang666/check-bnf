(in-package :cl-user)

(defpackage :check-bnf
  (:use :cl)
  (:export ;;;; Main API
           #:check-bnf
           #:doc
           ;;;; Condition
           #:syntax-error
           ;;;; Miscellaneous TYPE.
           #:expression))

(in-package :check-bnf)

;;;; TYPES

(deftype type-specifier () '(or symbol cons))

(deftype expression () t)

;;;; SPECIAL VARIABLES

(defvar *whole* nil)

(defvar *bnf* nil)

;;;; CONDITION

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
  (error *default-condition*
         :name name
         :format-control format-control
         :format-arguments format-arguments
         :whole *whole*
         :definitions *bnf*))

;;;; UTILITIES

(defun cons-equal (list1 list2) (tree-equal list1 list2 :test (constantly t)))

(defun ignored (arg) (declare (ignore arg)) nil)

(defun extended-marker (name)
  (let ((char (char (symbol-name name) (1- (length (symbol-name name))))))
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

(defmacro capture-syntax-error (form)
  `(handler-case ,form
     (syntax-error (c)
       c)
     (:no-error (&rest args)
       (declare (ignore args))
       nil)))

;;;; SPEC-INFER

(defun t-p (thing &optional (*bnf* *bnf*))
  (let ((seen))
    (labels ((rec (thing)
               (let ((spec (assoc thing *bnf*)))
                 (if spec
                     (if (find (car spec) seen)
                         nil
                         (progn
                          (push (car spec) seen)
                          (every #'rec (cdr spec))))
                     (cond
                       ((millet:type-specifier-p thing)
                        (if (typep thing '(cons (eql or) *))
                            (some #'rec (cdr thing))
                            (eq t (millet:type-expand thing))))
                       ((atom thing)
                        (multiple-value-bind (but mark)
                            (but-extended-marker thing)
                          (if (null (assoc but *bnf*))
                              (error "NIY")
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
                    (list "~{" ; iterate.
                          "~/check-bnf:pprint-def-elt/~^ " ; each elt.
                          "~}"))))
        stream exp)))

(defun definitions (thing bnf)
  (let ((acc))
    (labels ((rec (thing)
               (let ((definition
                      (or (assoc thing bnf)
                          (assoc (but-extended-marker thing) bnf))))
                 (if definition
                     (progn
                      (pushnew definition acc :key #'car)
                      (body (cdr definition)))
                     (typecase thing (cons (body thing))))))
             (body (list)
               (dolist (spec list)
                 (when (and (not (eq spec thing)) (not (assoc spec acc)))
                   (rec spec)))))
      (rec thing))
    (nreverse acc)))

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

;; <REQUIRE-FORM>

(defun <require-form> (name spec+)
  (if (cdr spec+)
      (error "BNF definition error: ~S must have + or * its name at last."
             name)
      (let ((form (<local-check-form> name name (car spec+))))
        `(,name (,name)
          ,@(if form
                (list form)
                `((declare (ignore ,name)) nil))))))

;; <*FORM>

(defun <*form> (name spec+)
  `(,name (,name)
    (if (typep ,name '(and atom (not null)))
        (let ((*default-condition* 'violate-list))
          (syntax-error ',name "Require LIST but ~S." ,name))
        ,(<*form-body> name spec+))))

(defun check-length (actual spec name)
  (let ((mod (mod (length actual) (length spec))))
    (unless (zerop mod)
      (restart-case (let ((*default-condition* 'length-mismatch))
                      (syntax-error name
                                    "Length mismatch. Lack last ~{~S~^ ~} of ~S~:@_~S"
                                    (subseq spec mod) spec actual))
        (continue ())))))

(defun resignaler (name actual-args)
  (lambda (&rest conditions)
    (when (find-if (lambda (x) (typep x 'syntax-error)) conditions)
      (let ((*default-condition*
             (if (null (car conditions))
                 'syntax-error
                 'may-syntax-error)))
        (syntax-error name "~{~?~^ ~}~@?"
                      (loop :for c :in conditions
                            :when c
                              :collect (simple-condition-format-control c)
                              :and :collect (simple-condition-format-arguments
                                              c))
                      "~2I~:@_in ~S~I" actual-args)))))

(defun <*form-body> (name spec+)
  (let* ((length (length spec+))
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
        `(progn
          ,@(when (< 1 length)
              `((check-length ,name ',spec+ ',name)))
          (loop :for ,args :on ,name
                     :by ,(let ((length (length spec+)))
                            (case length
                              (1 '#'cdr)
                              (2 '#'cddr)
                              (3 '#'cdddr)
                              (4 '#'cddddr)
                              (otherwise
                               `(lambda (list) (nthcdr ,length list)))))
                :for ,gsyms := ,args
                :do (multiple-value-call (resignaler ',name ,args) ,@forms))))))

;; <LOCAL-CHECK-FORM>

(defun <local-check-form> (name var spec)
  (cond
    ((millet:type-specifier-p spec) (<local-type-check-form> name var spec))
    ((atom spec) (<local-atom-check-form> name var spec))
    ((typep spec '(cons (eql or) *)) (<local-or-check-form> name var spec))
    ((consp spec) (<local-cons-check-form> name var spec))))

(defun <check-type-form> (name var type-specifier)
  `(unless (typep ,var ',type-specifier)
     (syntax-error ',name "but ~S, it is type-of ~S" ,var (type-of ,var))))

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

(defun <local-or-check-form> (name var spec)
  (if (t-p spec)
      nil
      `(tagbody
         (or ,@(maplist
                 (lambda (forms)
                   `(handler-case ,(car forms)
                      (syntax-error ()
                        ,(if (cdr forms)
                             nil
                             `(syntax-error ',name "but ~S" ,var)))
                      (:no-error (&rest args)
                        (declare (ignore args))
                        t)))
                 (mapcar (lambda (spec) (<local-check-form> name var spec))
                         (cdr spec)))))))

;;;; SPEC the intermediate object.

(defstruct (spec (:constructor spec (name checker)))
  (name (error "Name is Required for spec.") :type symbol :read-only t)
  (checker (error "Checker is Required for spec.") :type function :read-only t))

(defun check-cons (actual-args specs name)
  (do ((actual actual-args (cdr actual))
       (specs specs (cdr specs)))
      ((or (atom actual) (atom specs))
       (matrix-case:matrix-typecase (actual specs)
         ((null null))
         ((atom atom) (local-check actual specs))
         ((null (cons * null))
          (let ((elt (car specs)))
            (if (spec-p elt)
                (case (extended-marker (spec-name elt))
                  ((#\? #\*)
                   (handler-case (local-check actual elt) (syntax-error ())))
                  (otherwise
                   (let ((*default-condition* 'length-mismatch))
                     (syntax-error name
                                   "Length mismatch. Lack last ~{~S~^ ~} of ~S"
                                   (mapcar
                                     (lambda (x)
                                       (if (spec-p x)
                                           (spec-name x)
                                           x))
                                     specs)
                                   name))))
                (case (extended-marker (car specs))
                  ((#\? #\*) (local-check actual (car specs)))
                  (otherwise
                   (let ((*default-condition* 'length-mismatch))
                     (syntax-error name
                                   "Length mismatch. Lack last ~{~S~^ ~} of ~S"
                                   specs name)))))))
         ((atom (cons * null))
          (syntax-error name "Require CONS but ~S" actual))
         (otherwise
          (let ((*default-condition* 'length-mismatch))
            (syntax-error name "Length mismatch. ~S but ~S" name
                          actual-args)))))
    (let ((elt (car specs)))
      (if (spec-p elt)
          (case (extended-marker (spec-name elt))
            (#\?
             (handler-case (local-check (car actual) elt)
               (syntax-error (c)
                 (if (cdr specs)
                     (push "dummy" actual)
                     (error c)))))
            ((#\*)
             (handler-case
                 (if (cdr specs)
                     (handler-bind ((length-mismatch #'continue))
                       (local-check actual elt))
                     (local-check actual elt))
               (may-syntax-error (condition)
                 (if (cdr specs)
                     (setf actual
                             (cons "dummy"
                                   (car
                                     (last
                                       (simple-condition-format-arguments
                                         condition)))))
                     (error condition)))
               (:no-error (&rest _)
                 (declare (ignore _))
                 (setf actual nil))))
            ((#\+) (local-check actual elt) (setf actual nil))
            (otherwise (local-check (car actual) elt)))
          (local-check (car actual) elt)))))

(defun <local-cons-check-form> (name var spec)
  (if (t-p spec)
      `(unless (cons-equal ,var ',spec)
         (let ((*default-condition* 'length-mismatch))
           (syntax-error ',spec "Length mismatch. ~S but ~S" ',spec ,var)))
      `(check-cons ,var ,(<spec-form> spec name) ',spec)))

(defun <spec-form> (spec name)
  (cond ((null spec) nil)
        ((millet:type-specifier-p spec) `',spec)
        ((atom spec)
         (multiple-value-bind (but mark)
             (but-extended-marker spec)
           `(spec ',spec
                  ,(if (and (find mark "+*?") (assoc but *bnf*))
                       (ecase mark
                         (#\+ `(+-checker ',name #',but))
                         (#\* `(*-checker ',name #',but))
                         (#\? `#',but))
                       `#',spec))))
        ((typep spec '(cons (eql or) *)) (error "NIY"))
        ((consp spec)
         `(cons ,(<spec-form> (car spec) name)
                ,(<spec-form> (cdr spec) name)))))

(declaim
 (ftype (function (t t)
         (values null ; or signals an error.
                 &optional))
        local-check))

(defun local-check (name spec)
  (cond
    ((millet:type-specifier-p spec)
     (unless (eql t (millet:type-expand spec))
       (unless (typep name spec)
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
    ((consp spec)
     (do* ((spec spec (cdr spec))
           (value name (cdr value)))
          ((or (atom spec) (atom value))
           (matrix-case:matrix-typecase (spec value)
             ((null null))
             ((atom atom) (local-check value spec))
             (otherwise
              (let ((*default-condition* 'length-mismatch))
                (syntax-error spec "Length mismatch. ~S but ~S" spec name)))))
       (let ((elt (car spec)))
         (if (and (symbolp elt) (eql #\? (extended-marker elt)))
             (handler-case (local-check (car value) elt)
               (error ()
                 (push "dummy" value))) ; as rewind.
             (local-check (car value) elt)))))))

;; <+FORM>

(defun <+form> (name spec+)
  (let ((*form (<*form-body> name spec+)))
    `(,name (,name)
      (if (atom ,name)
          (syntax-error ',name "Require CONS but ~S" ,name)
          ,*form))))

(defun *-checker (name cont)
  (lambda (arg)
    (if (typep arg '(and atom (not null)))
        (let ((*default-condition* 'violate-list))
          (syntax-error name "Require LIST but ~S." arg))
        (handler-case (mapc cont arg)
          (syntax-error (c)
            (syntax-error name
                          (concatenate 'string
                                       (simple-condition-format-control c)
                                       "~@?")
                          (simple-condition-format-arguments c) "~:@_in ~S"
                          arg))
          (:no-error (&rest args)
            (declare (ignore args))
            nil)))))

(defun +-checker (name cont)
  (lambda (arg)
    (if (atom arg)
        (syntax-error name "Require CONS but ~S" arg)
        (handler-case (mapc cont arg)
          (syntax-error (c)
            (syntax-error name
                          (concatenate 'string
                                       (simple-condition-format-control c)
                                       "~@?")
                          (simple-condition-format-arguments c) "~:@_in ~S"
                          arg))
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
