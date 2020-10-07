(in-package :cl-user)

(defpackage :check-bnf
  (:use :cl)
  (:export ;;;; Main API
           #:check-bnf
           ;;;; Condition
           #:syntax-error
           ;;;; Miscellaneous TYPE.
           #:expression))

(in-package :check-bnf)

;;;; SPECIAL VARIABLES

(defvar *whole* nil)

(defvar *bnf* nil)

;;;; CONDITION

(define-condition syntax-error (program-error simple-error cell-error)
  ((whole :initform nil :initarg :whole :reader whole-form<=syntax-error)
   (definitions :initform nil :initarg :definitions :reader bnf-definitions))
  (:report
   (lambda (condition stream)
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
             (whole-form<=syntax-error condition)))))
  (:default-initargs :format-control ""))

;;;; SIGNALER

(defun syntax-error (name format-control &rest format-arguments)
  (error 'syntax-error
         :name name
         :format-control format-control
         :format-arguments format-arguments
         :whole *whole*
         :definitions *bnf*))

;;;; FORMATTER

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

(defun pprint-definitions (stream definitions &rest noise)
  (declare (ignore noise))
  (when (typep definitions '(cons (eql check-bnf) *))
    (setf definitions
            (mapcar
              (lambda (def) (cons (alexandria:ensure-car (car def)) (cdr def)))
              (cddr definitions))))
  (funcall
    (formatter
     #.(apply #'concatenate 'string
              (alexandria:flatten
                (list "~<" ; pprint-logical-block
                      (list "~{" ; each line.
                            "~VA := ~:[[ ~{~A~^ ~} ]~;~{~A~}~]~@[~A~]~:@_"
                            "~}")
                      "~:>"))))
    stream
    (let ((num
           (reduce #'max definitions
                   :initial-value 0
                   :key (alexandria:compose 'length 'string 'car))))
      (mapcar
        (lambda (definition)
          (multiple-value-bind (name mark)
              (but-extended-marker (car definition))
            (let ((list (mapcar #'or-formatter (cdr definition))))
              (list num name (and list (null (cdr list))) ; one-element-p
                    list mark))))
        definitions))))

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

(defun or-formatter (form)
  (typecase form
    (atom (prin1-to-string form))
    ((cons (eql or) *)
     (format nil "[ ~{~A ~^| ~}]" (mapcar #'or-formatter (cdr form))))
    (cons (format nil "(~{~A~^ ~})" (mapcar #'or-formatter form)))))

(defun extended-marker (name)
  (let ((char (char (symbol-name name) (1- (length (symbol-name name))))))
    (find char "+*?")))

;;;; TYPES

(deftype type-specifier () t)

(deftype expression () t)

;;;; UTILITY

(defun cons-equal (list1 list2) (tree-equal list1 list2 :test (constantly t)))

(defun ignored (arg) (declare (ignore arg)) nil)

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
  `(let ((*whole* ,whole?))
     ,@(loop :for def :in def+
             :for *bnf*
                  = (mapcar
                      (lambda (clause)
                        (cons (alexandria:ensure-car (car clause))
                              (cdr clause)))
                      def)
             :for (name arg) = (alexandria:ensure-list (caar def))
             :collect `(let ((*bnf* ',*bnf*))
                         (labels ,(loop :for clause :in def
                                        :collect (<local-fun> clause))
                           (,name ,(or arg name)))))))

(defun <local-fun> (clause)
  (destructuring-bind
      (var-spec . spec+)
      clause
    (let ((name (alexandria:ensure-car var-spec)))
      (case (extended-marker name)
        (#\+ (<+form> name spec+))
        (#\* (<*form> name spec+))
        (otherwise (<require-form> name spec+))))))

(defun <require-form> (name spec+)
  (if (cdr spec+)
      (error "BNF definition error: ~S must have + or * its name at last."
             name)
      (let ((form (<local-check-form> name name (car spec+))))
        `(,name (,name)
          ,@(if form
                (list form)
                `((declare (ignore ,name)) nil))))))

(defun <check-type-form> (name var type-specifier)
  `(unless (typep ,var ',type-specifier)
     (syntax-error ',name "but ~S, it is type-of ~S" ,var (type-of ,var))))

(defun <*form> (name spec+)
  `(,name (,name)
    (if (typep ,name '(and atom (not null)))
        (syntax-error ',name "Require LIST but ~S." ,name)
        ,(<*form-body> name spec+))))

(defun <*form-body> (name spec+)
  (let* ((length (length spec+))
         (gsyms (alexandria:make-gensym-list length))
         (forms
          (loop :for g :in gsyms
                :for spec :in spec+
                :for form = (<local-check-form> name g spec)
                :when form
                  :collect `(handler-case ,form
                              (syntax-error (c)
                                (values ,g c))
                              (:no-error (&rest args)
                                (declare (ignore args)) (values ,g nil)))
                :else
                  :collect `(ignored ,g))))
    (if (null (remove 'ignored forms :key #'car))
        nil
        `(progn
          ,@(when (< 1 length)
              `((let ((mod (mod (length ,name) ,length)))
                  (unless (zerop mod)
                    (syntax-error ',name
                                  "Length mismatch. Lack last ~{~S~^ ~} of ~S~@?"
                                  (subseq ',spec+ mod) ',spec+ "~:@_~S"
                                  ,name)))))
          (loop :for ,gsyms :on ,name
                     :by ,(let ((length (length spec+)))
                            (case length
                              (1 '#'cdr)
                              (2 '#'cddr)
                              (3 '#'cdddr)
                              (4 '#'cddddr)
                              (otherwise
                               `(lambda (list) (nthcdr ,length list)))))
                :do (multiple-value-call
                        (lambda (&rest args)
                          (loop :for (nil c) :on args :by #'cddr
                                :when c
                                  :do (syntax-error ',name "but ~{~S~^ ~}~@?"
                                                    (loop :for arg :in args
                                                               :by #'cddr
                                                          :collect arg)
                                                    "~2I~:@_in ~S~I" ,name)))
                      ,@forms))))))

(defun <local-check-form> (name var spec)
  (cond
    ((millet:type-specifier-p spec) (<local-type-check-form> name var spec))
    ((atom spec) (<local-atom-check-form> name var spec))
    ((typep spec '(cons (eql or) *)) (<local-or-check-form> name var spec))
    ((consp spec) (<local-cons-check-form> name var spec))))

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
                        (declare (ignore args)) t)))
                 (mapcar (lambda (spec) (<local-check-form> name var spec))
                         (cdr spec)))))))

(defun <local-cons-check-form> (name var spec)
  (if (t-p spec)
      `(unless (cons-equal ,var ',spec)
         (syntax-error ',spec "Length mismatch. ~S but ~S" ',spec ,var))
      (alexandria:with-gensyms (vl sl elt)
        `(do* ((,vl ,var (cdr ,vl))
               (,sl ,(<spec-form> spec name) (cdr ,sl)))
              ((or (atom ,vl) (atom ,sl))
               (matrix-case:matrix-typecase (,vl ,sl)
                 ((null null))
                 ((atom atom) (local-check ,vl ,sl))
                 ((null (cons * null))
                  (let ((elt (car ,sl)))
                    (if (vectorp elt)
                        (case (extended-marker (elt elt 0))
                          (#\? (ignore-errors (local-check ,vl elt)))
                          (otherwise (local-check ,vl elt)))
                        (local-check ,vl (car ,sl)))))
                 (otherwise
                  (syntax-error ',spec "Length mismatch. ~S but ~S" ',spec
                                ,var))))
           (let ((,elt (car ,sl)))
             (if (vectorp ,elt)
                 (case (extended-marker (elt ,elt 0))
                   (#\?
                    (handler-case (local-check (car ,vl) ,elt)
                      (error ()
                        (push "dummy" ,vl))))
                   ((#\+ #\*) (local-check ,vl ,elt) (setf ,vl nil))
                   (otherwise (local-check (car ,vl) ,elt)))
                 (local-check (car ,vl) ,elt)))))))

(defun <spec-form> (spec name)
  (cond ((null spec) nil)
        ((millet:type-specifier-p spec) `',spec)
        ((atom spec)
         (multiple-value-bind (but mark)
             (but-extended-marker spec)
           `(vector ',spec
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
    ((vectorp spec) (funcall (elt spec 1) name))
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
              (syntax-error spec "Length mismatch. ~S but ~S" spec name))))
       (let ((elt (car spec)))
         (if (and (symbolp elt) (eql #\? (extended-marker elt)))
             (handler-case (local-check (car value) elt)
               (error ()
                 (push "dummy" value))) ; as rewind.
             (local-check (car value) elt)))))))

(defun <+form> (name spec+)
  (let ((*form (<*form-body> name spec+)))
    `(,name (,name)
      (if (atom ,name)
          (syntax-error ',name "Require CONS but ~S" ,name)
          ,*form))))

(defun *-checker (name cont)
  (lambda (arg)
    (if (typep arg '(and atom (not null)))
        (syntax-error name "Require LIST but ~S." arg)
        (handler-case (mapc cont arg)
          (syntax-error (c)
            (syntax-error name
                          (concatenate 'string
                                       (simple-condition-format-control c)
                                       "~@?")
                          (simple-condition-format-arguments c) "~:@_in ~S"
                          arg))
          (:no-error (&rest args)
            (declare (ignore args)) nil)))))

(defun +-checker (name cont)
  (lambda (arg)
    (if (atom arg)
        (syntax-error name "Require CONS but ~S" name)
        (handler-case (mapc cont arg)
          (syntax-error (c)
            (syntax-error name
                          (concatenate 'string
                                       (simple-condition-format-control c)
                                       "~@?")
                          (simple-condition-format-arguments c) "~:@_in ~S"
                          arg))
          (:no-error (&rest args)
            (declare (ignore args)) nil)))))

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

;;;; PRETY-PRINTER.

(defun pprint-check-bnf (stream exp)
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
                                              "~:>~^ ~_")
                                        "~}")
                                  "~:>~^~:@_")
                            "~}")
                      "~:>"))))
    stream exp))

(set-pprint-dispatch '(cons (eql check-bnf)) 'pprint-check-bnf)
