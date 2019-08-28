(in-package :cl-user)
(defpackage :check-bnf
  (:use :cl)
  (:export
    ;;;; Main API
    #:check-bnf

    ;;;; Condition
    #:syntax-error

    ;;;; Miscellaneous TYPE.
    #:expression
    ))
(in-package :check-bnf)

(define-condition syntax-error(program-error simple-error cell-error)
  ((whole :initform nil
	  :initarg :whole
	  :reader whole-form<=syntax-error)
   (definitions :initform nil
		:initarg :definitions
		:reader bnf-definitions))
  (:report (lambda(condition stream)
	     (format stream
		     "~@[Syntax-error in ~S~%~]~A~?~@[~%in ~S~]"
		     (car(whole-form<=syntax-error condition))
		     (format-definition
		       (definitions (cell-error-name condition)
				    (bnf-definitions condition)))
		     (simple-condition-format-control condition)
		     (simple-condition-format-arguments condition)
		     (whole-form<=syntax-error condition)))))

(defun definitions(thing bnf)
  (let((acc))
    (labels((rec(thing)
	      (let((definition
		     (assoc thing bnf)))
		(if definition
		  (progn (pushnew definition acc :key #'car)
			 (body (cdr definition)))
		  (typecase thing
		    (cons
		      (body thing))))))
	    (body(list)
	      (dolist(spec list)
		(when(and (not(eq spec thing))
			  (not(assoc spec acc)))
		  (rec spec)))))
      (rec thing))
    (nreverse acc)))

(defun format-definition(definitions)
  (format nil "~:{~A := ~:[[ ~{~A~^ ~} ]~;~{~A~}~]~@[~A~]~%~}"
	  (mapcar (lambda(definition)
		    (multiple-value-bind(name mark)(but-extended-marker
						     (car definition))
		      (let((list
			     (mapcar #'or-formatter (cdr definition))))
			(list name
			      (and list (null(cdr list))) ; one-element-p
			      list
			      mark))))
		  definitions)))

(defun but-extended-marker(thing)
  (and (symbolp thing)
       (let((mark
	      (extended-marker thing)))
	 (case mark
	   ((#\? #\* #\+)
	    (let((name
		   (symbol-name thing)))
	      (values (subseq name 0 (1- (length name)))
		      mark)))
	   (otherwise
	     (values (symbol-name thing)
		     nil))))))

(defun syntax-error(name format-control &rest format-arguments)
  (error 'syntax-error
	 :name name
	 :format-control format-control
	 :format-arguments format-arguments
	 :whole *whole*
	 :definitions *bnf*))

(deftype type-specifier()t)
(deftype expression()
  t)

(defvar *whole* nil)
(defvar *bnf* nil)

(defmacro check-bnf(&whole whole
			   &environment env
			   (&key ((:whole whole?)))
			   &rest clause+)
  ;; THIS IS THE WHAT WE WANT TO WRITE.
  #++(check-bnf(:whole whole :name 'check-bnf)
       (whole (or null expression))
       (name (or null expression))
       (clause+ (var-spec spec+))
       (var-spec (or bnf-name alias))
       (bnf-name symbol)
       (alias (bnf-name var-name))
       (var-name symbol)
       (or-form ((eql or)spec+))
       (spec+ (or type-specifier bnf-name or-form spec+))
       )

  ;; THIS IS THE WHAT WE WANT TO GENERATE.
  (labels((clause+(clause+)
	    (if(null clause+)
	      (syntax-error 'clause+ "Require at least one, but null")
	      (loop :for (var-spec . spec+) :in clause+
		    :do (var-spec var-spec)
		    (spec+ spec+))))
	  (var-spec(var-spec)
	    (unless(typep var-spec '(or symbol
					(cons symbol (cons symbol null))))
	      (syntax-error 'var-spec "but ~S" var-spec)))
	  (spec+(spec+)
	    (if(null spec+)
	      (syntax-error 'spec+ "Require at least one, but null"))))
    (let((*whole* whole))
      (clause+ clause+)))

  ;; Body of CHECK-BNF.
  `(labels,(loop :for clause :in clause+
		 :collect (<local-fun> clause))
     (let((*whole* ,whole?)
	  (*bnf* ',clause+))
       ,@(loop :for (var-spec) :in clause+
	       :for (name . var) := (alexandria:ensure-list var-spec)
	       :when (eq :lexical (cltl2:variable-information name env))
	       :collect `(,name ,(or (car var)
				     name))))))

(defun <local-fun>(clause)
  (destructuring-bind(var-spec . spec+)clause
    (let((name
	   (alexandria:ensure-car var-spec)))
      (case(extended-marker name)
	(#\+ (<+form> name spec+))
	(#\* (<*form> name spec+))
	(#\? (<?form> name spec+))
	(otherwise
	  (<require-form> name spec+))))))

(defun extended-marker(name)
  (let((char
	 (char (symbol-name name)
	       (1-(length(symbol-name name))))))
    (find char "+*?")))

(defun <require-form>(name spec+)
  (if(cdr spec+)
    (error "BNF definition error: ~S must have + or * its name at last."
	   name)
    `(,name(,name)
       ,(<local-check-form> name name (car spec+)))))

(defun <?form>(name spec+)
  (if(cdr spec+)
    (error "BNF definition error: ~S must have + or * its name at last."
	   name)
    `(,name(,name)
       ,@(if(eql t (millet:type-expand (car spec+)))
	   `((declare(ignore(,name)))T)
	   `((typep ,name ',(car spec+)))))))

(defun <check-type-form>(name var type-specifier)
  `(unless(typep ,var ',type-specifier)
     (syntax-error ',name "but ~S, it is type-of ~S" ,var (type-of ,var))))

(defun <*form>(name spec+)
  `(,name(,name)
     (if(typep ,name '(and atom (not null)))
       (syntax-error ',name "Require LIST but ~S." ,name)
       ,(<*form-body> name spec+))))

(defun <*form-body>(name spec+)
  (let*((length
	  (length spec+))
	(gsyms
	  (alexandria:make-gensym-list length))
	(forms
	  (loop :for g :in gsyms
		:for spec :in spec+
		:for form = (<local-check-form> name g spec)
		:when form
		:collect `(handler-case,form
			    (syntax-error(c)
			      (values ,g c))
			    (:no-error(&rest args)
			      (declare(ignore args))
			      (values ,g nil)))))
	)
    (if(null forms)
      `(declare(ignore ,name))
      `(progn
	 ,@(when(< 1 length)
	     `((let((mod
		      (mod (length ,name),length)))
		 (unless(zerop mod)
		   (syntax-error ',name
				 "Length mismatch. Lack last ~{~S~^ ~} of ~S~@?"
				 (subseq ',spec+ mod)
				 ',spec+
				 "~%~S"
				 ,name
				 )))))
	 (loop :for ,gsyms :on ,name
	       :by ,(let((length(length spec+)))
		      (case length
			(1 '#'cdr)
			(2 '#'cddr)
			(3 '#'cdddr)
			(4 '#'cddddr)
			(otherwise `(lambda(list)
				      (nthcdr ,length list)))))
	       :do
	       (multiple-value-call
		 (lambda(&rest args)
		   (loop :for (nil c) :on args :by #'cddr
			 :when c
			 :do (syntax-error
			       ',name
			       "but ~{~S~*~^ ~}~@?"
			       args
			       "~%in ~S"
			       ,name)))
		 ,@forms))))))

(defun <local-check-form>(name var spec &optional fun)
  (cond
    ((millet:type-specifier-p spec)
     (unless(eql t (millet:type-expand spec))
       (<check-type-form> name var spec)))
    ((atom spec)
     (if fun
       `(,fun ,spec ,var)
       `(,spec ,var)))
    ((typep spec '(cons (eql or)*))
     `(or ,@(mapcon (lambda(spec+)
		      (let((form
			     (<local-check-form> name var (car spec+))))
			(if(cdr spec+)
			  (when form
			    `((ignore-errors ,form)))
			  (when form
			    `((handler-case,form
				(syntax-error()
				  (syntax-error ',name "but ~S" ,var))))))))
		    (cdr spec))))
    ((consp spec)
     (alexandria:with-gensyms(vl sl elt)
       `(do*((,vl ,var (cdr ,vl))
	     (,sl ,(<spec-form> spec)
		  (cdr ,sl)))
	  ((or (atom ,vl)
	       (atom ,sl))
	   (trivia:match*(,vl ,sl)
	     ((nil nil))
	     (((type atom)(type atom))
	      (local-check ,vl ,sl))
	     ((_ _)
	      (syntax-error ',spec
			    "Length mismatch. ~S but ~S"
			    ',spec ,var))))
	  (let((,elt
		 (car ,sl)))
	    (if(functionp ,elt)
	      (case(extended-marker(millet:function-name ,elt))
		(#\?
		 (unless(local-check (car ,vl),elt)
		   (push "dummy" ,vl)))
		((#\+ #\*)
		 (local-check ,vl ,elt)
		 (setf ,vl nil))
		(otherwise
		  (local-check (car ,vl),elt)))
	      (local-check(car ,vl),elt))))))))

(defun <spec-form>(spec)
  (cond
    ((null spec)nil)
    ((millet:type-specifier-p spec)
     `',spec)
    ((atom spec)
     `#',spec)
    ((typep spec '(cons (eql or)*))
     (error "NIY"))
    ((consp spec)
     `(cons ,(<spec-form> (car spec))
	    ,(<spec-form> (cdr spec))))))

(defun local-check(name spec)
  (cond
    ((millet:type-specifier-p spec)
     (unless(eql t (millet:type-expand spec))
       (unless(typep name spec)
	 (syntax-error name
		       "but ~S, it is type-of ~S"
		       name (type-of name)))))
    ((atom spec)
     (funcall spec name))
    ((typep spec '(cons (eql or)*))
     (loop :for (spec+ . rest) :on (cdr spec)
	   :if (null rest)
	   :do (handler-case(local-check name spec+)
		 (syntax-error()
		   (syntax-error name
				 "~S := ~S but not exhausted. ~S"
				 name spec spec+)))
	   :else :do (ignore-errors (local-check name spec+))))
    ((consp spec)
     (do*((spec spec (cdr spec))
	  (value name (cdr value)))
       ((or (atom spec)
	    (atom value))
	(trivia:match*(spec value)
	  ((nil nil))
	  (((type atom)(type atom))
	   (local-check value spec))
	  ((_ _)
	   (syntax-error spec
			 "Length mismatch. ~S but ~S"
			 spec name))))
       (let((elt
	      (car spec)))
	 (if(and (symbolp elt)
		 (eql #\? (extended-marker elt)))
	   (unless(local-check(car value)elt)
	     (push "dummy" value)) ; as rewind.
	   (local-check (car value)elt)))))))


(defun <+form>(name spec+)
  (let((*form
	 (<*form-body> name spec+)))
    `(,name(,name)
       (if(atom ,name)
	 (syntax-error ',name "Require CONS but ~S" ,name)
	 ,(if(typep *form '(cons (eql declare)*))
	    nil
	    *form)))))

(defun or-formatter(form)
  (typecase form
    (atom
      (prin1-to-string form))
    ((cons (eql or) *)
     (format nil "[ ~{~A ~^| ~}]"
	     (mapcar #'or-formatter (cdr form))))
    (cons
      (format nil "(~{~A~^ ~})"
	      (mapcar #'or-formatter form)))))
