(in-package :cl-user)
(defpackage :check-bnf
  (:use :cl)
  (:export
    ;;;; Main API
    #:check-bnf

    ;;;; Miscellaneous TYPE.
    #:expression
    ))
(in-package :check-bnf)

(define-condition syntax-error(program-error simple-error)
  ()
  (:report (lambda(condition stream)
	     (format stream
		     "~@[Syntax-error in ~S~%~]~?~@[~%in ~S~]"
		     *name*
		     (simple-condition-format-control condition)
		     (simple-condition-format-arguments condition)
		     *whole*))))

(defun syntax-error(format-control &rest format-arguments)
  (error 'syntax-error
	 :format-control format-control
	 :format-arguments format-arguments))

(deftype type-specifier()t)
(deftype expression(&key eval)
  (declare(ignore eval))
  t)

(defvar *whole* nil)
(defvar *name* nil)

(defmacro check-bnf(&whole whole
			   &environment env
			   (&key ((:whole whole?))
				 ((:name name?)))
			   &rest clause+)
  ;; THIS IS THE WHAT WE WANT TO WRITE.
  #++(check-bnf(:whole whole :name 'check-bnf)
       (whole? (expression :eval t))
       (name? (expression :eval t))
       (clause+ (var-spec spec+))
       (var-spec (or name alias))
       (name symbol)
       (alias (name name))
       (spec+ type-specifier))

  ;; THIS IS THE WHAT WE WANT TO GENERATE.
  (labels((clause+(clause+)
	    (if(null clause+)
	      (syntax-error "Require at least one, but null")
	      (loop :for (var-spec . spec+) :in clause+
		    :do (var-spec var-spec)
		    (spec+ spec+))))
	  (var-spec(var-spec)
	    (unless(typep var-spec '(or symbol
					(cons symbol (cons symbol null))))
	      (syntax-error "var-spec := [ name | (name var) ]~%~
			    name := SYMBOL~%~
			    var := SYMBOL~%but ~S"
			    var-spec)))
	  (spec+(spec+)
	    (if(null spec+)
	      (syntax-error "Require at least one, but null"))))
    (let((*whole* whole)
	 (*name* 'check-bnf))
      (clause+ clause+)))

  ;; Body of CHECK-BNF.
  `(labels,(loop :for clause :in clause+
		 :collect (<local-fun> clause))
     (let((*name* ,name?)
	  (*whole* ,whole?))
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
	   `((declare(ignore(,name))))
	   `((when ,name
	       ,(<check-type-form> name name(car spec+))))))))

(defun <check-type-form>(name var type-specifier)
  `(unless(typep ,var ',type-specifier)
     (syntax-error "~A := ~A~%but ~S, it is type-of ~S"
		   ',name ',type-specifier
		   ,var (type-of ,var))))

(defun <*form>(name spec+)
  `(,name(,name)
     (if(typep ,name '(and atom (not null)))
       (syntax-error "~S := ~{~S~^ ~}~%Require LIST but ~S,."
		     ',name ',spec+ ,name)
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
	     `((unless(zerop(mod (length ,name),length))
		 (syntax-error "Length mismatch"))))
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
			 :do (syntax-error "~S := ~{~S~^ ~}~%~
					   but ~{~S~*~^ ~}~%~?"
					   ',name ',spec+ args
					   "~2*~S is ~S, not ~3:* ~S"
					   (simple-condition-format-arguments c))))
		 ,@forms))))))

(defun <local-check-form>(name var spec &optional fun)
  (cond
    ((millet:type-specifier-p spec)
     (unless(eql t (millet:type-expand spec))
       (<check-type-form> name var spec)))
    ((atom spec)
     (if fun
       `(,fun ,spec ,name)
       `(,spec ,name)))
    ((typep spec '(cons (eql or)*))
     `(or ,@(maplist (lambda(spec+)
		       (if(cdr spec+)
			 `(ignore-errors ,(<local-check-form> name var (car spec+)))
			 `(handler-case,(<local-check-form> name var (car spec+))
			    (syntax-error()
			      (syntax-error "~S := ~S but not exhausted. ~S"
					    ',name ',spec ,var)))))
		     (cdr spec))))
    ((consp spec)
     `(mapc #'local-check ,var ',spec))))

(defun local-check(name spec)
  (cond
    ((millet:type-specifier-p spec)
     (unless(eql t (millet:type-expand spec))
       (unless(typep name spec)
	 (syntax-error "~A := ~A~%but ~S, it is type-of ~S"
		       name spec name (type-of name)))))
    ((atom spec)
     (funcall spec name))
    ((typep spec '(cons (eql or)*))
     (loop :for (spec+ . rest) :on (cdr spec)
	   :if (null rest)
	   :do (handler-case(local-check name spec+)
		 (syntax-error()
		   (syntax-error "~S := ~S but not exhausted. ~S"
				 name spec spec+)))
	   :else :do (ignore-errors (local-check name spec+))))
    ((consp spec)
     (mapc #'local-check name spec))))

(defun <+form>(name spec+)
  (let((*form
	 (<*form-body> name spec+)))
    `(,name(,name)
       (if(atom ,name)
	 (syntax-error "~S := ~{~S~^ ~}~%Require CONS but ~S"
		       ',name ',spec+ ,name)
	 ,(if(typep *form '(cons (eql declare)*))
	    nil
	    *form)))))
