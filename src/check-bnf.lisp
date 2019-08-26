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
       (clause+ (var spec+))
       (var symbol)
       (spec+ type-spcifier))

  ;; THIS IS THE WHAT WE WANT TO GENERATE.
  (labels((clause+(clause+)
	    (if(null clause+)
	      (syntax-error 'check-bnf "Require at least one, but null")
	      (loop :for (var . spec+) :in clause+
		    :do (var var)
		    (spec+ spec+))))
	  (var(var)
	    (unless(typep var 'symbol)
	      (syntax-error 'check-bnf "var := SYMBOL, but ~S~%in ~S"
			    var whole)))
	  (spec+(spec+)
	    (if(null spec+)
	      (syntax-error 'check-bnf "Require at least one, but null")
	      (unless(every (lambda(elt)
			      (typep elt 'type-specifier))
			    spec+)
		(syntax-error 'check-bnf "spec := TYPE-SPECIFIER, but ~S~%in ~S"
			      spec+ whole)))))
    (let((*whole* whole)
	 (*name* 'check-bnf))
      (clause+ clause+)))

  ;; Body of CHECK-BNF.
  `(labels,(loop :for clause :in clause+
		 :collect (<local-fun> clause))
     (let((*name* ,name?)
	  (*whole* ,whole?))
       ,@(loop :for (name) :in clause+
	       :when (eq :lexical (cltl2:variable-information name env))
	       :collect `(,name ,name)))))

(defun <local-fun>(clause)
  (destructuring-bind(name . spec+)clause
    (case(char (symbol-name name)
	       (1-(length(symbol-name name))))
      (#\+ (<+form> name spec+))
      (#\* (<*form> name spec+))
      (#\? (<?form> name spec+))
      (otherwise
	(<default-form> name spec+)))))

(defun <default-form>(name spec+)
  (if(cdr spec+)
    (error "BNF definition error: ~S must have + or * its name at last."
	   name)
    `(,name(,name)
       ,(if(eql t (millet:type-expand (car spec+)))
	  `((declare(ignore ,name)))
	  (<check-type-form> name (car spec+))))))

(defun <?form>(name spec+)
  (if(cdr spec+)
    (error "BNF definition error: ~S must have + or * its name at last."
	   name)
    `(,name(,name)
       ,@(if(eql t (millet:type-expand (car spec+)))
	   `((declare(ignore(,name))))
	   `((when ,name
	       ,(<check-type-form> name (car spec+))))))))

(defun <check-type-form>(name type-specifier)
  `(unless(typep ,name ',type-specifier)
     (error "~A := ~A~%but ~S is ~S"
	    ',name ',type-specifier
	    ,name (type-of ,name))))

(defun <*form>(name spec+)
  `(,name(,name)
     ,(<*form-body> name spec+)))

(defun <*form-body>(name spec+)
  (let*((length
	  (length spec+))
	(gsyms
	  (alexandria:make-gensym-list length)))
    `(progn
       ,@(when(< 1 length)
	   `((unless(zerop(mod (length ,name),length))
	       (error "Length mismatch"))))
       (loop :for ,gsyms :on ,name
	     :by ,(let((length(length spec+)))
		    (case length
		      (1 ''cdr)
		      (2 ''cddr)
		      (3 ''cdddr)
		      (4 ''cddddr)
		      (otherwise `(lambda(list)
				    (nthcdr ,length list)))))
	     :do
	     ,@(loop :for g :in gsyms
		     :for elt :in spec+
		     :collect (<local-check-form> g elt))))))

(defun <local-check-form>(name elt &optional fun)
  (cond
    ((millet:type-specifier-p elt)
     (<check-type-form> name elt))
    ((atom elt)
     (if fun
       `(,fun ,elt ,name)
       `(,elt ,name)))
    ((consp elt)
     (alexandria:with-gensyms(a b)
       `(loop :for ,a :in ',elt
	      :for ,b :in ,name
	      :do ,(<local-check-form> b a 'funcall))))))

(defun <+form>(name spec+)
  `(,name(,name)
     (if(null ,name)
       (error "Required")
       ,(<*form-body> name spec+))))
