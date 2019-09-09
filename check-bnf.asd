; vim: ft=lisp et
(in-package :asdf)
(defsystem "check-bnf"
  :version
  "5.9.2"
  :depends-on
  (
   "millet" ; Wrapper for implementation dependent tiny utilities.
   "alexandria" ; Public domain utilities.
   "trivial-cltl2" ; Wrapper for cltl2.
   "matrix-case" ; Control flow.
   )
  :pathname
  "src/"
  :components
  ((:file "check-bnf")))

(defmethod operate :after ((o load-op)(c (eql (find-system "check-bnf")))&key)
  (unless(fboundp(find-symbol "VARIABLE-INFORMATION" "CLTL2"))
    (warn "CHECK-BNF does not work in ~A due to TRIVIAL-CLTL2 does not support."
          (lisp-implementation-type))))

;; These forms below are added by JINGOH.GENERATOR.
(in-package :asdf)
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "check-bnf"))))
  (append (call-next-method) '((test-op "check-bnf.test"))))
(defmethod operate :around
           ((o test-op) (c (eql (find-system "check-bnf")))
            &rest keys
            &key ((:compile-print *compile-print*))
            ((:compile-verbose *compile-verbose*)) &allow-other-keys)
  (flet ((jingoh.args (keys)
           (loop :for (key value) :on keys :by #'cddr
                 :when (find key '(:on-fails :subject :vivid) :test #'eq)
                 :collect key
                 :and
                 :collect value :else
                 :when (eq :jingoh.verbose key)
                 :collect :verbose
                 :and
                 :collect value)))
    (let ((args (jingoh.args keys)))
      (declare (special args))
      (call-next-method))))
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system
             (not(uiop:featurep :clisp)))
    (load-system system)
    (defmethod operate :around
               ((o load-op) (c (eql (find-system "check-bnf"))) &key)
      (let* ((seen nil)
             (*default-pathname-defaults*
              (merge-pathnames "spec/" (system-source-directory c)))
             (*macroexpand-hook*
              (let ((outer-hook *macroexpand-hook*))
                (lambda (expander form env)
                  (if (not (typep form '(cons (eql defpackage) *)))
                      (funcall outer-hook expander form env)
                      (if (find (cadr form) seen :test #'string=)
                          (funcall outer-hook expander form env)
                          (progn
                           (push (cadr form) seen)
                           `(progn
                             ,form
                             ,@(symbol-call :jingoh.documentizer :importer
                                            form)))))))))
        (call-next-method)))))
