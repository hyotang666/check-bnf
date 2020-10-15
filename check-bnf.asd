; vim: ft=lisp et
(in-package :asdf)
(defsystem "check-bnf"
  :version
  "7.5.0"
  :description "Macro arguments checker."
  :author "SATO Shinichi"
  :license "MIT"
  :depends-on
  (
   "millet" ; Wrapper for implementation dependent tiny utilities.
   "alexandria" ; Public domain utilities.
   "matrix-case" ; Control flow.
   )
  :pathname
  "src/"
  :components
  ((:file "check-bnf")))

;;; These forms below are added by JINGOH.GENERATOR.
;; Ensure in ASDF for pretty printings.
(in-package :asdf)
;; Enable testing via (asdf:test-system "check-bnf").
(defmethod component-depends-on
           ((o test-op) (c (eql (find-system "check-bnf"))))
  (append (call-next-method) '((test-op "check-bnf.test"))))
;; Enable passing parameter for JINGOH:EXAMINER via ASDF:TEST-SYSTEM.
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
;; Enable importing spec documentations.
(let ((system (find-system "jingoh.documentizer" nil)))
  (when (and system (not (featurep :clisp)))
    (load-system system)
    (defmethod perform :after
               ((o load-op) (c (eql (find-system "check-bnf"))))
      (symbol-call :jingoh.documentizer :import c))))
