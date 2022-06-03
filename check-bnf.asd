; vim: ft=lisp et
(in-package :asdf)
(defsystem "check-bnf"
  :version
  "8.1.30"
  :description "Macro arguments checker."
  :author "SATO Shinichi"
  :license "MIT"
  :source-control (:git "git@github.com:hyotang666/check-bnf")
  :bug-tracker "https://github.com/hyotang666/check-bnf/issues"
  :depends-on
  (
   "alexandria"         ; Utilities.
   "millet"             ; Wrapper for implementation dependent tiny utilities.
   "closer-mop"         ; Wrapper for meta-object-protocols.
   "matrix-case"        ; Macros for control flow.
   )
  :pathname
  "src/"
  :components
  ((:file "check-bnf")
   (:file "defbnf" :depends-on ("check-bnf"))))

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
