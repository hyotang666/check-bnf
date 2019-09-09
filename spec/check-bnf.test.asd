; vim: ft=lisp et
(in-package :asdf)
(defsystem "check-bnf.test"
  :version
  "0.2.16"
  :depends-on
  (:jingoh "check-bnf")
  :components
  ((:file "check-bnf"))
  :perform
  (test-op (o c) (declare (special args))
    (if(fboundp(find-symbol "VARIABLE-INFORMATION" "CLTL2"))
      (apply #'symbol-call :jingoh :examine :check-bnf args)
      (warn "CHECK-BNF does not work in ~A, due to TRIVIAL-CLTL2 does not support."
            (lisp-implementation-type)))))
