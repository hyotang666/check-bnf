; vim: ft=lisp et
(in-package :asdf)
(defsystem "check-bnf.test"
  :version
  "0.2.13"
  :depends-on
  (:jingoh "check-bnf")
  :components
  ((:file "check-bnf"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :check-bnf args)))
