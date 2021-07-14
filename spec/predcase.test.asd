; vim: ft=lisp et
(in-package :asdf)
(defsystem :predcase.test
  :version "0.0.1"
  :depends-on
  (:jingoh "predcase")
  :components
  ((:file "predcase"))
  :perform
  (test-op (o c) (symbol-call :jingoh :examine :predcase)))
