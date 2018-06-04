; vim: ft=lisp et
(in-package :asdf)

(defsystem :predcase
  :author "Shinich Sato"
  :license "Public Domain"
  :components((:file "predcase")))

(defmethod component-depends-on ((o test-op) (c (eql (find-system "predcase"))))
  (append (call-next-method)'((test-op "predcase.test"))))
