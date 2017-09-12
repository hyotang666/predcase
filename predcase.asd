; vim: ft=lisp et
(in-package :asdf)

(defsystem :predcase
  :author "Shinich Sato"
  :license "Public Domain"
  :components((:file "predcase")))
;; Perform method below is added by JINGOH.GENERATOR.
(defmethod perform ((o test-op) (c (eql (find-system "predcase"))))
  (test-system :predcase.test))
