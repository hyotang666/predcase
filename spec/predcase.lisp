(defpackage :predcase.spec
  (:use :cl :jingoh :predcase))
(in-package :predcase.spec)
(setup :predcase)

(common-requirements-about (PREDCASE EPREDCASE)
                           :as predcase)

;;;; Description:
; diverge by predicate, evaluate only satisfied clause.
#?(predcase 0
    (symbolp :no)
    (stringp :not)
    (numberp :yes))
=> :YES

#+syntax
(PREDCASE arg &rest clause*) ; => result

;;;; Arguments and Values:

; arg := form generate key. (i.e. evaluated.)
; which applied to predicate.
#?(predcase nil
    (print :through)
    (princ :through)
    (identity :through)
    (not :yes))
:outputs "
NIL NIL"

; clause := (predicate-key &body body)
; predicate-key := predicate-specifier | default-key
; predicate-specifier := function-name | compound-predicate-specifier
; function-name := symbol | lambda-form
#?(predcase nil
    (null :yes)) ; <--- Using symbol as function name.
=> :YES
#?(predcase nil
    ((lambda (x) (null x)) :yes)) ; <--- Using lambda-form as function name.
=> :YES
; default-key := T | otherwise
#?(predcase nil
    (numberp :no)
    (t :yes)) ; <--- Using `T` as default-key.
=> :YES
#?(predcase nil
    (numberp :no)
    (otherwise :yes)) ; <--- Using `otherwise` as default-key.
=> :YES
; compound-predicate-specifier := (not predicate-specifier) | (and predicate-specifier*) | (or predicate-specifier*)
#?(predcase nil
    ((not identity) :yes))
=> :YES
#?(predcase 1
    ((or zerop oddp) :yes))
=> :YES
#?(predcase nil
    ((and listp atom) :yes))
=> :YES
#?(predcase 1
    ((or) :never)
    (t :yes))
=> :yes
#?(predcase 1
    ((and) :yes)
    (t :never))
=> :yes

; body := right-arrow function-name* | implicit-progn
; right-arrow := -> | =>
#?(predcase 0
    (zerop -> princ))
:outputs "0"
#?(predcase 0
    (zerop => princ print))
:outputs "0
0 "
#?(predcase 0
    (zerop ->)
    (t :never))
=> NIL

; result := return value of chosen clause's body.

;;;; Affected By:
; none

;;;; Side-Effects:
; none

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EPREDCASE)

;;;; Description:

#+syntax
(EPREDCASE arg &rest clauses) ; => result
; When any clause is chosen, an error is signaled.
#?(epredcase nil
    (integerp :never)
    (stringp :never))
:signals predcase-error

(requirements-about CPREDCASE)

;;;; Description:

#+syntax
(CPREDCASE arg &rest clauses) ; => result
; when any clause is chosen, continueable error is signaled.
#?(let ((var nil))
    (cpredcase var
      (integerp :never)
      (stringp :never)))
:signals error
,:with-restarts store-value

#?(handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'store-value 0))))
    (let ((var 1))
      (cpredcase var
        (zerop -> princ))
      (print var)))
:outputs "0
0 "

;;;; Notes:
; `ARG` should setfable place.
#?(cpredcase 0
    (zerop :work?))
=> unspecified

(requirements-about PREDCASE-ERROR)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; predcase-error simple-error simple-condition error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

(requirements-about INVALID-CLAUSE)

;;;; Description:
;;;; Class Precedence List: (case in SBCL)
; invalid-clause predcase-error simple-error simple-condition program-error error serious-condition condition slot-object t

;;;; Effective Slots:

; FORMAT-CONTROL [Type] T
; [READER] simple-condition-format-control

; FORMAT-ARGUMENTS [Type] T
; [READER] simple-condition-format-arguments

;;;; Notes:

