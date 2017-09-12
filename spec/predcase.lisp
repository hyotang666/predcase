(defpackage :predcase.spec
  (:use :cl :jingoh :predcase))
(in-package :predcase.spec)
(setup :predcase)

(requirements-about PREDCASE)

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

; arg := form generate key.
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
    (null :yes))
=> :YES
#?(predcase nil
    ((lambda(x)(null x)):yes))
=> :YES
; default-key := T | otherwise
#?(predcase nil
    (numberp :no)
    (t :yes))
=> :YES
#?(predcase nil
    (numberp :no)
    (otherwise :yes))
=> :YES
; compound-predicate-specifier := (not predicate-specifier) | (and predicate-specifier*) | (or predicate-specifier*)
#?(predcase nil
    ((not identity):yes))
=> :YES
#?(predcase 1
    ((or zerop oddp) :yes))
=> :YES
#?(predcase nil
    ((and listp atom) :yes))
=> :YES

; body := implicit progn

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

;;;; Arguments and Values:

; arg := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CPREDCASE)

;;;; Description:

#+syntax
(CPREDCASE arg &rest clauses) ; => result

;;;; Arguments and Values:

; arg := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

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

(requirements-about PREDCASE)

;;;; Description:

#+syntax
(PREDCASE arg &rest clauses) ; => result

;;;; Arguments and Values:

; arg := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about EPREDCASE)

;;;; Description:

#+syntax
(EPREDCASE arg &rest clauses) ; => result

;;;; Arguments and Values:

; arg := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

(requirements-about CPREDCASE)

;;;; Description:

#+syntax
(CPREDCASE arg &rest clauses) ; => result

;;;; Arguments and Values:

; arg := 

; clauses := 

; result := 

;;;; Affected By:

;;;; Side-Effects:

;;;; Notes:

;;;; Exceptional-Situations:

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

