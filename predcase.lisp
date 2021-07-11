(in-package :cl-user)

(defpackage :predcase
  (:use :cl)
  (:export #:predcase
           #:epredcase
           #:cpredcase
	   ;; condition
           #:predcase-error
           #:invalid-clause))

(in-package :predcase)

(define-condition predcase-error (simple-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (apply #'format t "~S: ~S must satisfies one of ~S."
            (simple-condition-format-arguments c)))))

(define-condition invalid-clause (predcase-error program-error)
  ()
  (:report
   (lambda (c *standard-output*)
     (let ((control
            (or (simple-condition-format-control c)
                "~S Invalid clause comes. ~S")))
       (apply #'format t control (simple-condition-format-arguments c))))))

(defmacro predcase (arg &rest clauses)
  (handler-bind ((invalid-clause
                  (lambda (c)
                    (rplaca (simple-condition-format-arguments c) 'predcase))))
    (let ((garg (gensym "ARG")))
      `(prog ((,garg ,arg)) ; in order to share code with CPREDCASE. CPREDCASE
                            ; needs to CL:GO.
         (declare (ignorable ,garg)) ; in order to validate (predcase :foo).
         (cond ,@(clauses clauses garg))))))

(defmacro epredcase (arg &rest clauses)
  (handler-bind ((invalid-clause
                  (lambda (c)
                    (rplaca (simple-condition-format-arguments c) 'epredcase))))
    (let ((garg (gensym "ARG")))
      `(prog ((,garg ,arg))
         (declare (ignorable ,garg))
         (cond ,@(clauses clauses garg)
               (t
                (error 'predcase-error
                       :format-arguments (list 'epredcase ,garg
                                               ',(predicates clauses)))))))))

(defmacro cpredcase (arg &rest clauses)
  (handler-bind ((invalid-clause
                  (lambda (c)
                    (rplaca (simple-condition-format-arguments c) 'cpredcase))))
    (let ((garg (gensym "ARG")))
      `(prog ((,garg ,arg))
         (declare (ignorable ,garg))
        :top
         (cond ,@(clauses clauses garg)
               (t
                (restart-case (error
                                "CPREDCASE: The value ~S must satisfies one of the predicates ~{~S~^, ~}."
                                ,garg ',(predicates clauses))
                  (store-value (v)
                      :report (lambda (*standard-output*)
                                (format t "Store new value for ~S." ',arg))
                      :interactive (lambda ()
                                     (format *debug-io* "New value> ")
                                     (force-output)
                                     (list (read *debug-io*)))
                    (setf ,garg (setf ,arg v))
                    (go :top)))))))))

(defun clauses (clauses arg)
  (loop :for clause :in clauses
        :collect (clause clause arg)))

(defun predicates (clauses) (mapcar #'car clauses))

(defun clause (clause arg)
  (typecase clause
    (atom
     (error
       (make-condition 'invalid-clause
                       :format-arguments (list 'clause clause))))
    (list
     (let ((pred (car clause))) ; pred is one of symbol, lambda-form,
                                ; default-clause or compound form.
       (cond ; THIS COND FORM IS JUST WE WANT TO AVOID TO WRITE!
             ((default-clause-p pred) ; comes T or OTHERWISE.
              (if (used-arrow-syntax-p clause)
                  `(t (return (progn ,@(consequents clause arg))))
                  `(t (return (progn ,@(cdr clause))))))
             ((predicate-name-p pred) ; comes symbol or lambda-form.
              (if (used-arrow-syntax-p clause)
                  `((,pred ,arg) (return (progn ,@(consequents clause arg))))
                  `((,pred ,arg) (return (progn ,@(cdr clause))))))
             (t ; finally comes compound form.
              (if (used-arrow-syntax-p clause)
                  `(,(predexpand pred arg)
                    (return (progn ,@(consequents clause arg))))
                  `(,(predexpand pred arg)
                    (return (progn ,@(cdr clause)))))))))))

(defun default-clause-p (pred) (find pred '(t otherwise)))

(defun used-arrow-syntax-p (clause) ; We don't export symbol -> and =>. So we
                                    ; must test it with STRING=.
  (let ((it (second clause)))
    (and (symbolp it) (find it '(-> =>) :test #'string=))))

(defun consequents (clause arg)
  (loop :for task :in (cddr clause)
        :if (predicate-name-p task)
          :collect `(,task ,arg)
        :else
          :do (error 'invalid-clause
                     :format-control "~S: Invalid syntax in clause ~S.~&In consequent part with right arrow syntax,~&only function name is valid, but ~S."
                     :format-arguments (list 'consequents clause task))))

(defun predicate-name-p (arg) (or (symbolp arg) (lambda-form-p arg)))

(defun lambda-form-p (expr) (typep expr `(cons (eql lambda) t)))

(defun predexpand (preds arg)
  (mapcar
    (lambda (form)
      (if (find form '(and or not))
          form
          (if (predicate-name-p form)
              `(,form ,arg)
              (predexpand form arg))))
    preds))
