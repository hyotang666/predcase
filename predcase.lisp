(in-package :cl-user)
(defpackage :predcase(:use :cl)
  (:export
    #:predcase
    #:epredcase
    #:cpredcase
    ; condition
    #:predcase-error
    #:invalid-clause
    ))
(in-package :predcase)

(define-condition predcase-error(simple-error)()
  (:report(lambda(c *standard-output*)
	    (apply #'format t "~S: ~S must satisfies one of ~S."
		   (simple-condition-format-arguments c)))))

(define-condition invalid-clause(predcase-error program-error)()
  (:report(lambda(c *standard-output*)
	    (let((control(or (simple-condition-format-control c)
			     "~S Invalid clause comes. ~S")))
	      (apply #'format t control
		     (simple-condition-format-arguments c))))))

(defmacro predcase (arg &rest clauses)
  (handler-bind((invalid-clause(lambda(c)
				 (rplaca (simple-condition-format-arguments c)
					 'predcase))))
    (let((garg(gensym"ARG")))
      `(PROG((,garg ,arg)) ; in order to share code with CPREDCASE.
	                   ; CPREDCASE needs to CL:GO.
	 (DECLARE(IGNORABLE ,garg)) ; in order to validate (predcase :foo).
	 (COND,@(clauses clauses garg))))))

(defmacro epredcase (arg &rest clauses)
  (handler-bind((invalid-clause(lambda(c)
				 (rplaca (simple-condition-format-arguments c)
					 'epredcase))))
    (let((garg(gensym"ARG")))
      `(PROG((,garg ,arg))
	 (DECLARE(IGNORABLE ,garg))
	 (COND,@(clauses clauses garg)
	   (T(ERROR 'PREDCASE-ERROR 
		    :FORMAT-ARGUMENTS (LIST 'EPREDCASE ,garg ',(predicates clauses)))))))))

(defmacro cpredcase(arg &rest clauses)
  (handler-bind((invalid-clause(lambda(c)
				 (rplaca (simple-condition-format-arguments c)
					 'cpredcase))))
    (let((garg(gensym"ARG")))
      `(PROG((,garg ,arg))
	 (DECLARE(IGNORABLE ,garg))
	 :TOP
	 (COND,@(clauses clauses garg)
	   (T(RESTART-CASE(ERROR "CPREDCASE: The value ~S must satisfies one of the predicates ~{~S~^, ~}.",garg ',(predicates clauses))
	       (STORE-VALUE(V)
		 :REPORT (LAMBDA(*STANDARD-OUTPUT*)
			   (FORMAT T "Store new value for ~S." ',arg))
		 :INTERACTIVE (LAMBDA()
				(format *debug-io* "New value> ")
				(force-output)
				(list(read *debug-io*)))
		 (SETF ,garg (SETF ,arg V))
		 (GO :TOP)))))))))

(defun clauses(clauses arg)
  (loop :for clause :in clauses
	:collect (clause clause arg)))

(defun predicates(clauses)
  (mapcar #'car clauses))

(defun clause(clause arg)
  (typecase clause
    (atom(error(make-condition 'invalid-clause
			       :format-arguments(list 'clause clause))))
    (list
      (let((pred(car clause)))
	; pred is one of symbol, lambda-form, default-clause or compound form.
	(cond ; THIS COND FORM IS JUST WE WANT TO AVOID TO WRITE!
	  ((default-clause-p pred) ; comes T or OTHERWISE.
	   (if(used-arrow-syntax-p clause)
	     `(T (RETURN(PROGN ,@(consequents clause arg))))
	     `(T (RETURN(PROGN ,@(cdr clause))))))
	  ((predicate-name-p pred) ; comes symbol or lambda-form.
	   (if(used-arrow-syntax-p clause)
	     `((,pred ,arg)(RETURN(PROGN ,@(consequents clause arg))))
	     `((,pred ,arg)(RETURN(PROGN ,@(cdr clause))))))
	  (t ; finally comes compound form.
	    (if(used-arrow-syntax-p clause)
	      `(,(predexpand pred arg)(RETURN(PROGN ,@(consequents clause arg))))
	      `(,(predexpand pred arg)(RETURN(PROGN ,@(cdr clause)))))))))))

(defun default-clause-p(pred)
  (find pred '(T OTHERWISE)))

(defun used-arrow-syntax-p(clause)
  ; We don't export symbol -> and =>.
  ; So we must test it with STRING=.
  (let((it(second clause)))
    (and (symbolp it)
	 (find it '(-> =>):test #'string=))))

(defun consequents(clause arg)
  (loop :for task :in (cddr clause)
	:if (predicate-name-p task) :collect `(,task ,arg)
	:else :do (error 'invalid-clause
			 :format-control "~S: Invalid syntax in clause ~S.~&In consequent part with right arrow syntax,~&only function name is valid, but ~S."
			 :format-arguments (list 'consequents clause task))))

(defun predicate-name-p(arg)
  (or (symbolp arg)
      (lambda-form-p arg)))

(defun lambda-form-p(expr)
  (typep expr `(CONS(EQL LAMBDA)T)))

(defun predexpand(preds arg)
  (mapcar(lambda(form)
	   (if(find form '(AND OR NOT))
	     form
	     (if(predicate-name-p form)
	       `(,form ,arg)
	       (predexpand form arg))))
    preds))
