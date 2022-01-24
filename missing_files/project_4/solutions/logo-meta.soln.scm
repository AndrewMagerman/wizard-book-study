;;; logo-meta.soln.scm      Part of programming project #4  SOLUTIONS

;;; Differences between the book and this version:  Eval and apply have
;;; been changed to logo-eval and logo-apply so as not to overwrite the Scheme
;;; versions of these routines. An extra procedure initialize-logo has been
;;; added. This routine resets the global environment and then executes the
;;; driver loop. This procedure should be invoked to start the Logo
;;; evaluator executing.  Note: It will reset your global environment and all
;;; definitions to the Logo interpreter will be lost. To restart the Logo
;;; interpreter without resetting the global environment, just invoke
;;; driver-loop.  Don't forget that typing control-C will get you out of
;;; the Logo evaluator back into Scheme.

;;; Problems A1, A2, and B2 are entirely in logo.scm
;;; Problems A3, B3, and 6 require you to find and change existing procedures.

;;;  Procedures that you must write from scratch:

;;; Problem B1    eval-line

(define (eval-line line-obj env)
  (if (ask line-obj 'empty?)
      '=no-value=
      (let ((value (logo-eval line-obj env)))
	(if (eq? value '=no-value=)
       	    (eval-line line-obj env)
	    value))))


;;; Problem B3    variables  (other procedures must be modified, too)
;;; data abstraction procedures

(define (variable? exp)
  (and (word? exp) (eq? (first exp) ':)))

(define (variable-name exp)
  (bf exp))


;;; Problem A4   handle-infix

(define (de-infix token)
  (cdr (assoc token '((+ . sum)
		      (- . difference)
		      (* . product)
		      (/ . quotient)
		      (= . equalp)
		      (< . lessp)
		      (> . greaterp)))))

(define (handle-infix value line-obj env)
  (if (ask line-obj 'empty?)
      value
      (let ((token (ask line-obj 'next)))
	(if (memq token '(+ - * / = < >))
	    (handle-infix ((text (lookup-procedure (de-infix token)))
			   value
			   (eval-prefix line-obj env) )
			  line-obj
			  env)
	    (begin
	     (ask line-obj 'put-back token)
	     value)))))


;;; Problem B4    eval-definition


(define (eval-definition line-obj)
  (define (parse-formal token)
    (if (eq? (first token) ':)
	(bf token)
	(error "Bad input name format in TO " token)))
  (define (get-formals)
    (if (ask line-obj 'empty?)
	'()
	(let ((token (ask line-obj 'next)))
	  (if (eq? token 'static)		;; added for problem 6
	      '()				;; ...
	      (cons (parse-formal token) (get-formals))))))

;;; The following procedure is added for problem 6.
  (define (get-statics)
    (if (ask line-obj 'empty?)
	(make-frame '() '())
	(let ((variable (parse-formal (ask line-obj 'next))))
	  (let ((value (logo-eval line-obj the-global-environment)))
	    (let ((frame (get-statics)))
	      (add-binding-to-frame! variable value frame)
	      frame)))))
;;; End of problem 6 addition.

  (define (get-body)
    (prompt "-> ")
    (let ((line (logo-read)))
      (if (equal? line '(end))
	  '()
	  (cons line (get-body)))))
  (let ((name (ask line-obj 'next)))
    (let ((formals (get-formals)))
      (let ((statics (if (ask line-obj 'empty?)   ;; added for problem 6
			 (make-frame '() '())     ;; ...
			 (get-statics))))	  ;; ...
	(set! the-procedures
	      (cons (list name
			  'compound
			  (length formals)
			  (cons formals (get-body))
			  statics		  ;; added for problem 6
			  #f)			;; not stepped
		  the-procedures)))))
  '=no-value=)


;;; Problem 5    eval-sequence

(define (eval-sequence exps env step?)
  (if (null? exps)
      '=no-value=
      (begin
       (if step?
	   (begin (logo-type (car exps))
		  (prompt ">>> ")
		  (logo-read)))
       (let ((value (eval-line (make-line-obj (car exps)) env)))
	 (cond ((eq? value '=stop=) '=no-value=)
	       ((and (pair? value) (eq? (car value) '=output=))
		(cdr value))
	       ((not (eq? value '=no-value=))
		(error "You don't say what to do with " value))
	       (else (eval-sequence (cdr exps) env step?)))))))



;;; SETTING UP THE ENVIRONMENT

(define the-primitive-procedures '())

(define (add-prim name count proc)
  (set! the-primitive-procedures
	(cons (list name 'primitive count proc)
	      the-primitive-procedures)))

(add-prim 'first 1 first)
(add-prim 'butfirst 1 bf)
(add-prim 'bf 1 bf)
(add-prim 'last 1 last)
(add-prim 'butlast 1 bl)
(add-prim 'bl 1 bl)
(add-prim 'word -2 word)
(add-prim 'sentence -2 se)
(add-prim 'se -2 se)
(add-prim 'list -2 list)
(add-prim 'fput 2 cons)

(add-prim 'sum -2 (make-logo-arith +))
(add-prim 'difference 2 (make-logo-arith -))
(add-prim '=unary-minus= 1 (make-logo-arith -))
(add-prim '- 1 (make-logo-arith -))
(add-prim 'product -2 (make-logo-arith *))
(add-prim 'quotient 2 (make-logo-arith /))
(add-prim 'remainder 2 (make-logo-arith remainder))

(add-prim 'print 1 logo-print)
(add-prim 'pr 1 logo-print)
(add-prim 'show 1 logo-show)
(add-prim 'type 1 logo-type)
(add-prim 'make '(2) make)

(add-prim 'run '(1) run)
(add-prim 'if '(2) logo-if)
(add-prim 'ifelse '(3) ifelse)
(add-prim 'test '(1) test)
(add-prim 'iftrue '(1) iftrue)
(add-prim 'ift '(1) iftrue)
(add-prim 'iffalse '(1) iffalse)
(add-prim 'iff '(1) iffalse)
(add-prim 'equalp 2 (logo-pred (make-logo-arith equalp)))
(add-prim 'lessp 2 (logo-pred (make-logo-arith <)))
(add-prim 'greaterp 2 (logo-pred (make-logo-arith >)))
(add-prim 'emptyp 1 (logo-pred empty?))
(add-prim 'numberp 1 (logo-pred (make-logo-arith number?)))
(add-prim 'listp 1 (logo-pred list?))
(add-prim 'wordp 1 (logo-pred (lambda (x) (not (list? x)))))

(add-prim 'stop 0 (lambda () '=stop=))
(add-prim 'output 1 (lambda (x) (cons '=output= x)))
(add-prim 'op 1 (lambda (x) (cons '=output= x)))

(define (pcmd proc) (lambda args (apply proc args) '=no-value=))
(add-prim 'cs 0 (pcmd cs))
(add-prim 'clearscreen 0 (pcmd cs))
(add-prim 'fd 1 (pcmd fd))
(add-prim 'forward 1 (pcmd fd))
(add-prim 'bk 1 (pcmd bk))
(add-prim 'back 1 (pcmd bk))
(add-prim 'lt 1 (pcmd lt))
(add-prim 'left 1 (pcmd lt))
(add-prim 'rt 1 (pcmd rt))
(add-prim 'right 1 (pcmd rt))
(add-prim 'setxy 2 (pcmd setxy))
(add-prim 'setx 1 (lambda (x) (setxy x (ycor)) '=no-value=))
(add-prim 'sety 1 (lambda (y) (setxy (xcor) y) '=no-value=))
(add-prim 'xcor 0 xcor)
(add-prim 'ycor 0 ycor)
(add-prim 'pos 0 pos)
(add-prim 'seth 1 (pcmd setheading))
(add-prim 'setheading 1 (pcmd setheading))
(add-prim 'heading 0 heading)
(add-prim 'st 0 (pcmd st))
(add-prim 'showturtle 0 (pcmd st))
(add-prim 'ht 0 (pcmd ht))
(add-prim 'hideturtle 0 (pcmd ht))
(add-prim 'shown? 0 shown?)
(add-prim 'pd 0 (pcmd pendown))
(add-prim 'pendown 0 (pcmd pendown))
(add-prim 'pu 0 (pcmd penup))
(add-prim 'penup 0 (pcmd penup))
(add-prim 'pe 0 (pcmd penerase))
(add-prim 'penerase 0 (pcmd penerase))
(add-prim 'home 0 (pcmd home))
(add-prim 'setpc 1 (pcmd setpc))
(add-prim 'setpencolor 1 (pcmd setpc))
(add-prim 'pc 0 pc)
(add-prim 'pencolor 0 pc)
(add-prim 'setbg 1 (pcmd setbg))
(add-prim 'setbackground 1 (pcmd setbg))

(add-prim 'load 1 meta-load)

(add-prim 'step 1 (lambda (name)
		    (let ((proc (lookup-procedure name)))
		      (if proc
			  (set-stepped?! proc #t)))
		    '=no-value=))

(add-prim 'unstep 1 (lambda (name)
		    (let ((proc (lookup-procedure name)))
		      (if proc
			  (set-stepped?! proc #f)))
		    '=no-value=))

(define the-global-environment '())
(define the-procedures the-primitive-procedures)

;;; INITIALIZATION AND DRIVER LOOP

;;; The following code initializes the machine and starts the Logo
;;; system.  You should not call it very often, because it will clobber
;;; the global environment, and you will lose any definitions you have
;;; accumulated.

(define (initialize-logo)
  (set! the-global-environment (extend-environment '(" test") '(()) '()))
  (set! the-procedures the-primitive-procedures)
  (driver-loop))

(define (driver-loop)
  (define (helper)
    (prompt "? ")
    (let ((line (logo-read)))
      (if (not (null? line))
  	  (let ((result (eval-line (make-line-obj line)
				   the-global-environment)))
	    (if (not (eq? result '=no-value=))
		(logo-print (list "You don't say what to do with" result))))))
    (helper))
  (logo-read)
  (helper))

;;; APPLYING PRIMITIVE PROCEDURES

;;; To apply a primitive procedure, we ask the underlying Scheme system
;;; to perform the application.  (Of course, an implementation on a
;;; low-level machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (text p) args))


;;; Now for the code that's based on the book!!!


;;; Section 4.1.1

;; Given an expression like (proc :a :b :c)+5
;; logo-eval calls eval-prefix for the part in parentheses, and then
;; handle-infix to check for and process the infix arithmetic.
;; Eval-prefix is comparable to Scheme's eval.

(define (logo-eval line-obj env)
  (handle-infix (eval-prefix line-obj env) line-obj env))

(define (eval-prefix line-obj env)
  (define (eval-helper paren-flag)
    (let ((token (ask line-obj 'next)))
      (cond ((self-evaluating? token) token)
            ((variable? token)
	     (lookup-variable-value (variable-name token) env))
            ((quoted? token) (text-of-quotation token))
            ((definition? token) (eval-definition line-obj))
	    ((left-paren? token)
	     (let ((result (handle-infix (eval-helper #t)
				       	 line-obj
				       	 env)))
	       (let ((token (ask line-obj 'next)))
	       	 (if (right-paren? token)
		     result
		     (error "Too much inside parens")))))
	    ((right-paren? token)
	     (error "Unexpected ')'"))
            (else
	     (let ((proc (lookup-procedure token)))
	       (if (not proc) (error "I don't know how  to " token))

;;; Problem A3 solution starts here
	       (if (list? (arg-count proc))
	       	   (logo-apply proc
			       (cons env
				     (collect-n-args (car (arg-count proc))
						     line-obj
						     env))
			       env)
	       	   (logo-apply proc
		       	       (collect-n-args (if paren-flag
						   (arg-count proc)
						   (abs (arg-count proc)))
					       line-obj
					       env)
		       	       env)))) )))
;;; Problem A3 solution ends here

  (eval-helper #f))

(define (logo-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                         (cons " test" (parameters procedure))
                         (cons (lookup-variable-value " test" env)
			       arguments)
			 (adjoin-frame			;; added for problem 6
			  (statics-frame procedure)	;; ...
			  env))
			(stepped? procedure)))
        (else
         (error "Unknown procedure type -- LOGO-APPLY " procedure))))

(define adjoin-frame cons)

(define (collect-n-args n line-obj env)
  (cond ((= n 0) '())
	((and (< n 0) (not (ask line-obj 'empty?)))
	 (let ((token (ask line-obj 'next)))
	   (ask line-obj 'put-back token)
	   (if (right-paren? token)
	       '()
      	       (let ((next (logo-eval line-obj env)))
        	 (cons next
	      	       (collect-n-args (- n 1) line-obj env)) ))))
	(else      
      	 (let ((next (logo-eval line-obj env)))
           (cons next
	      	 (collect-n-args (- n 1) line-obj env)) ))))

;;; Section 4.1.2 -- Representing expressions

;;; numbers

(define (self-evaluating? exp) (number? exp))

;;; quote

(define (quoted? exp)
  (or (list? exp)
      (eq? (string-ref (word->string (first exp)) 0) #\")))

(define (text-of-quotation exp)
  (if (list? exp)
      exp
      (bf exp)))

;;; parens

(define (left-paren? exp) (eq? exp left-paren-symbol))

(define (right-paren? exp) (eq? exp right-paren-symbol))

;;; definitions

(define (definition? exp)
  (eq? exp 'to))

;;; procedures

(define (lookup-procedure name)
  (assoc name the-procedures))

(define (primitive-procedure? p)
  (eq? (cadr p) 'primitive))

(define (compound-procedure? p)
  (eq? (cadr p) 'compound))

(define (arg-count proc)
  (caddr proc))

(define (text proc)
  (cadddr proc))

(define (parameters proc) (car (text proc)))

(define (procedure-body proc) (cdr (text proc)))

;;; Added for problem 6.
(define (statics-frame proc) (car (cddddr proc)))

;;; ***
(define (stepped? proc) (cadr (cddddr proc)))
(define (set-stepped?! proc tf)
  (set-car! (cdr (cddddr proc)) tf))

;;; Section 4.1.3

;;; Operations on environments

(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied " vars vals)
          (error "Too few arguments supplied " vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET! " var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; This procedure is modified in problem B3.
(define (define-variable! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((equal? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (add-binding-to-frame! var val (first-frame the-global-environment))
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
