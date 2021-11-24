#lang racket

(require compatibility/mlist)                                          ; jsp
    
(provide mce)                                                          ; jsp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The source for the evaluator came from github:
;
;   https://github.com/imrehorvath/CS61A/blob/master/mceval.scm
;
; I modified the code to run under Racket, using mutable lists.
;
; I added the comment "jsp" on all of the lines that I modified and
;   moved some of the code around to group it together to help my
;   understanding.
;
; I included a number of test cases to test it, but I give no guarantees
;   that it all works or that I didn't add any bugs.
;
; Hope that this helps...
;
; JSP
; 11/21/2021
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;METACIRCULAR EVALUATOR FROM CHAPTER 4 (SECTIONS 4.1.1-4.1.4) of
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch4.scm

;;;;This file can be loaded into Scheme as a whole.
;;;;Then you can initialize and start the evaluator by evaluating
;;;; the expression (mce).


(display "----------- SECTION 4.1.1 ---------")(newline)
(display "-------- mc-eval / mc-apply -------")(newline)

;;;from section 4.1.4 -- must precede def of metacircular apply
(define apply-in-underlying-scheme apply)


(define (mc-eval exp env)
  ;(printf "in mc-eval: ~a : ~a" exp env)(newline)
  (cond ((self-evaluating? exp) exp)
	((variable? exp) (lookup-variable-value exp env))
	((quoted? exp) (text-of-quotation exp))
	((assignment? exp) (eval-assignment exp env))
	((definition? exp) (eval-definition exp env))
	((if? exp) (eval-if exp env))
	((lambda? exp)
	 (make-procedure (lambda-parameters exp)
			 (lambda-body exp)
			 env))
	((begin? exp) 
	 (eval-sequence (begin-actions exp) env))
	((cond? exp) (mc-eval (cond->if exp) env))
	((application? exp)
	 (mc-apply (mc-eval (operator exp) env)
		   (list-of-values (operands exp) env)))
	(else
	 (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply procedure arguments)
  ;(printf "in mc-apply: ~a : ~a" procedure arguments)(newline)
  (cond
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ((compound-procedure? procedure)
     (eval-sequence
      (procedure-body procedure)
      (extend-environment-proc
       (procedure-parameters procedure)
       arguments
       (procedure-environment procedure))))
    ((equal? procedure 'ok) (mcar arguments))                          ; jsp
    (else
     (error
      "Unknown procedure type -- APPLY" procedure))))


(display "---------- SECTION 4.1.2 ----------")(newline)
(display "---- handle eval request types ----")(newline)

; ------------------- helpers -----------------
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;(define (list-of-values exps env)                                     ; jsp
;  (newline)(printf "list-ofval: ~a : ~a" exps env)(newline)           ; jsp
;  (if (no-operands? exps)                                             ; jsp
;      '()                                                             ; jsp
;      (cons (mc-eval (first-operand exps) env)                        ; jsp                   
;            (list-of-values (rest-operands exps) env))))              ; jsp

(define (list-of-values exps env)                                      ; jsp
  (letrec ((loop (lambda (exps*)                                       ; jsp
                (if (no-operands? exps*)                               ; jsp
                    '()                                                ; jsp
                    (cons (mc-eval (first-operand exps*) env)          ; jsp
                          (loop (rest-operands exps*)))))))            ; jsp
 (list->mlist (loop exps))))    

; -------------- self-evaluating ---------------
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
	((boolean? exp) true)
	(else false)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (mc-eval (first-exp exps) env))
        (else (mc-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; ----------------- variable -------------------
(define (variable? exp) (symbol? exp))

; ------------------ quouted -------------------
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

; ----------------- assignment -----------------
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (mc-eval (assignment-value exp) env)
                       env)
  'ok)

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; ----------------- definition -----------------

(define (definition? exp)
  (tagged-list? exp 'define))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (mc-eval (definition-value exp) env)
                    env)
  'ok)

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; --------------------- if ---------------------

(define (eval-if exp env)
  (if (true? (mc-eval (if-predicate exp) env))
      (mc-eval (if-consequent exp) env)
      (mc-eval (if-alternative exp) env)))

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; ------------------ lambda -------------------

(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp)
  (list->mlist (cadr exp)))                                            ; jsp

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (mcons 'lambda (mcons parameters body)))                             ; jsp

; ------------------- begin --------------------

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; -------------------- cond -------------------

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                         
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; ------------------- application -------------

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(display "---------- SECTION 4.1.3 ----------")(newline)
(display "------------ procedures -----------")(newline)

(define (true? x)
  (not (eq? x false)))

(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


(display "------- environment / frames ------")(newline)

(define (enclosing-environment env) (mcdr env))                        ; jsp

(define (first-frame env) (mcar env))                                  ; jsp

(define the-empty-environment '())

(define (make-frame variables values)
  (mcons variables values))                                            ; jsp

(define (frame-variables frame) (mcar frame))                          ; jsp
(define (frame-values frame) (mcdr frame))

(define (add-binding-to-frame! var val frame)
  (set-mcar! frame (mcons var (mcar frame)))                           ; jsp
  (set-mcdr! frame (mcons val (mcdr frame))))                          ; jsp

(define (extend-environment vars vals base-env)                        ; jsp
  (if (= (mlength vars) (mlength vals))                                ; jsp
      (mcons (make-frame vars vals) base-env)                          ; jsp
      (if (< (mlength vars) (mlength vals))                            ; jsp
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (extend-environment-proc vars vals base-env)                   ; jsp
  (if (= (mlength vars) (mlength vals))                                ; jsp
      (mcons (make-frame vars vals) base-env)                          ; jsp
      (if (< (mlength vars) (mlength vals))                            ; jsp
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))                                     ; jsp
             (mcar vals))                                              ; jsp
            (else (scan (mcdr vars) (mcdr vals)))))                    ; jsp
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))                                     ; jsp
             (set-mcar! vals val))                                     ; jsp
            (else (scan (mcdr vars) (mcdr vals)))))                    ; jsp
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))                                     ; jsp
             (set-mcar! vals val))                                     ; jsp
            (else (scan (mcdr vars) (mcdr vals)))))                    ; jsp
    (scan (frame-variables frame)
          (frame-values frame))))


(display "---------- SECTION 4.1.4 ----------")(newline)
(display "--------- setup environment -------")(newline)

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    (define-variable! 'import
                      (list 'primitive
			    (lambda (name)
			      (define-variable! name
				                (list 'primitive (eval name))
				                the-global-environment)))
                      initial-env)
    initial-env))

; [do later] (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (mlist (list 'car car)                                               ; jsp
         (list 'cdr cdr)
         (list 'cons cons)
         (list 'null? null?)
         (list '+ +)
         (list '- -)
         (list '* *)
         (list '/ /)
         (list '= =)
         (list 'list list)
         (list 'append append)
         (list 'equal? equal?)
         ;;      more primitives
        ))

(define (primitive-procedure-names)
  (mmap car                                                            ; jsp
       primitive-procedures))

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (list 'primitive (cadr proc)))                  ; jsp
       primitive-procedures))

;[moved to start of file] (define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) (mlist->list args)))                 ; jsp


(display "-----------------------------------")(newline)
(display "------ commnad line version -------")(newline)

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (mc-eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment '())

(define (mce)
  (set! the-global-environment (setup-environment))
  (driver-loop))

;(mce)              ; commented out so as not to be evaluated when the file is loaded


(display "-----------------------------------")(newline)
(display "---------- test functions ---------")(newline)

(define user-initial-environment '())
(set! the-global-environment (setup-environment))


(display "------ (self-evaluating? exp) -----")(newline)
(mc-eval 5 the-global-environment)
(mc-eval '* the-global-environment)

(display "------- ((definition? exp) --------")(newline)
(display "-------- ((variable? exp) ---------")(newline)
(mc-eval '((define t 9)
           t)
         the-global-environment)

(mc-eval '((define t 9)
           t)
         the-global-environment)

(mc-eval '((define t 9)
           (= 9 t))
         the-global-environment)

(mc-eval '((define t 9)
           (= 7 t))
         the-global-environment)

(mc-eval 't
         the-global-environment)


(display "-------- ((assignment? exp) -------")(newline)

(mc-eval '(set! t 22)
         the-global-environment)

(mc-eval 't
         the-global-environment)


(display "------------- ((if? exp) ----------")(newline)

(mc-eval '(if (= 5 5)
             2
             3)
         the-global-environment)

(mc-eval '(if (= 5 4)
             2
             3)
         the-global-environment)


(display "----------- ((lambda? exp) --------")(newline)

(mc-eval '((define square
             (lambda (x y)
               (* x y)))
           (square 5 7))
         the-global-environment)

(mc-eval '((lambda (x) (* x x))
           5)
         the-global-environment)

         
(display "----------- ((begin? exp) ---------")(newline)
(mc-eval '(if '(= 5 5)
             (begin
               (+ 2 3)
               (+ 3 4)
               (+ 4 5))
             3)
         the-global-environment)


(display "------------ ((cond? exp) ---------")(newline)
(mc-eval '(cond
            ((= 5 6) "equal 1")
            ((= 5 5) "equal 2")
            ((= 7 6) "equal 3")
            (else "none equal"))
         the-global-environment)

(display "------------ ((procedure) ---------")(newline)
(mc-eval '(* 5 5) the-global-environment)
(mc-eval '(+ 5 (+ 2 5)) the-global-environment)
(mc-eval '(/ 5 5) the-global-environment)
(mc-eval '(= 5 5) the-global-environment)

(display "----------- ((function) ----------")(newline)

(mc-eval '((define test
             (lambda (x y)
               (cons x y)))
           (test 5 7))
         the-global-environment)

(mc-eval '((define test
             (lambda (x y)
               (if (= y 0)
                   0
                   (+ x (test x (- y 1))))))
           (test 5 7))
         the-global-environment)

(display "-----------------------------------")(newline)
(display "-----------------------------------")(newline)
(display "-----------------------------------")(newline)


