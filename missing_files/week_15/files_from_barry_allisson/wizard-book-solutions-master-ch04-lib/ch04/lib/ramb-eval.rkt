#lang racket

(require (except-in "base-eval.rkt"
                    interpret))

(provide interpret
         driver-loop)

;;;;AMB EVALUATOR FROM SECTION 4.3 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;Code from SECTION 4.3.3, modified as needed to run it

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (mcdr exp))

;; analyze from 4.1.6, with clause from 4.3.3 added
;; and also support for Let
(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp)          (analyze-quoted exp))
        ((variable? exp)        (analyze-variable exp))
        ((assignment? exp)      (analyze-assignment exp))
        ((permanent-set? exp)   (analyze-permanent-set! exp))
        ((definition? exp)      (analyze-definition exp))
        ((if? exp)              (analyze-if exp))
        ((if-fail? exp)         (analyze-if-fail exp))
        ((lambda? exp)          (analyze-lambda exp))
        ((begin? exp)           (analyze-sequence (begin-actions exp)))
        ((cond? exp)            (analyze (cond->if exp)))
        ((let? exp)             (analyze (let->combination exp))) ;**
        ((amb? exp)             (analyze-amb exp))                ;**
        ((ramb? exp)            (analyze-ramb exp))
        ((application? exp)     (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

;;;Simple expressions

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;;Conditionals and sequences

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define if-fail-success mcadr)
(define if-fail-failure mcaddr)

(define (analyze-if-fail exp)
  (let ((sproc (analyze (if-fail-success exp)))
        (fproc (analyze (if-fail-failure exp))))
    (lambda (env succeed fail) 
      (sproc env
             succeed
             (lambda ()
               (fproc env succeed fail))))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (mcar rest-procs))
              (mcdr rest-procs))))
  (let ((procs (mmap analyze exps)))
    (when (null? procs)
      (error "Empty sequence -- ANALYZE"))
    (loop (mcar procs) (mcdr procs))))

;;;Definitions and assignments

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (permanent-set? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-set! exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (fail2)))))
             fail))))

;;;Procedure applications

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (mmap analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((mcar aprocs) env
                     ;; success continuation for this aproc
                     (lambda (arg fail2)
                       (get-args (mcdr aprocs)
                                 env
                                 ;; success continuation for recursive
                                 ;; call to get-args
                                 (lambda (args fail3)
                                   (succeed (mcons arg args)
                                            fail3))
                                 fail2))
                     fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

;;;amb expressions

(define (analyze-amb exp)
  (let ((cprocs (mmap analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((mcar choices) env
                            succeed
                            (lambda ()
                              (try-next (mcdr choices))))))
      (try-next cprocs))))

;;; random amb expressions

(define (any l) (mlist-ref l (random (mlength l))))

(define (remove-from x xs)
  (cond ((null? xs) null)
        ((equal? x (mcar xs)) (mcdr xs))
        (else (mcons (mcar xs) (remove-from x (mcdr xs))))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (mcdr exp))

(define (analyze-ramb exp)
  (let ((cprocs (mmap analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((choice (any choices)))
              (choice env
                      succeed
                      (lambda ()
                        (try-next (remove-from choice choices)))))))
      (try-next cprocs))))

;;;Driver loop

(define input-prompt ";;; Ramb-Eval input:")
(define output-prompt ";;; Ramb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (cond ((eq? input 'quit)      (void))
            ((eq? input 'try-again) (try-again))
            (else (newline)
                  (display ";;; Starting a new problem ")
                  (mlist->exp (ambeval (exp->mlist input)
                                       the-global-environment
                                       ;; ambeval success
                                       (lambda (val next-alternative)
                                         (announce-output output-prompt)
                                         (user-print val)
                                         (internal-loop next-alternative))
                                       ;; ambeval failure
                                       (lambda ()
                                         (announce-output
                                        ";;; There are no more values of")
                                         (user-print input)
                                         (driver-loop))))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline)
  (newline)
  (display string)
  (newline))

(define (announce-output string)
  (newline)
  (display string)
  (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (mlist 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
      (display object)))

;;; Support for Let (as noted in footnote 56, p.428)

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (mcadr  exp))
(define (let-body exp) (mcddr  exp))

(define (let-var binding) (mcar binding))
(define (let-val binding) (mcadr  binding))

(define (make-combination operator operands) (mcons operator operands))

(define (let->combination exp)
  ;;make-combination defined in earlier exercise
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (mmap let-var bindings)
                                   (let-body exp))
                      (mmap let-val bindings))))

(define (interpret input)
  (mlist->exp (ambeval (exp->mlist input)
                       the-global-environment
                       ;; ambeval success
                       (lambda (val next-alternative)
                         val)
                       ;; ambeval failure
                       (lambda ()
                         (newline)
                         (display "fail")
                         (newline)))))
