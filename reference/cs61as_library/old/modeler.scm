;; Replacement Modeler for STk, by Brian Gaeke <brg@EECS.Berkeley.EDU>
;; Version 2.0$Revision: 1.6 $ $Date: 2002/09/22 11:43:15 $
;; This version of the modeler is used for CS3.  The only difference
;; is that "using-berkeley.scm?" is defined to be true.

;==============================================================================
; Initialization code
;==============================================================================

; This is required to keep STk's eval from replacing variable names with
; pointers into environment structures:
(set! *debug* #t)

; A futile attempt to ignore bgerror and make errors come up in the console.
(define (bgerror . args) #f)
(define (stk-error . args) args)

; Triggering the autoload of the pretty-print package seems to
; slightly reduce the amount of memory needed to start the modeler.
(pp #f #f)

;==============================================================================
; Back end replacement evaluation code
;==============================================================================

;; (8/16/94 added map, reduce, filter, apply, and let* as "special forms")
;; This file requires GET-FN to be defined, and requires BOUND? and EVAL.
;; This file "exports" PART-EVAL, which takes an expression as its
;; argument and returns the same expression, evaluated by one step.
;; Also, it "exports" FULL-EVAL, which takes an expression as its
;; argument and returns the same expression, evaluated "all the way".
;; FULL-eval is different from Scheme's EVAL because, for example,
;; it returns a variable naming a procedure or a lambda expression
;; instead of a procedure.

;; Global variables:
;
; This is in support of CS3.
;
(define *they-know-lambda* #f)
;
; STk always loads berkeley.scm.
;
(define (using-berkeley-scm?) #t)
(define *harvey+wright* (using-berkeley-scm?))
(define *grillmeyer* (not (using-berkeley-scm?)))

;; The evaluator

; This is one of the main entry points to the replacement modeler.
; It is used for doing a partial evaluation of a function.
(define *the-expression-that-failed* #f)
(define (part-eval exp)
    (let ((result (cond ((basic? exp) exp)
                        ((symbol? exp) (symeval exp))
                        ((special-form? exp) (eval-spec-form exp))
                        ((application? exp)
                         (eval-application exp))
                        (else (eval-error "Unrecognized expession type:" exp)))))
      result))

;; Variables
;; Note that all local variables get substituted before the evaluator
;; gets to them, so any variable that you actually try to evaluate is
;; global and you can just eval it.
;; Maybe add a hack for global variables whose value is a function, e.g.,
;; (define foo +)...

; Used for finding the values of variables (symbols).
(define (symeval var)
  (cond ((member var *the-real-special-forms*)
         (eval-error "Can't evaluate the name of a special form: " var))
        ((not (bound? var)) (eval-error "Variable not bound: " var))
        (else (let ((val (eval var)))
                (if (procedure? val)
                    var
                    (quote-if-necessary val))))))

;; Special Forms
;; Note that quote and lambda expressions are treated as basic, so they
;; don't really count as special forms.  Also, some of the higher-order
;; functions we provide are treated as special forms, even though they're
;; really not special forms.

(define (eval-accumulate exp)
  (let ((fn (cadr exp))
        (stuff (map quote-if-necessary
                    ((if *grillmeyer* map every)
                     (lambda (x) x) (eval (caddr exp)))) ))
    (if (null? stuff)
        (list fn)
        (accum/reduce-expand fn stuff)) ))

(define (accum/reduce-expand fn stuff)
  (if (null? (cdr stuff))
      (car stuff)
      (if *grillmeyer*          
          ; left-to-right accumulation
          (accum/reduce-expand 
            fn 
            (cons (list fn (car stuff) (cadr stuff))
                  (cddr stuff)))
          ; Harvey+Wright right-to-left accumulation
          (list fn (car stuff)  
                (accum/reduce-expand fn (cdr stuff)) ) ) ) )

(define *not-basic* (cons 'not-basic '()))

(define (eval-and exp)
  (define (and-value exp)
    (cond ((not (basic? (car exp)))
           *not-basic*)
          ((null? (cdr exp)) (car exp))
          ((car exp) (and-value (cdr exp)))
          (else #f) ))
  (if (null? (cdr exp))
      #t
      (let ((ans (and-value (cdr exp))))
        (if (eq? ans *not-basic*)
            (cons 'and (make-next-basic (cdr exp)))
            ans)) ))

(define (eval-apply exp)
  (let* ((fn-exp (cadr exp))
         (lst-exp (caddr exp))
         (lst-elements (map quasiquote-if-necessary (cadr lst-exp))))
    (cons fn-exp lst-elements)))

  (define (eval-cond-try-next-clause clauses)
    (if (basic? (car (car clauses)))
        (cons (car clauses)
              (eval-cond-try-next-clause (cdr clauses)))
        (cons (cons (full-eval (car (car clauses)))
                    (cdr (car clauses)))
              (cdr clauses) )))
  (define (eval-cond-valid-shapes? clauses)
    (if (null? clauses)
        #t
        (let ((c (car clauses)))
          (and (list? c)
               (not (null? c))
               (<= (length c) 2)
               (if (eq? (car c) 'else)
                   (and (= (length c) 2) (null? (cdr clauses)))
                   (eval-cond-valid-shapes? (cdr clauses)) )))))
(define (eval-cond exp)
  (define (eval-cond-helper clauses)
    (cond ((null? clauses) 
           (eval-error "No true clauses or else clause in COND"))
          ((eq? (car (car clauses)) #f)
           (eval-cond-helper (cdr clauses)))
          ((eq? (car (car clauses)) 'else)
           (cadr (car clauses)) )
          ((basic? (car (car clauses)))
           (if (null? (cdr (car clauses)))
               (car (car clauses))
               (cadr (car clauses))))
          (else (cons 'cond (eval-cond-try-next-clause (cdr exp)))) ))
  (if (eval-cond-valid-shapes? (cdr exp))
      (eval-cond-helper (cdr exp))
      (eval-error "Bad COND syntax:" exp) ))

; Grillmeyer only
(define (eval-count-if exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons '+
          (map (lambda (elt) `(if (,fn-exp ,elt) 1 0))
               list-elts)) ))

; Grillmeyer only
(define (eval-count-if-not exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons '+
          (map (lambda (elt) `(if (,fn-exp ,elt) 0 1))
               list-elts))))

(define (eval-define exp)
  (eval-error "You can't use this evaluator for DEFINE") )

; Grillmeyer and Harvey+Wright have different versions.
(define (eval-every exp)
  (if *grillmeyer*         
      ; Grillmeyer version, returns #t or #f
      (let* ((fn-exp (cadr exp))
             (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
             (list-elts (map quasiquote-if-necessary datum)))
        (append
          (cons 'and
              (map (lambda (elt) (list fn-exp elt))
                   list-elts))
          '(#t)) )
      
      ; Harvey+Wright word/sent version of map
      (let* ((fn-exp (cadr exp))
             (datum (eval (caddr exp)))
             (exploded (map quote-if-necessary (every (lambda (x) x) datum))))
        ;; EXPLODED has two purposes:
        ;; 1 -- Turn a word into a list of letters.
        ;; 2 -- Generate an error if datum isn't a word or sentence.
        (cons 'se
              (map (lambda (elt) (list fn-exp elt))
                   exploded)) )))

; Used by Harvey & Wright to evaluate their filter, and used by Grillmeyer
; to evaluate his keep-if.
(define (eval-filter exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons 'append
          (map (lambda (elt) `(if (,fn-exp ,elt) (list ,elt) '()))
               list-elts)) ))

(define (eval-find-if exp) #t)

(define (eval-find-if-not exp) #t)

(define (eval-if exp)
  (if (cadr exp)
      (caddr exp)
      (cadddr exp) ))

;R4RS optional 1+ and -1+
(define (eval-1+ exp)
  (1+ (cadr exp)))

(define (eval--1+ exp)
  (-1+ (cadr exp)))


; Harvey & Wright only
(define (eval-keep exp)
  (let* ((fn (cadr exp))
         (datum (eval (caddr exp)))
         (stuff (map quote-if-necessary (every (lambda (x) x) datum)))
         (combiner (if (word? datum) 'word 'se))
         (ident (if (word? datum) "" ''())) )
    (cons combiner
          (map (lambda (elt) `(if (,fn ,elt) ,elt ,ident))
               stuff)) ))

(define (eval-let exp)
  (let* ((bindings (cadr exp))
         (foo (if (not (correct-let-binding-syntax? bindings))
                  (eval-error "Illegal LET syntax: " exp)
                  'foo))
         (names (map car bindings))
         (values (map cadr bindings))
         (body (caddr exp)) )
    `((lambda ,names ,body) ,@values) ))

(define (eval-let* exp)
  (define (translate bindings body)
    (if (null? bindings)
        body
        (list 'let
              (list (car bindings))
              (translate (cdr bindings) body))))
  (let ((bindings (cadr exp))
        (body (caddr exp)))
    (if (not (correct-let-binding-syntax? (cadr exp)))
        (eval-error "Illegal LET* syntax: " exp)
        (translate bindings body))))

(define (correct-let-binding-syntax? bindings)
  (if (null? bindings)
      #t
      (and (list? (car bindings))
           (= (length (car bindings)) 2)
           (correct-let-binding-syntax? (cdr bindings)))))

(define (eval-map exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons 'list 
          (map (lambda (elt) (list fn-exp elt))
               list-elts))))

(define (eval-or exp)
  (define (or-value exp)
    (cond ((null? exp) #f)
          ((not (basic? (car exp)))
           *not-basic*)
          ((car exp))   ; Change to #t to make OR sensible
          (else (or-value (cdr exp))) ))
  (let ((ans (or-value (cdr exp))))
    (if (eq? ans *not-basic*)
        (cons 'or (make-next-basic (cdr exp)))
        ans)) )

; Harvey & Wright only
(define (eval-reduce exp)
  (let* ((fn (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (if (null? list-elts)
        (list fn)
        (accum/reduce-expand fn list-elts)) ))

; Grillmeyer only
(define (eval-remove-if exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons 'append
          (map (lambda (elt) `(if (,fn-exp ,elt) '() (list ,elt)))
               list-elts)) ))

; Harvey & Wright only
(define (eval-repeated exp)
  (let ((fn (cadr exp))
        (num (caddr exp)) )
    (if (not (and (integer? num) (>= num 0)))
        (eval-error "Second argument to REPEATED must be a positive integer")
        `(lambda (x) ,(nest-function-calls fn num 'x)) )))

; Grillmeyer only
(define (eval-some exp)
  (let* ((fn-exp (cadr exp))
         (datum (cadr (caddr exp)))     ; Get rid of quote or quasiquote
         (list-elts (map quasiquote-if-necessary datum)))
    (cons 'or
          (map (lambda (elt) (list fn-exp elt))
               list-elts)) ))

(define (nest-function-calls fn n base-case)
  (if (= n 0)
      base-case
      (list fn (nest-function-calls fn (- n 1) base-case)) ))


;; Each entry in this table has the form:
;; 1) Keyword
;; 2) Modeller procedure to translate the special form
;; 3) Which arguments are evaluated before the special form does its
;;    translation.  (E.g., IF evaluates its first argument only, and
;;    REPEATED evaluated both of its args.)
;; 4) The required length of the special form, i.e., one more than the number
;;    of arguments.  0 if it can be any length.

(define *the-special-forms*
  (if *grillmeyer*
      ; Grillmeyer forms to be evaluated specially
      `((1+ ,eval-1+ (1) 2)
        (1- ,eval--1+ (1) 2)
        (-1+ ,eval--1+ (1) 2)
        (accumulate ,eval-reduce (1 2) 3)  ; Grillmeyer version
        (and ,eval-and () 0)
        (apply ,eval-apply (1 2) 3)
        (cond ,eval-cond () 0)
        (count-if ,eval-count-if (1 2) 3)  ; Grillmeyer only
        (count-if-not ,eval-count-if-not (1 2) 3)  ; Grillmeyer only
        (define ,eval-define () 0)
        (every ,eval-every (1 2) 3)  ; Grillmeyer and Harvey & Wright differ
        ; Modelling find-if(-not) as a recursive function is ok.
        ;   (find-if ,eval-find-if (1 2) 3)  ; Grillmeyer only
        ;   (find-if-not ,eval-find-if-not (1 2) 3)  ; Grillmeyer only
        (if ,eval-if (1) 4)
        (keep-if ,eval-filter (1 2) 3)  ; Grillmeyer only
        (let ,eval-let () 3)
        (let* ,eval-let* () 3)
        (letrec ,eval-define () 0)
        (map ,eval-map (1 2) 3)
        (or ,eval-or () 0)
        (remove-if ,eval-remove-if (1 2) 3)  ; Grillmeyer only
        (some ,eval-some (1 2) 3) )  ; Grillmeyer only
      
      ; Harvey+Wright forms to be evaluated specially
      `((1+ ,eval-1+ (1) 2)
        (1- ,eval--1+ (1) 2)
        (-1+ ,eval--1+ (1) 2)
        (accumulate ,eval-accumulate (1 2) 3) ; Harvey & Wright version
        (and ,eval-and () 0)
        (apply ,eval-apply (1 2) 3)
        (cond ,eval-cond () 0)
        (define ,eval-define () 0)
        (every ,eval-every (1 2) 3)  ; Grillmeyer and Harvey & Wright differ
        (filter ,eval-filter (1 2) 3)  ; Harvey & Wright only
        (if ,eval-if (1) 4)
        (keep ,eval-keep (1 2) 3)  ; Harvey & Wright only
        (let ,eval-let () 3)
        (let* ,eval-let* () 3)
        (letrec ,eval-define () 0)
        (map ,eval-map (1 2) 3)
        (or ,eval-or () 0)
        (reduce ,eval-reduce (1 2) 3)  ; Harvey & Wright only
        (repeated ,eval-repeated (2) 3)  ; Harvey & Wright only
        ) ) )

;; A list of the actual special forms, i.e., symbols we should try not
;; to EVAL.
(define *the-real-special-forms*
  '(lambda quote and cond define if let or))

;; The actual evaluation of special forms

(define (special-form? exp)
  (and (list? exp)
       (assoc (car exp) *the-special-forms*)))

(define (eval-spec-form exp)
  (let* ((record (assoc (car exp) *the-special-forms*))
         (num-subexprs (cadddr record))
         (which-to-eval (caddr record))
         (eval-fn (cadr record)))
    (cond ((and (> num-subexprs 0)
                (not (= num-subexprs (length exp))))
           (eval-error "Wrong number of arguments to special form: " exp))
          ((these-basic? which-to-eval exp)
           (eval-fn exp))
          (else (make-these-basic which-to-eval exp)) )))

;; For special forms that evaluate some of their arguments:
(define (these-basic? which-ones exp)
  (cond ((null? which-ones) #t)
        ((basic? (list-ref exp (car which-ones)))
         (these-basic? (cdr which-ones) exp))
        (else #f) ))

(define (make-these-basic which-ones exp)
  (define (helper this which-ones rest-exp)
    (cond ((null? rest-exp) '())
          ((null? which-ones) rest-exp)
          ((= this (car which-ones))
           (cons (full-eval (car rest-exp))
                 (helper (+ this 1) (cdr which-ones) (cdr rest-exp)) ))
          (else (cons (car rest-exp)
                      (helper (+ this 1) which-ones (cdr rest-exp)) ))))
  (helper 0 which-ones exp) )

(define (make-next-basic exps)
  (if (basic? (car exps))
      (cons (car exps)
            (make-next-basic (cdr exps)) )
      (cons (full-eval (car exps))
            (cdr exps))))


;; Applications:

(define application? list?)

(define (eval-application exp)
  (cond ((all-basic? exp) (part-apply (car exp) (cdr exp)))
        ((all-but-one-basic? exp) (map part-eval exp))
        (else (map full-eval exp))))

(define (part-apply fn args)
  (cond ((function-name? fn)
         (let ((lambda-expr-or-procedure (get-fn fn)))
           (if (procedure? lambda-expr-or-procedure)
               (apply-primitive lambda-expr-or-procedure args)
               (user-apply lambda-expr-or-procedure args))))
        ((legit-lambda-expr? fn)
         (user-apply (fn-record-from-lambda fn) args))
        ((call-to-repeated? fn)
         (handle-repeated-as-primitive fn args))
        (else (eval-error "Not a function: " fn)) ))

(define (legit-lambda-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'lambda)
       (pair? (cdr expr))
       (ok-formals? (cadr expr))
       (pair? (cddr expr))
       (null? (cdddr expr))))

(define (ok-formals? formals)
  (cond ((null? formals) #t)
        ((symbol? formals) #t)
        ((pair? formals)
         (and (symbol? (car formals))
              (ok-formals? (cdr formals))))
        (else #f)))

(define (call-to-repeated? fn)
  (and (pair? fn)
       (eq? 'repeated (car fn))
       (pair? (cdr fn))
       (pair? (cddr fn))
       (integer? (caddr fn))
       (>= (caddr fn) 0)
       (null? (cdddr fn))
       (procedural-basic-value? (cadr fn))))

(define (procedural-basic-value? thingo)
  (or (function-name? thingo) 
      (legit-lambda-expr? thingo)
      (call-to-repeated? thingo)))

(define (handle-repeated-as-primitive repeated-call args)
  (let ((fn (cadr repeated-call))
        (num (caddr repeated-call)))
    (if (= 1 (length args))
        (nest-function-calls fn num (car args))
        (eval-error "Wrong number of arguments to function returned by REPEATED."))))

(define (apply-primitive fn args)
;; ARGS is a list of basic expressions
  (thunky-val->basic-expr
    (apply fn (map basic-expr->thunky-val args))))

(define (basic-expr->thunky-val expr)
  (cond ((procedural-basic-value? expr)
         (lambda () expr))
        ((quasiquoted? expr)
         (deep-comma->thunk (cadr expr)))
        (else (eval expr))))

(define (thunky-val->basic-expr tv)
  (cond ((procedure? tv) (tv))
        ((list-with-any-thunks? tv)
         (list 'quasiquote (deep-thunk->comma tv)))
        ((or (symbol? tv) (list? tv))
         (list 'quote tv))
        (else tv)))

(define (quasiquoted? expr)
  (and (pair? expr) (eq? (car expr) 'quasiquote)))

(define (deep-comma->thunk expr)
  (cond ((unquoted? expr) (lambda () (cadr expr)))
        ((quoted? expr) expr)
        ((quasiquoted? expr) expr)      ;; we think this can't happen
        ((pair? expr)
         (cons (deep-comma->thunk (car expr))
               (deep-comma->thunk (cdr expr))))
        (else expr)))

(define (quoted? expr)
  (and (pair? expr) (eq? (car expr) 'quote)))

(define (list-with-any-thunks? tree)
  (cond ((procedure? tree) #t)
        ((pair? tree) (or (list-with-any-thunks? (car tree))
                          (list-with-any-thunks? (cdr tree))))
        (else #f)))

(define (deep-thunk->comma tree)
  (cond ((procedure? tree) (list 'unquote (tree)))
        ((pair? tree) (cons (deep-thunk->comma (car tree))
                            (deep-thunk->comma (cdr tree))))
        (else tree)))

(define (any-procedures? tree)
  (cond ((procedure? tree) #t)
        ((pair? tree)
         (or (any-procedures (car tree))
             (any-procedures (cdr tree))))
        (else #f)))

;; The only primitive functions that return functions are lambda and repeated,
;; and these are treated specially by other parts of the program.  So the 
;; only thing we have to be careful about is quoting the result if it's a
;; symbol or list.
(define (quote-if-necessary value)
  (cond ((and (pair? value) (eq? (car value) 'unquote)) (cadr value))
        ((or (symbol? value) (list? value)) (list 'quote value))
        (else value)))

(define (quasiquote-if-necessary value)
  (cond ((and (pair? value) (eq? (car value) 'unquote)) (cadr value))
        ((any-unquoted? value) (list 'quasiquote value))
        ((or (symbol? value) (list? value)) (list 'quote value))
        (else value)))


;; Full-eval: evaluate an expression all the way to a basic value, but not
;; truly all the way.
;; This is a real kludge.  I can't just call eval and then quote-if-necessary
;; the output, because what if the value is a procedure and I really
;; want the name of the procedure as a symbol?
;; So I just repeatedly part-eval the expression until I get the same
;; answer I got last time.  This is *totally* slow, but I don't think
;; there's any way to do it, short of writing a metacircular evaluator.
;; At least it's tail recursive.

(define (full-eval exp)
  (let ((next (part-eval exp)))
    (if (equal? exp next)
        exp
        (full-eval next) )))

;; user-apply: apply a compound function to some (basic) arguments
;; Basically just substitution.

(define (substitute alist tree)
  (cond ((not (pair? tree))
         (let ((record (assq tree alist)))
           (if record (cdr record) tree) ) )
        ((eq? 'lambda (car tree))
         (substitute-lambda alist tree))
        ((eq? 'quote (car tree)) tree)
        (else (cons (substitute alist (car tree))
                    (substitute alist (cdr tree)) ))))

(define (user-apply fn-record args)
  (let ((substitute substitute))
    (substitute (make-bindings (formals fn-record) args)
                (body fn-record) ) ) )

(define (make-bindings names values)
  (cond ((null? names) 
         (if (null? values)
             '()
             (eval-error "Wrong number of arguments to function")))
        ((symbol? names) (list (cons names values)))
        ((null? values) (eval-error "Wrong number of arguments to function"))
        (else (cons (cons (car names) (car values))
                    (make-bindings (cdr names) (cdr values)) ))))

;; Lambda is hard.  If you have (define (f x) (lambda (x) (+ x 4)) )
;; then you have to be careful to leave the x as x in (+ x 4)

(define (substitute-lambda alist lam-exp)
  (let ((substitute substitute))
    (if (not (= (length lam-exp) 3))
        (eval-error "Incorrect use of LAMBDA: " lam-exp)
        (list (car lam-exp)
              (cadr lam-exp)
              (substitute (remove-bindings alist (cadr lam-exp))
                          (caddr lam-exp) )))) )

(define (remove-bindings alist names)
  (cond ((null? alist) '())
        ((member (car (car alist)) names)
         (remove-bindings (cdr alist) names))
        (else (cons (car alist) (remove-bindings (cdr alist) names))) ))


;; Basicness

;; Something is "basic" if it has been evaluated as far as it can be
;; evaluated.  If a compound expression is composed of nothing but
;; basic subexpressions, then evaluating it causes the apply to happen.
;; Otherwise, it causes the subexpressions to be evaluated.
;; The following data types are basic:
;; - Anything self-evaluating, e.g., any atom besides a symbol
;; - Names of functions and lambda expressions
;; - Any quoted data object
;; - Calls to "primitive" functions that return functions, if they don't
;;   know lambda yet.
;; Note that anything basic can be evaluated by the regular EVAL to produce
;; the value that it represents.
;; Also note that checking if a symbol is basic involves evaluating it;
;; so it will generate unbound variable errors.

(define (all-basic? exps)
  (if (null? exps)
      #t
      (and (basic? (car exps))
           (all-basic? (cdr exps)) )))

(define (all-but-one-basic? exps)
  (cond ((null? exps) #f)
        ((not (basic? (car exps))) (all-basic? (cdr exps)))
        (else (all-but-one-basic? (cdr exps)) )))

(define (basic? exp)
  (cond ((number? exp) #t)
        ((symbol? exp) (function-name? exp))
        ((null? exp) #t)
        ((not (pair? exp)) #t)
        ((eq? 'quote (car exp)) #t)
        ((eq? 'quasiquote (car exp)) #t)
        ((eq? 'lambda (car exp)) #t)
        ((eq? (car exp) 'repeated)
         (and (not *they-know-lambda*)
              (basic? (cadr exp))
              (basic? (caddr exp)) ))
        (else #f) ))


;; Compound functions
;;
;; GET-FN, which is implementation-dependent, takes a symbol as its
;; argument.  If the symbol names a compound procedure, GET-FN
;; returns a lambda expression.  If the symbol names a primitive
;; procedure, GET-FN returns the primitive procedure named.
;; GET-FN is implementation-dependent.

(define (function-name? exp)
  (and (symbol? exp)
       (not (member exp *the-real-special-forms*))
       (bound? exp)
       (procedure? (eval exp))) )


;; A lambda expression already looks like a function record.
(define (fn-record-from-lambda l-exp)
  (if (not (= (length l-exp) 3))
      (eval-error "Invalid use of LAMBDA:" l-exp)
      l-exp))

(define formals cadr)

(define (body exp)
  (if (null? (cdr exp))
      (car exp)
      (body (cdr exp))))

(define (deep-dequasiquote tree)
  (cond ((not (pair? tree)) tree)
        ((eq? (car tree) 'quasiquote) (de-quasiquote (cadr tree)))
        (else (cons (deep-dequasiquote (car tree))
                    (deep-dequasiquote (cdr tree))))))

(define (de-quasiquote tree)
  (cond ((unquoted? tree) (cadr tree))
        ((not (any-unquoted? tree)) (quote-if-necessary tree))
        ((and (pair? tree)
              (or (not (list? tree))
                  (and (= (length tree) 3)
                       (eq? (cadr tree) 'unquote))))
         (list 'cons
               (de-quasiquote (car tree))
               (de-quasiquote (cdr tree))))
        ((list? tree)
         (cons 'list (map de-quasiquote tree)))
        (else (error "This can't happen."))))

(define (unquoted? tree)
  (and (pair? tree)
       (eq? 'unquote (car tree))))

(define (any-unquoted? tree)
  (cond ((unquoted? tree) #t)
        ((pair? tree)
         (or (any-unquoted? (car tree))
             (any-unquoted? (cdr tree))))
        (else #f)))

(define book-primitive?
  (if *grillmeyer*       
      ; Grillmeyer version
      (let* 
        ((names '(* + - / < <= = > >= abs acos append
                    asin assoc atan boolean?
                    car cdr caar cadr cdar cddr
                    caaar caadr cadar caddr cdaar cdadr cddar cdddr
                    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                    ceiling cons cos count equal? even?
                    exit exp expt floor gcd integer? lcm
                    length list list-ref list? log max member
                    min modulo not null? number?
                    odd? procedure? quotient random remainder round
                    sin sqrt tan truncate
                    first second third fourth fifth rest atom?
                    list-head subseq position remove count rassoc
                    intersection union set-difference subset? adjoin))
         (procs (map eval names)))
        (lambda (name value)
          (define (helper names procs)
            (cond ((null? names) #f)
                  ((eq? (car names) name)
                   ;; Make sure it hasn't been redefined
                   (eq? (car procs) value))
                  (else (helper (cdr names) (cdr procs)))))
          (helper names procs)))
      
      ; Harvey+Wright version
      (let* 
        ((names '(* + - / < <= = > >= abs align acos appearances append
                    asin assoc atan before? bf bl boolean? butfirst
                    butlast car cdr caar cadr cdar cddr
                    caaar caadr cadar caddr cdaar cdadr cddar cdddr
                    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
                    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
                    ceiling children cons cos count datum empty? equal? even?
                    exit exp expt first floor gcd integer? item last lcm
                    length list list-ref list? log make-node max member
                    member? min modulo not null? number?
                    odd? procedure? quotient random remainder round se
                    sentence sentence? sin sqrt tan truncate word word?))
         (procs (map eval names)))
        (lambda (name value)
          (define (helper names procs)
            (cond ((null? names) #f)
                  ((eq? (car names) name)
                   ;; Make sure it hasn't been redefined
                   (eq? (car procs) value))
                  (else (helper (cdr names) (cdr procs)))))
          (helper names procs)))
      ) )

;==============================================================================
; Interface between the replacement modeler and the STk interpreter
;==============================================================================

;; get-fn for stk:
; This version of the replacement modeler uses STk's "procedure-body" function
; to get at the innards of user-defined procedures.

(define (get-fn name)
  (let ((value (eval name)))
    (if (book-primitive? name value)
        value
        (procedure-body value))))

; We assume a variable is unbound if we catch an error trying to eval it.
(define (bound? x) (not (catch (eval x))))

; Errors are added to the list named *the-errors*, which is checked by the
; front-end after each expression is evaluated.
(define *the-errors* '())

(define (eval-error . args)
  (let ((errtext (apply & (cons "*** Error: " args))))
    (if (not (member errtext *the-errors*))
      (set! *the-errors* (cons errtext *the-errors*)))))

;==============================================================================
; Front end console code
;==============================================================================
; This is based on stk/Lib/console.stk, with added code to find parenthesized
; expressions and call the replacement modeler backend.

; This deselects the current selection and moves the insertion point of
; the console to the position POS.
(define (set-cursor console pos)
  (let ((pos (if (console 'compare pos "==" "end") "end - 1 chars" pos)))
    (console 'mark 'set 'insert pos)
    (console 'tag 'remove 'sel "1.0" "end")
    (console 'see "insert")))

; This array is used to keep track of where the front-end code has inserted
; expressions. The start of each line which begins a new expression is
; pushed on this array as if it were a stack, with the older ones at the end.
(define *exprlinestarts* '())

; Returns true if CONSOLE contains any expressions; false otherwise.
(define (any-more-line-starts? console)
	(> (length *exprlinestarts*) 0))

; Returns the position of the beginning of the first expression in the window
; referenced by CONSOLE, or (1 . 0) if there is no such expression.
(define (first-expr-line-start console)
  (if (> (length *exprlinestarts*) 0)
    (list-ref *exprlinestarts* (- (length *exprlinestarts*) 1))
    (cons 1 0)))

; Returns the position of the beginning of the second expression in the window,
; or the string "END" if there is no such expression.
(define (second-expr-line-start console)
  (if (> (length *exprlinestarts*) 1)
    (list-ref *exprlinestarts* (- (length *exprlinestarts*) 2))
    "end"))

; Returns the position of the beginning of the last expression in the window
; designated by CONSOLE. Only works if (any-more-line-starts? console) is true.
(define (last-expr-line-start console)
	(car *exprlinestarts*))

; Positions are "indices" as documented in the Tk "text" widget's man page.
; A range is an ordered pair of positions.
;
; Searches through the list *exprlinestarts* for two positions, one which
; is less than or equal to all positions in RANGE, the other of which is
; greater than or equal to all positions in RANGE. A range formed from
; such a pair of positions is said to "surround" RANGE, if there are no
; positions in *exprlinestarts* between them.
;
; Returns the surrounding range if one is found; false otherwise.
(define (line-starts-surrounding console range)
  (define (helper console range l)
    (if (> (length l) 1)
	  (if (and (console 'compare (cdr range) "<=" (car l))
	           (console 'compare (car range) ">=" (cadr l)))
          (cons (cadr l) (console 'index (& (car l) " - 1 char")))
          (helper console range (cdr l)))
      #f))
  (helper console range (cons "end" *exprlinestarts*)))

; Returns true if the surrounding range of OLD-RANGE in CONSOLE is not the
; same as the surrounding range of NEW-RANGE, false otherwise. See the
; comment describing line-starts-surrounding for more info. 
(define (violates-line-starts? console old-range new-range)
     (let ((lr-old (line-starts-surrounding console old-range))
           (lr-new (line-starts-surrounding console new-range)))
       (if (not (equal? lr-old lr-new)) #t #f)))

; Removes the position corresponding to the beginning of the last expression
; in the window designated by CONSOLE from *exprlinestarts*. Only works if
; (any-more-line-starts? console) is true.
(define (remove-last-expr-line-start! console)
    (set! *exprlinestarts* (cdr *exprlinestarts*)))

; Adds the position POS, corresponding to the beginning of a new expression
; added to the end of the window designated by CONSOLE, to *exprlinestarts*.
; Used by console-append-expr.
(define (add-last-expr-line-start! pos console)
    (set! *exprlinestarts* (cons pos *exprlinestarts*)))

; Appends the string S to the window designated by CONSOLE.
(define (console-append console s)
  (unless (zero? (string-length s))
    ; 1. Raise window
    (raise (winfo 'top console))
    ; 2. Do text insertion
    (console 'mark 'set "insert" "end")
    (console 'insert "insert" s "input")
    (console 'see "insert")))

; Appends the string S to the window designated by CONSOLE, and adds
; the position of the beginning of the line where S starts to the
; *exprlinestarts* array, using add-last-expr-line-start!.
(define (console-append-expr console s)
  (unless (zero? (string-length s))
    ; 1. Raise window
    (raise (winfo 'top console))
	(add-last-expr-line-start! (console 'index "end - 1 char") console)
    ; 2. Do text insertion
    (console 'mark 'set "insert" "end")
    (console 'insert "insert" s "input")
    (console 'see "insert")))

; Returns the number of characters in the string S which are equal to CH.
(define (string-count-matching-chars s ch)
  (define (count-iter s ch cur-count)
    (if (equal? s "") 
      cur-count
      (let ((rest-of-s (substring s 1 (string-length s))))
        (if (equal? (string-ref s 0) ch)
            (count-iter rest-of-s ch (+ 1 cur-count))
            (count-iter rest-of-s ch cur-count)))))
  (count-iter s ch 0))

; Returns the number of left parentheses in the string S.
(define (string-count-lparens s)
  (string-count-matching-chars s #\())

; Returns the number of right parentheses in the string S.
(define (string-count-rparens s)
  (string-count-matching-chars s #\)))

; Returns the number of parentheses in the string S.
(define (string-count-parens s)
  (+ (string-count-lparens s) (string-count-rparens s)))

; Returns true if S is a string with balanced parentheses.
(define (single-paren-expr? s)
  (define (lparen? ch) (equal? ch #\())
  (define (rparen? ch) (equal? ch #\)))
  (define (paren? ch) (or (lparen? ch) (rparen? ch)))
  (define (s? s level)
    (if (equal? s "")
      (equal? level 0)
      (let ((first-of-s (string-ref s 0))
            (rest-of-s (substring s 1 (string-length s))))
        (cond
         ((and (equal? level 0) (not (paren? first-of-s))) #f)
         ((lparen? first-of-s) (s? rest-of-s (+ level 1)))
         ((rparen? first-of-s) (s? rest-of-s (- level 1)))
         (else (s? rest-of-s level))))))
  (and (not (equal? (string-length s) 0)) (s? s 0)))

; Returns the index of the next left parenthesis in the specified CONSOLE
; after FROM-POINT.
(define (forwards-find-lparen console from-point)
  (console 'index
    (let ((maybe-result (console 'search :forwards "(" from-point)))
      (if (console 'compare maybe-result ">" from-point)
          maybe-result
          from-point))))

; Returns the index of the next right parenthesis in the specified CONSOLE
; after FROM-POINT.
(define (forwards-find-rparen console from-point)
  (console 'index
    (let ((maybe-result (console 'search :forwards ")" from-point)))
      (if (console 'compare maybe-result ">" from-point)
          maybe-result
          from-point))))

; Returns the index of the next left parenthesis in the specified CONSOLE
; before FROM-POINT.
(define (backwards-find-lparen console from-point)
  (console 'index
    (let ((maybe-result (console 'search :backwards "(" from-point)))
      (if (console 'compare maybe-result "<" from-point)
          maybe-result
          from-point))))

; Returns the index of the next right parenthesis in the specified CONSOLE
; before FROM-POINT.
(define (backwards-find-rparen console from-point)
  (console 'index
    (let ((maybe-result (console 'search :backwards ")" from-point)))
      (if (console 'compare maybe-result "<" from-point)
          maybe-result
          from-point))))

; If position POINT in CONSOLE points to either of the parentheses in an
; empty list -- that is, "()" -- then return the range that surrounds 
; the empty list, possibly including a preceding quote. If POINT does not
; point to an empty list, then return #f.
(define (emptylist console point)
 (let ((point-next (& point " + 1 char"))
       (point-next2 (& point " + 2 chars"))
       (point-prev (& point " - 1 char")))
  (if (and (equal? (string-count-lparens (console 'get point point-next)) 1)
       (equal? (string-count-rparens (console 'get point-next point-next2)) 1))
   (maybe-include-quote-in-range console (cons point point-next))
   (if (and (equal? (string-count-rparens (console 'get point point-next)) 1)
        (equal? (string-count-lparens (console 'get point-prev point)) 1))
    (maybe-include-quote-in-range console (cons point-prev point))
    #f))))

; Returns the text in the specified CONSOLE delimited by the specified RANGE
(define (get-text-span console range)
  (console 'get (car range) (& (cdr range) " + 1 char")))

; Returns the text in the specified CONSOLE which is currently selected.
(define (get-selected-text console)
  (get-text-span console (cons "sel.first" "sel.last - 1 char")))

; Returns a true value if the specified CONSOLE has no text currently selected.
(define (selection-empty? console)
  (null? (console 'tag 'ranges 'sel)))

; Causes the specified CONSOLE to have nothing selected.
(define (remove-selection console)
  (if (not (selection-empty? console))
    (console 'tag 'remove 'sel "1.0" "end")))

; Causes the specified CONSOLE to select the text in the specified RANGE.
(define (select-range! console range)
  (set-cursor console (car range))
  (remove-selection console)
  (console 'tag 'add 'sel (car range) (& (cdr range) " + 1 char")))

; Returns a true value if X is a Scheme expression of the form (quote ...)
(define (quoted-expr? x)
  (and (pair? x) (equal? (car x) 'quote)))

; Returns the eof symbol for this Scheme interpreter.
(define (make-eof)
	(with-input-from-string "" (lambda () (read))))

; Joins a list of strings L together into a single string,
; with DELIMITER inserted between each pair of strings.
; Returns the resulting single string. (Named after 
; Perl's "join" operator, whose function it mimics.)
(define (join delimiter l)
	(define (join0 str l)
		(if (null? l)
			str
			(if (equal? (string-length str) 0)
				(join0 (car l) (cdr l))
				(join0 (string-append str delimiter (car l)) (cdr l)))))
	(join0 "" l))

; This is the function called when you press Return or Enter.
; (If with Return, evaluator == part-eval; if with Enter, evaluator ==
; full-eval). The effect is to evaluate either the selection or the last
; expression in the window, and append the result to the window.
; If there are errors, nothing is appended to the window; instead, the
; errors are popped up in another window using static-text-box.
; 
; This is actually basically an evaluator-binding local variable.
(define *last-clicked-expr* #f)
;
(define (evaluator-binding console evaluator)
  (lambda ()
     (if (selection-empty? console)
       (select-range! console (cons (last-expr-line-start console) "end")))
     (let ((selected-exprlinestarts (line-starts-surrounding console (cons "sel.first" "sel.last"))))
     (with-input-from-string
       (get-selected-text console)
       (lambda () (set! *last-clicked-expr* (read))))
     (when (not (equal? (make-eof) *last-clicked-expr*))
       (set! *the-errors* '())
       (let* ((prefix
                (if (console 'compare (car selected-exprlinestarts) "==" "sel.first")
                    ""
                    (get-text-span console
                      (cons (car selected-exprlinestarts) "sel.first - 1 char"))))
              (value (evaluator *last-clicked-expr*))
              (suffix (get-text-span console
                        (cons "sel.last" (cdr selected-exprlinestarts))))
              (stringification (with-output-to-string
                                 (lambda ()
                                   (display prefix)
                                   (write value)
                                   (display suffix)))))
         (if (not (null? *the-errors*))
           (static-text-box "Evaluation Errors" (join "\n" *the-errors*))
           (begin
            (erase-other-lines console (car selected-exprlinestarts))
            (console-append console "\n")
            (console-append-expr console
              (pp
                (with-input-from-string stringification (lambda () (read))) #f 'yes-string)) ) )))
     (remove-selection console)
     'break)))

; Remove lines between the selected line and the end
(define (erase-other-lines console selline)
  (if (and (any-more-line-starts? console)
           (= (car (last-expr-line-start console)) (car selline)))
      'done
      (begin (console 'delete (last-expr-line-start console) "end")
             (remove-last-expr-line-start! console)
             (erase-other-lines console selline))))



; Returns true if the point (X,Y) in the specified CONSOLE is past the end of
; the line of text containing the Y coordinate.
(define (past-end-of-line? console x y)
  (let* ((point (& "@" x "," y))
         (lineinfo (console 'dlineinfo point))
         (startx (list-ref lineinfo 0))
         (width (list-ref lineinfo 2)))
         (> x (+ width startx))))

; Given CONSOLE with an expression selected, deselect that expression, and
; select the expression before it.
(define (select-previous-expr console)
  (let* ((previous-rparen (backwards-find-rparen console "sel.first"))
         (previous-lparen (backwards-find-lparen console previous-rparen))
         (previous-range (cons previous-lparen previous-rparen)))
   (if (single-paren-expr? (get-text-span console previous-range))
       (select-range! console
		(maybe-include-quote-in-range console previous-range))
       (select-expr-enclosing-point! console (& previous-rparen " - 1 char")))))

; Given CONSOLE with an expression selected, deselect that expression, and
; select the expression after it.
(define (select-next-expr console)
  (let* ((next-lparen (forwards-find-lparen console "sel.last"))
         (next-rparen (forwards-find-rparen console next-lparen))
         (next-range (cons next-lparen next-rparen)))
   (if (single-paren-expr? (get-text-span console next-range))
       (select-range! console
		(maybe-include-quote-in-range console next-range))
       (select-expr-enclosing-point! console (& next-lparen " + 1 char")))))

; Given CONSOLE with an expression selected, deselect that expression, and
; select the first expression inside it (scanning from left to right).
(define (select-first-enclosed-expr console)
  (unless (equal? (string-count-parens (get-selected-text console)) 2)
    (let ((foo (forwards-find-lparen console
                 (& (car (maybe-exclude-quote-from-range console
                           (cons "sel.first" "sel.last"))) " + 1 char"))))
    (select-range! console (expr-enclosing-range console
                                (cons (& foo " + 1 char") (& foo " + 1 char")))))))

; Given a CONSOLE, select the last expression in it.
(define (select-last-expr console)
	(let ((lparen (backwards-find-lparen console "end")))
	  (select-range! console (cons lparen "end"))))

; Returns a true value if the text delimited by the specified RANGE in the 
; given CONSOLE is preceded immediately by a CHAR.
(define (range-preceded-by? console range char)
  (equal? (char-before-range console range) char))

; Returns a true value if the text delimited by the specified RANGE in the 
; given CONSOLE starts with a CHAR.
(define (range-begins-with? console range char)
  (equal? (char-beginning-range console range) char))

; Returns a true value if the text delimited by the specified RANGE in the 
; given CONSOLE is followed immediately by a CHAR.
(define (range-followed-by? console range char)
  (equal? (char-after-range console range) char))

; Returns the character which immediately precedes the text delimited by
; the specified RANGE in the given CONSOLE.
(define (char-before-range console range)
  (let ((prev-char-loc (& (car range) " - 1 char")))
    (safe-string-ref (console 'get prev-char-loc (car range)) 0)))

; Returns the character which immediately follows the text delimited by
; the specified RANGE in the given CONSOLE.
(define (char-after-range console range)
  (let ((next-char-loc (& (cdr range) " + 1 char")))
    (safe-string-ref (console 'get (cdr range) next-char-loc) 0)))

; Returns the character which begins the text delimited by
; the specified RANGE in the given CONSOLE.
(define (char-beginning-range console range)
  (let ((next-char-loc (& (car range) " + 1 char")))
    (safe-string-ref (console 'get (car range) next-char-loc) 0)))

; Returns the same as (string-ref s posn), or #f if it would have caused an
; error.
(define (safe-string-ref s posn)
  (if (< (string-length s) (+ posn 1))
    #f
    (string-ref s posn)))

; If the text delimited by the specified RANGE in the given CONSOLE is
; preceded by a quote ('), move the beginning of the range back by one
; character (thereby including the quote) and return it; otherwise return
; the range unchanged.
(define (maybe-include-quote-in-range console range)
  (if (range-preceded-by? console range #\')
    (cons (& (car range) " - 1 char") (cdr range))
    range))

; If the text delimited by the specified RANGE in the given CONSOLE
; starts with a quote ('), move the beginning of the range forward by one
; character (thereby excluding the quote) and return it; otherwise return
; the range unchanged.
(define (maybe-exclude-quote-from-range console range)
  (if (range-begins-with? console range #\')
    (cons (& (car range) " + 1 char") (cdr range))
    range))

; If the text delimited by the specified RANGE in the given CONSOLE
; is followed by a newline, move the end of the range forward by one
; character (thereby including the newline) and return it; otherwise return
; the range unchanged.
(define (maybe-include-newline-in-range console range)
  (if (range-followed-by? console range #\newline)
    (cons (car range) (& (cdr range) " + 1 char"))
    range))

; Given a console and a range, extend the range forwards to include the next
; right parenthesis to the right of it.
(define (seek-forwards console range)
  (cons (car range) (forwards-find-rparen console (& (cdr range) " + 1 char"))))

; Given a console and a range, extend the range backwards to include the next
; left parenthesis to the left of it.
(define (seek-backwards console range)
  (cons (backwards-find-lparen console (car range)) (cdr range)))

; Given a console where there is currently a selection, find the expression
; that encloses the selection, and select it.
(define (select-enclosing-expr console)
  (select-range! console (expr-enclosing-range console
                           (cons "sel.first" "sel.last - 1 char"))))

; Returns the expression enclosing the range CURRENT-RANGE.
(define (expr-enclosing-range console current-range)
  (let ((original-line-starts (line-starts-surrounding console current-range))
        (resulting-range (seek-forwards console
                           (seek-backwards console current-range))))
    (if (violates-line-starts? console current-range resulting-range)
      original-line-starts
      (verify-helper console resulting-range original-line-starts))))

;;attempt to get selection to work properly
(define (true-balance sent)
  (true-balance-h sent 1 0 (string-length sent)))  

(define (true-balance-h str pos stat strlen)        
  (cond ((equal? #\newline (string-ref str (- strlen 1)))
         (true-balance-h str pos stat (- strlen 1)))
        ((>= (+ pos 1) strlen)
         stat)
        ((< stat 0)
         stat)
        ((equal? #\( (string-ref str pos))
         (true-balance-h str (+ 1 pos) (+ 1 stat) strlen))
        ((equal? #\) (string-ref str pos))
         (true-balance-h str (+ 1 pos) (- stat 1) strlen))
        (else (true-balance-h str (+ 1 pos) stat strlen))))



; A helper procedure of expr-enclosing-range. Tries to expand CURRENT-RANGE
; until it contains a whole Scheme expression, but without crossing the
; borders demarcated by the range ORIGINAL-LINE-STARTS.
(define (verify-helper console current-range original-line-starts)
  (let* ((the-text (get-text-span console current-range))
        (num-lparens (string-count-lparens the-text))
        (num-rparens (string-count-rparens the-text))
        (balance-score (true-balance the-text)))
    (if (= 0 balance-score)
        ;;(equal? num-lparens num-rparens)
      (maybe-include-quote-in-range console current-range)
      (let ((resulting-range
        (if 
            (> balance-score 0)
            ;;(> num-lparens num-rparens)
          (seek-forwards console current-range)
        (if 
            (< balance-score 0)
            ;;(< num-lparens num-rparens)
          (seek-backwards console current-range)))))
        (if (violates-line-starts? console current-range resulting-range)
          original-line-starts
          (verify-helper console resulting-range original-line-starts))))))

; Given a console and a point, return the range which delimits the expression
; which encloses the point.
(define (expr-enclosing-point console point)
    (let ((emptylistcheck (emptylist console point)))
     (if (pair? emptylistcheck)
      emptylistcheck
	  (expr-enclosing-range console (cons point point)))))

; Given a console and a point, select the expression which encloses the point.
(define (select-expr-enclosing-point! console point)
  (select-range! console (expr-enclosing-point console point)))

;=============================================================================
; Help menu dialogs
;=============================================================================

; This is used to display the about window.
(define (modeler-about)
  (static-text-box "About the Modeler" "
This is the Replacement Modeler for STk 4.0.1.  Written July 2000 - Jan
2002 by Brian Gaeke <brg@EECS.Berkeley.EDU>, based on the version 1.5
modeler by Harvey & Wright and the STk 4.0.1 console library.

This Replacement Modeler is Version 2.0$Revision: 1.6 $ $Date: 2002/09/22 11:43:15 $
"))

; This is used to display the help window.
(define (modeler-help)
  (static-text-box "Modeler Help" "
To start the replacement modeler, use the 'model' special form.
(model EXP) will start up a new Replacement Modeler window containing
the Scheme expression EXP, which you can manipulate.

Keystroke & mouse commands:  Clicking on a s-exp will select it, and
clicking past the end of the line or hitting Escape will cause nothing to be
selected.

Hit Enter to do a full replacement-model evaluation of the current line,
or hit Return to do only one level of replacement. Hit Backspace to
remove the last expression from the window.

Left & right arrow keys let you select preceding and following expressions
to the one which is selected, respectively, and up and down arrow keys
let you select the enclosing and first enclosed expressions, respectively.

Menu commands:  You can save a transcript of your session with the
replacement modeler using the File menu 'Save As' option; you can also
copy the currently selected text out of the replacement modeler into
the Clipboard or cut buffer by using the Edit menu 'Copy' option.

You can go back to the underlying Scheme interpreter and end your session
with the replacement modeler using the File menu 'Quit' option.

You can change the size of the display face used for expressions in the
modeler window using the items in the 'Font' menu.
"))

; This makes a simple dialog box with a title that contains a message.
(define (static-text-box window-title message-text)
  (let* ((top (toplevel (gensym ".statictext_")))
         (msg (label (& top ".text")))
         (ok (button (& top ".ok") :text "OK"
                     :command (lambda () (destroy top)))))
    (wm 'title top window-title)
    (pack msg)
    (pack ok)
    (msg 'configure :text message-text :justify "left")
    (make-undefined)))

;=============================================================================
; Font stuff and menu stuff.
;=============================================================================

; Ask the user to give us a file name, and then save the console contents
; as text in the file.
(define (save-as console)
  (let ((filename (Tk:get-save-file)))
    (with-output-to-file filename
      (lambda ()
        (display (console 'get "1.0" "end"))))))

; This function sets up all the fonts and menus.
(define (setup-console-fonts top console)
  (define (make-named-font real-font)
    (let ((real-font-parameters (font 'actual real-font)))
      (eval `(font 'create (gensym "namedfont") ,@real-font-parameters))))
  (let* ((menubar (menu (& top ".menu") :tearoff 0))
        (filemenu (menu (& menubar ".file") :tearoff 0))
        (editmenu (menu (& menubar ".edit") :tearoff 0))
        (fontmenu (menu (& menubar ".font") :tearoff 0))
        (helpmenu (menu (& menubar ".help") :tearoff 0))
        (weight-bold? #f)
        (slant-italic? #f)
        (underline? #f))
    ; Set up the menu bar
    (top 'configure :menu menubar)
    ; Set up the File menu
    (menubar 'add 'cascade :label "File" :menu filemenu)
    (filemenu 'add 'command :label "Save as..."
      :command (lambda () (save-as console)))
    (filemenu 'add 'command :label "Quit"
      :command (lambda () (destroy top)))
    ; Set up the Edit menu
    (menubar 'add 'cascade :label "Edit" :menu editmenu)
    (editmenu 'add 'command :label "Copy"
		:command (lambda ()
                   (if (not (selection-empty? console))
                     (let ((text (get-selected-text console)))
                       (clipboard 'clear :displayof top)
                       (clipboard 'append :displayof top :- text)))))
    (editmenu 'add 'command :label "Select All"
		:command (lambda ()
                   (select-range! console (cons "1.0" "end"))))
    ; Set up the Console font
    (console 'configure :font (make-named-font "courier 14"))
    ; Set up the Font menu
    (menubar 'add 'cascade :label "Font" :menu fontmenu)
    (fontmenu 'add 'command :label "Courier 12" :command
		(lambda () (console 'configure :font (make-named-font "courier 12"))))
    (fontmenu 'add 'command :label "Courier 14" :command
		(lambda () (console 'configure :font (make-named-font "courier 14"))))
    (fontmenu 'add 'command :label "Courier 18" :command
		(lambda () (console 'configure :font (make-named-font "courier 18"))))
    (fontmenu 'add 'command :label "Courier 24" :command
		(lambda () (console 'configure :font (make-named-font "courier 24"))))
    (fontmenu 'add 'command :label "Courier 36" :command
		(lambda () (console 'configure :font (make-named-font "courier 36"))))
    (fontmenu 'add 'command :label "Courier 48" :command
		(lambda () (console 'configure :font (make-named-font "courier 48"))))
    (fontmenu 'add 'command :label "Courier 54" :command
		(lambda () (console 'configure :font (make-named-font "courier 54"))))
    (fontmenu 'add 'command :label "Lucida Typewriter 24" :command
                (lambda () (console 'configure :font (make-named-font"lucidatypewriter 24"))))
    ; Set up the Help menu
    (menubar 'add 'cascade :label "Help" :menu helpmenu)
    (helpmenu 'add 'command :label "About the Modeler..."
      :command (lambda () (modeler-about)))
    (helpmenu 'add 'command :label "Modeler Help..."
      :command (lambda () (modeler-help)))))

;=============================================================================
; General initialization code.
;=============================================================================

; Sets up the keyboard and mouse bindings for the replacement modeler window.
(define (init-modeler-bindings console)
  (let ((null-binding (lambda () 'break)))
    ;; Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
    ;; Otherwise, if a widget binding for one of these is defined, the
    ;; <KeyPress> class binding will also fire and insert the character,
    ;; which is wrong.  Ditto for <Escape>.
    (bind console "<KeyPress>" null-binding)
    (bind console "<Alt-KeyPress>" null-binding)
    (bind console "<Meta-KeyPress>" null-binding)
    (bind console "<Control-KeyPress>" null-binding)
    ; You can hit escape to select nothing.
    (bind console "<Escape>"
        (lambda (|W| x y)
           (remove-selection console)
           'break))
    ; Evaluating expressions:
	; Enter is used for a full evaluation, and
	; Return is used for one step of the evaluation.
    (bind console "<KP_Enter>" (evaluator-binding console full-eval))
    (bind console "<Return>" (evaluator-binding console part-eval))
    ; Backspace is used to select and delete the last expr.
    (bind console "<BackSpace>"
      (lambda (|W| x y)
        (when (any-more-line-starts? console)
			(console 'delete (last-expr-line-start console) "end")
			(remove-last-expr-line-start! console))
;            (console-append console "\n"))
  	    'break))
    ; Navigating expressions
	; Left arrow: select previous subexpression before selection.
    (bind console "<Left>"
      (lambda (|W| x y)
        (if (selection-empty? console)
            (select-range! console (cons (last-expr-line-start console) "end"))
            (select-previous-expr console))
        'break))
	; Right arrow: select next subexpression after selection.
    (bind console "<Right>"
      (lambda (|W| x y)
        (if (selection-empty? console)
            (select-range! console (cons (first-expr-line-start console)
                                (second-expr-line-start console)))
            (select-next-expr console))
        'break))
	; Up arrow: select expression enclosing current selection.
    (bind console "<Up>"
      (lambda (|W| x y)
        (if (selection-empty? console)
            (bell)
            (select-enclosing-expr console))
        'break))
	; Down arrow: select first expression enclosed by current selection, if any.
    (bind console "<Down>"
      (lambda (|W| x y)
        (if (selection-empty? console)
            (bell)
            (select-first-enclosed-expr console))
        'break))
    ; Clicking mouse selects expression you clicked on, or removes the
	; selection if you clicked outside the line.
    (bind console "<ButtonRelease-1>" (lambda (|W| x y) 
  		(let ((point (& "@" x "," y)))
            (if (past-end-of-line? console x y)
              (remove-selection console)
  			(select-expr-enclosing-point! console point)))
          (focus console)
          'break))
    (bind console "<Motion>" null-binding)
    (bind console "<ButtonPress-1>" (lambda (|W| x y) 
  		(let ((point (& "@" x "," y)))
            (if (past-end-of-line? console x y)
              (remove-selection console)
  			(select-expr-enclosing-point! console point)))
          (focus console)
          'break)) ))

; Main modeler initialization routine.
; This is called by the (model ...) macro.
(define (init-modeler expr)
  (let* ((top     (toplevel (gensym ".modeler_") :class "STk"))
	 (console (text (& top ".txt") :background "blue" :setgrid #t))
	 (sb	  (scrollbar (& top ".sb") :width 10)))
    ;; Associate the scrollbar commands
    (tk-set! sb :command      (lambda l (apply console 'yview l)))
    (tk-set! console :yscroll (lambda l (apply sb 'set l)))

    ;; Set up fonts
    (setup-console-fonts top console)

    ;; Pack stuff
    (pack console :expand #t :fill "both" :side "left")
    (pack sb :expand #f :fill "y" :side "left")
	; Set the title for the window.
    (wm 'title top "Replacement modeler")

	; Other random questionably-useful stuff.
    (console 'mark 'set "output" (console 'index "end - 1 char"))
    (set-cursor console "end")
    (console 'mark 'set "prompt-end" "insert")
    (console 'mark 'gravity "prompt-end" "left")

	; Set up keyboard and mouse controls.
	(init-modeler-bindings console)
	; Put the initial expression in the window.
	(console-append-expr console (pp expr #f 'yes-string))
    (focus console)
    console))

;=============================================================================
; User entry points.
;=============================================================================

; These can be used for testing purposes -- just uncomment, reload and say (go).
(define (go)
	(init-modeler
		'(list 'ab (list 'cd 'ef) '() (list (list 'gh 'ij) (list 'kl)))))
(define (go2)
	(init-modeler
		'(cond ((= 1 2) #f) ((= 1 1) #t))))

; Set up the macro used by students to run the modeler.
(define-macro (model expr)
   `(begin (init-modeler ',expr) (make-undefined)))

; EOF

