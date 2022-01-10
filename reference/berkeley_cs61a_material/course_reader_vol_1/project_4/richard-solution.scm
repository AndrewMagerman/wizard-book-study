;; Problem A1   make-line-obj

;; Create a LINE-OBJECT class with methods empty? next and put-back

(define-class (LINE-OBJECT tokenizedInput)
    (method (empty?) (null? tokenizedInput))
    (method (next) 
        (let ((first-token (car tokenizedInput)))
            (set! tokenizedInput (cdr tokenizedInput))
            first-token ) )
    (method (put-back tkn) 
        (set! tokenizedInput (cons tkn tokenizedInput) ) )
    (method (show) 
        (begin
            (map (lambda (t) (begin (display t) (display " "))) tokenizedInput ) )
            (newline)
            (void)
        )
)

;; tests for the LINE-OBJECT class
(define (assert-eq expected actual message)
  (if (equal? expected actual)
      'success
      (error message "expected" expected "actual" actual)))
      
(define Test-LINE-OBJECT-empty?-1 (instantiate LINE-OBJECT (list 'a 'b 'c)))
(assert-eq #f (ask Test-LINE-OBJECT-empty?-1 'empty?) 'Test-LINE-OBJECT-empty?-1)
(define Test-LINE-OBJECT-empty?-2 (instantiate LINE-OBJECT '()))
(assert-eq #t (ask Test-LINE-OBJECT-empty?-2 'empty?) 'Test-LINE-OBJECT-empty?-2)

(define Test-LINE-OBJECT-next-1 (instantiate LINE-OBJECT (list 'a 'b 'c)))
(assert-eq 'a (ask Test-LINE-OBJECT-next-1 'next) 'Test-LINE-OBJECT-next-1)
(assert-eq (list 'b 'c) (ask Test-LINE-OBJECT-next-1 'tokenizedInput) 'Test-LINE-OBJECT-next-1)

(define Test-LINE-OBJECT-put-back-1 (instantiate LINE-OBJECT (list 'a 'b 'c)))
(ask Test-LINE-OBJECT-put-back-1 'put-back 'new)
(assert-eq (list 'new 'a 'b 'c) (ask Test-LINE-OBJECT-put-back-1 'tokenizedInput)  'Test-LINE-OBJECT-put-back-1)


;; replace Brian Harvey's make-line-obj procedure
(define (make-line-obj text)
    (instantiate LINE-OBJECT text))
  ;;(error "make-line-obj not written yet!"))

;; tests
 (define Test-make-line-obj (make-line-obj (list 'a 'b 'c)) )
 (assert-eq (list 'a 'b 'c) (ask Test-make-line-obj 'tokenizedInput)  'Test-make-line-object)
  
;;; Problem A2   logo-type


;; RE: logo-type will be invoked with a word or with a list. 
;; It must not terminate with a newline
;; Sublists must be surrounded by square brackets
;; I choose to print the value or call a list printer if there is a list

(define (logo-type val)
    (if (list? val) 
        (logo-type-list val)
        (display val)
    )
    '=no-value=
)

;; The list printer knows it gets a list and will use a map function to call a
;; lambda on each element of the list.
;; The challenge is the requirement to print square brackets around sublists as in:
;; ? print [this is [a nested] list]
;; this is [a nested] list
;; The solution is to test if the element is a sublist and if so print the square brackets
;; and call the logo-type-list function recursively for the sublist.
;; The next challenge is not to have a trailing space character at the end of the list.
;; So we test if the element is the last element of the list and then choose to print nothing
;; if it is the last element or a space if it is not.

(define (logo-type-list lst)
    (map (lambda (t) 
        (begin 
            (if (list? t)
                (begin 
                    (display "[")
                        (logo-type-list t)
                        (display "]")
                    )
                (display t) 
            )
            (if (eq? t (last lst))
                (display "")
                (display " ")
            )
        )
        )
        lst
    )
)


;; tests
;; (logo-type 'hello)
;; (logo-type (list '"[" 'a '"[" 'b 'c '"]" 'd '"]") )


;; Problem B1    eval-line
;; Logo-eval's job is to evaluate one instruction or expression and return
;; its value.

(define (eval-line line-obj env)
    (if (ask line-obj 'empty?) 
        '=no-value=
        ;; get the LOGO command i.e. print
        (let ((token (ask line-obj 'next)))
            ;; figure out how many arguments the LOGO command needs i.e. print needs 1 
            (let ((proc (lookup-procedure token)))
                (if (not proc) 
                    (error "I don't know how  to " token)
                    (let ((number-of-arguments (arg-count proc)))
                        ;; collect-n-args pulls the needed number of arguments of out line-obj list
                        ;; then we cons the token to the args and package it up into a LINE-OBJECT.
                        ;; finally we ask logo-eval to interpret it
                        (logo-eval (instantiate LINE-OBJECT (cons token (collect-n-args (abs number-of-arguments) line-obj env))) env)
                        (eval-line line-obj env)
					)
                )
            )
        )
    )
)

;; tests
(define empty-line-obj (instantiate LINE-OBJECT '()))
(assert-eq '=no-value= (eval-line empty-line-obj '()) 'Test-empty-line-obj)


;; Problem B2   logo-pred

;; Your job is to write the higher-order function LOGO-PRED whose argument is
;; a Scheme predicate and whose return value is a Logo predicate, i.e., a
;; function that returns TRUE instead of #T and FALSE instead of #F.

;; Note that logo-pred returns a function, not the result of a function.
;; So, in the second lambda notice how the if is not a (if. If it had a brace in
;; front of it, the if would execute and we would return '[true] or '[false]. The
;; brace in front of the if is the opening body of the lambda.

(define (logo-pred pred)
    (lambda args (
        (if (eq? (length args) 1)
            (lambda () ( if (pred (car args) ) '[true] '[false]) )
            (lambda () ( if (pred (car args) (car(cdr args)) ) '[true] '[false]) )
        )
    ))
)

;; Redefining the function to the 'spiffy' version that can handle any number of arguments unlike
;; the first version above which can only deal with 1 or 2 argument predicate functions.
;; Thanks Corinna for the apply hint.
;; Again note that this function returns an evaluatable function. That's why there is no opening brace in
;; front of the if. The if function will execute the (apply function using the supplied predicate function
;; with all the arguments. The predicate function will return #t or #f in the Scheme binary type.
;; The if function will return the string '[true] or '[false]
(define (logo-pred pred)
    (lambda args (
        if (apply pred args) '[true] '[false]
    ) )
)

;; ((logo-pred (make-logo-arith equalp)) 2 2)
;; ((logo-pred (make-logo-arith equalp)) 2 3)
;; ((logo-pred empty?) '())
