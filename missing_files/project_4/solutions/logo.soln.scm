;;; logo.soln.scm         part of programming project #3  SOLUTIONS

;;; (Problems B1, A3, B4, 5, and 6 are entirely in logo-meta.scm)

;;; Problem A1   make-line-obj

(define-class (line-obj text)
  (method (next)
    (let ((result (car text)))
      (set! text (cdr text))
      result))
  (method (empty?) (null? text))
  (method (put-back token) (set! text (cons token text))) )

(define (make-line-obj text)
  (instantiate line-obj text))



;;; Problem A2   logo-type

(define (logo-type val)
  (define (print-one val)
    (cond ((list? val)
	   (display "[")
	   (if (pair? val) (logo-type val))
	   (display "]"))
	  (else (display val))))
  (define (iter val)
    (print-one (car val))
    (cond ((null? (cdr val)) '())
	  (else (display " ") (iter (cdr val)))))
  (cond ((null? val) '())
	((list? val) (iter val))
	(else (print-one val)))
  '=no-value=)

(define (logo-print val)
  (logo-type val)
  (newline)
  '=no-value=)

(define (logo-show val)
  (logo-print (list val)))



;;; Problem B3   variables   (logo-meta.scm is also affected)

(define (make env var val) (define-variable! var val env) '=no-value=)

;; I've modified define-variable! in logo-meta.scm so that it looks for
;; a binding anywhere in the environment, but if one isn't found, it
;; creates a new one in the-global-environment instead of in env.


;;; Here are the primitives RUN, IF, and IFELSE.  Problem B2 provides
;;; support for these, but you don't have to modify them.

(define (run env exp)
  (eval-line (make-line-obj exp) env))

(define (logo-if env t/f exp)
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp) env))
	((eq? t/f 'false) '=no-value=)
	(else (error "Input to IF not true or false " t/f))))

(define (ifelse env t/f exp1 exp2)
  (cond ((eq? t/f 'true) (eval-line (make-line-obj exp1) env))
	((eq? t/f 'false) (eval-line (make-line-obj exp2) env))
	(else (error "Input to IFELSE not true or false " t/f))))

(define (test env t/f)
  (make env " test" t/f))

(define (iftrue env exp)
  (let ((t/f (lookup-variable-value " test" env)))
    (cond ((eq? t/f 'true) (eval-line (make-line-obj exp) env))
	  ((eq? t/f 'false) '=no-value=)
	  (else (error "Input to TEST not true or false " t/f)))))

(define (iffalse env exp)
  (let ((t/f (lookup-variable-value " test" env)))
    (cond ((eq? t/f 'false) (eval-line (make-line-obj exp) env))
	  ((eq? t/f 'true) '=no-value=)
	  (else (error "Input to TEST not true or false " t/f)))))


;;; Problem B2   logo-pred

(define (logo-pred pred)
  (lambda args (if (apply pred args)
		   'true
		   'false)))


;;; Here is an example of a Scheme predicate that will be turned into
;;; a Logo predicate by logo-pred:

(define (equalp a b)
  (if (and (number? a) (number? b))
      (= a b)
      (equal? a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Stuff below here is needed for the interpreter to work but you  ;;;
;;;  don't have to modify anything or understand how they work.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The Logo reader

(define left-paren-symbol (string->symbol (make-string 1 #\( )))
(define right-paren-symbol (string->symbol (make-string 1 #\) )))
(define quote-symbol (string->symbol (make-string 1 #\" )))

(define (logo-read)  
  (define lookahead #f)   
  (define (logo-read-help depth)   
    (define (get-char)  
      (if lookahead  
          (let ((char lookahead))   
            (set! lookahead #f)   
            char) 
          (let ((char (read-char)))   
            (if (eq? char #\\)
                (list (read-char))  
                char)))) 
    (define (quoted char)   
      (if (pair? char)   
          char 
          (list char)))  
    (define (get-symbol char)   
      (define (iter sofar char)
        (cond ((pair? char) (iter (cons (car char) sofar) (get-char))) 
              ((memq char  
                     '(#\space #\newline #\+ #\- #\* #\/  
                               #\= #\< #\> #\( #\) #\[ #\] ))
               (set! lookahead char)   
               sofar) 
              (else (iter (cons char sofar) (get-char))) ))   
      (string->word (list->string (reverse (iter '() char)))) )
    (define (get-token space-flag)   
      (let ((char (get-char)))   
              (cond ((eq? char #\space) (get-token #t))  
              ((memq char '(#\+ #\* #\/ #\= #\< #\> #\( #\) ))   
               (string->symbol (make-string 1 char)))
              ((eq? char #\-)   
               (if space-flag  
                   (let ((char (get-char)))   
                     (let ((result (if (eq? char #\space)  
                                       '- 
                                       '=unary-minus=))) 
                       (set! lookahead char)   
                       result)) 
                   '-)) 
              ((eq? char #\[) (logo-read-help (+ depth 1)))  
              ((pair? char) (get-symbol char))
              ((eq? char #\")   
               (let ((char (get-char)))   
                 (if (memq char '(#\[ #\] #\newline))  
                     (begin (set! lookahead char) quote-symbol)
                     (string->symbol (word quote-symbol
					   (get-symbol (quoted char))))))) 
              (else (get-symbol char)) )))
    (define (after-space)
      (let ((char (get-char)))
	(if (eq? char #\space)
	    (after-space)
	    char)))
    (let ((char (get-char)))   
      (cond ((eq? char #\newline)
             (if (> depth 0) (set! lookahead char))   
             '()) 
	    ((eq? char #\space)
	     (let ((char (after-space)))
	       (if (eq? char #\newline)
		   (begin (if (> depth 0) (set! lookahead char))
			  '())
		   (begin (set! lookahead char)
			  (let ((token (get-token #t)))
			    (cons token (logo-read-help depth)))))))
            ((eq? char #\])   
             (if (> depth 0) '() (error "Unexpected ]")))
            ((eof-object? char) char)   
            (else (set! lookahead char)
                  (let ((token (get-token #f)))
                    (cons token (logo-read-help depth)) ))))) 
  (logo-read-help 0))  


;;; Assorted stuff   

(define (make-logo-arith op)   
  (lambda args (apply op (map maybe-num args))))   

(define (maybe-num val)
  (if (word? val)
      (string->word (word->string val))
      val))

(define tty-port (current-input-port))   

(define (prompt string)   
  (if (eq? (current-input-port) tty-port)
      (begin (display string)
	     (flush))))  

(define (meta-load fn)   
  (define (loader)  
    (let ((exp (logo-read)))   
      (if (eof-object? exp)   
          '() 
          (begin (eval-line (make-line-obj exp)
			    the-global-environment) 
		 (loader))))) 
  (with-input-from-file (symbol->string fn) loader)
  '=no-value=) 
