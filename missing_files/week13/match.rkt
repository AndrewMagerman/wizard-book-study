#lang racket

(provide match?)

(require threading)

; Thank you Eric Leschinski :)
; https://stackoverflow.com/questions/18402416/regular-expression-to-match-a-word-or-its-prefix
(define (make-pregexp pattern)
  (define (asterisk? str) (string=? "*" str))
  (define (match-whole-words expression)
    (map (lambda (n) (if (not (asterisk? n))
                         (string-append "\\b" n "\\b")
                         n))
         expression))
  (define (match-whole-sentence expression)
    (append (cons "^" expression)
            (list "$")))
  (define (expression-list->pregexp expression)
    (~>> expression
      (map (lambda (n) (if (asterisk? n)
                                 ".*"
                                 n)))
      (string-join _ "\\s*")
      pregexp))
  (~> pattern
      match-whole-words
      match-whole-sentence
      expression-list->pregexp))

(define (symbol-list->string-list lst)
  (map symbol->string lst))

(define (match? pattern sentence)
  (let ([needle (make-pregexp (symbol-list->string-list pattern))]
        [haystack (string-join (symbol-list->string-list sentence))])
    (regexp-match? needle haystack)))
