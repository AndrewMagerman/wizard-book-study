#lang racket

;; Use the swindle version of amb
(require (only-in swindle amb amb-assert amb-collect))

(provide (all-defined-out)
         amb amb-assert amb-collect)

(define (square n) (* n n ))

(define (prime? n)
  (define (smallest-divisor n)
    (define (find-divisor n test-divisor)
      (define (divides? a b) (= (remainder b a) 0))
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= n (smallest-divisor n)))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (amb-fail) (amb))
(define (try-again) (amb))
(define require amb-assert)

(define (an-element-of items)
  (amb-assert (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

