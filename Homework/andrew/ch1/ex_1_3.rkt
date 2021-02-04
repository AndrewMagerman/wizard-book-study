#lang racket
(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))


(define (sum-squares-largest-two x y z)
  (cond ((and (< x y) (< x z)) (sum-of-squares y z))
        ((and (< y x) (< y z)) (sum-of-squares x z))
        ((and (< z x) (< z y)) (sum-of-squares y z))
        )
  )

(sum-squares-largest-two 5 2 3)

