#lang racket
(require racket/stream
         (only-in racket/mpair mlist->list))

(provide (all-defined-out))

;; use the same api as the book (to allow for copy/paste examples
(define (cons-stream A B) (stream-cons A B))
(define stream-car stream-first)
(define stream-cdr stream-rest)
(define the-empty-stream empty-stream)
(define stream-null? stream-empty?)
(define (mlist->stream l)
  (list->stream (mlist->list l)))
(define (list->stream l)
  (foldr cons-stream empty-stream l))
(define (display-stream s)
  (stream-for-each displayln s))
(define (show-stream s n)
  (cond ((or (zero? n)
             (stream-null? s)) (displayln "done"))
        (else (displayln (stream-first s))
              (show-stream (stream-rest s) (sub1 n)))))

(define (stream-enumerate-interval low high) (in-range low high))
(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons s1car
                               (merge (stream-rest s1)
                                      (stream-rest s2)))))))))


(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed (force delayed-s2)
                           (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))
