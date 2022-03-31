#lang racket

; Some compatibility and consistency to use mutable lists
;; with the familiar procedures
(require racket/mpair)

(provide (all-defined-out)
         (all-from-out racket/mpair))
         
;; 2s         
(define mcaar (compose mcar mcar))
(define mcadr (compose mcar mcdr))
(define mcdar (compose mcdr mcar))
(define mcddr (compose mcdr mcdr))

;; 3s         
(define mcaaar (compose mcar mcar mcar))
(define mcaadr (compose mcar mcar mcdr))
(define mcadar (compose mcar mcdr mcar))
(define mcaddr (compose mcar mcdr mcdr))
(define mcdaar (compose mcdr mcar mcar))
(define mcdadr (compose mcdr mcar mcdr))
(define mcddar (compose mcdr mcdr mcar))
(define mcdddr (compose mcdr mcdr mcdr))

; 4s
(define mcaaaar (compose mcar mcar mcar mcar))
(define mcaaadr (compose mcar mcar mcar mcdr))
(define mcaadar (compose mcar mcar mcdr mcar))
(define mcaaddr (compose mcar mcar mcdr mcdr))
(define mcadaar (compose mcar mcdr mcar mcar))
(define mcadadr (compose mcar mcdr mcar mcdr))
(define mcaddar (compose mcar mcdr mcdr mcar))
(define mcadddr (compose mcar mcdr mcdr mcdr))
(define mcdaaar (compose mcdr mcar mcar mcar))
(define mcdaadr (compose mcdr mcar mcar mcdr))
(define mcdadar (compose mcdr mcar mcdr mcar))
(define mcdaddr (compose mcdr mcar mcdr mcdr))
(define mcddaar (compose mcdr mcdr mcar mcar))
(define mcddadr (compose mcdr mcdr mcar mcdr))
(define mcdddar (compose mcdr mcdr mcdr mcar))
(define mcddddr (compose mcdr mcdr mcdr mcdr))

(define (mapply proc l)
  (list->mlist (apply proc (mlist->list l))))

(define (exp->mlist xs)
  (cond ((null? xs) null)
        ((or (pair? xs)
             (list? xs))
         (mcons (exp->mlist (car xs))
                (exp->mlist (cdr xs))))
        (else xs)))

(define (mlist->exp mxs)
  (cond ((null? mxs) null)
        ((or (mpair? mxs)
             (mlist? mxs))
         (cons (mlist->exp (mcar mxs))
               (mlist->exp (mcdr mxs))))
        (else mxs)))
