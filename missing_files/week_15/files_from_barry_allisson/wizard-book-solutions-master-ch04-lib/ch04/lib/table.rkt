#lang racket
(require "mutable.rkt")

(provide make-table
         get-syntax
         put-syntax!
         attach-tag
         type-tag
         contents)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define syntax-table (make-table))
(define (get-syntax proc-id) ((syntax-table 'lookup-proc) 'meta proc-id))
(define (put-syntax! proc-id proc) ((syntax-table 'insert-proc!) 'meta proc-id proc))

(define (attach-tag type-tag contents)
  (mcons type-tag contents))

(define (type-tag datum)
  (if (mpair? datum)
      (mcar datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (mpair? datum)
      (mcdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))
 

