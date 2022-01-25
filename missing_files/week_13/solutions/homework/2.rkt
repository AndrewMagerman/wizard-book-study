#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

(define sender car)
(define receiver cadr)
(define subject caddr)
(define message cadddr)

; 2a

(define counted-subject-lines (mapreduce (lambda (email)
                                           (list (make-kv-pair (subject email) 1)))
                                         +
                                         0
                                         email1))

counted-subject-lines

; 2b

(define sorted-subject-lines (mapreduce (lambda (input-kv)
                                          (list (make-kv-pair (kv-value input-kv)
                                                              (kv-key input-kv))))
                                        cons
                                        '()
                                        counted-subject-lines))
sorted-subject-lines

; 2c

(define sender-subject-lines (mapreduce (lambda (email)
                                          (list (make-kv-pair (sender email)
                                                              (subject email))))
                                        cons
                                        '()
                                        email1))

(define sender-counted-subject-lines (mapreduce (lambda (input-kv)
                                                  (list (make-kv-pair (kv-key input-kv)
                                                                      (mapreduce (lambda (subject)
                                                                                   (list (make-kv-pair subject 1)))
                                                                                 +
                                                                                 0
                                                                                 (kv-value input-kv)))))
                                                cons
                                                '()
                                                sender-subject-lines))

sender-counted-subject-lines
