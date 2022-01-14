; Write a procedure that takes a pattern and a directory name as arguments,
; and returns a stream of lines (with their keys attached) that match the pattern.

#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

(define (match? pattern text)
  (cond ((null? pattern) (null? text))
	((null? text)
	 (and (equal? (car pattern) '*)
	      (match? (cdr pattern) text)))
	((equal? (car pattern) '*)
	 (or (match? (cdr pattern) text)
	     (match? pattern (cdr text))))
	((equal? (car pattern) (car text))
	 (match? (cdr pattern) (cdr text)))
	(else #f)))

; Call match? on every sentence on the map phase and use the result as a key.
; This means there are only two possible keys "#t" or "#f".

(define (find pattern file-name)
  (let ([findings (mapreduce (lambda (input-kv)
                               (list (make-kv-pair (match? pattern (kv-value input-kv)) input-kv)))
                             cons
                             '()
                             file-name)])
    ; Mapreduce is sorting the keys. This means "#t" comes before "#f".
    ; Because we wan't the values of the "#t" key, we can just select the first one.
    (stream-map identity (kv-value (first findings)))))

(define found (find '(* in *)  data))

(stream-first (stream-rest found))
