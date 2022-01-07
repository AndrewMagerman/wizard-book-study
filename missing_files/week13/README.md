# Week 13 - MapReduce

This README will explain how to do the lab and homework of week 13.
The MapReduce assignments can be solved with racket.
To do so require mapreduce.rkt, which is from the cs61as_library.

```lisp
#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))

(define file1 '((MapReduce: Simplified Data Processing on Large Clusters)
                (Jeffrey Dean and Sanjay Ghemawat)
                (jeff@google.com, sanjay@google.com)
                (Google, Inc.)
                (Abstract)
                (MapReduce is a programming model and an associated implementation for processing and generating large data sets.)
                (Users specify a map function that processes a key/value pair to generate a set of intermediate key/value pairs, and a reduce function that merges all intermediate values associated with the same intermediate key.)
                (Many real world tasks are expressible in this model, as shown in the paper.)))

(mapreduce (lambda (input-key-value-pair)
             (list (make-kv-pair 'line 1)))
           +
           0
           file1)

; '((line . 8))
```

## Labs

For the third assignment the function match? is needed.
The implementation is in missing_files/week13.

```lisp
#lang racket

(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/match.rkt"))

(match? '(* is * sentence *) '(This is a short sentence))   ; -> #t
(match? '(* is sentence *) '(This is a short sentence))     ; -> #f
(match? '(* short sentence *) '(This is a short sentence))  ; -> #t
(match? '(* sentence) '(This is a short sentence))          ; -> #t
(match? '(* ink *) '(thinking about it))                    ; -> #f
(match? '(* ink *) '(no ink left))                          ; -> #t
```

## Homework

## Solutions

Unfortunately there are no solutions.
