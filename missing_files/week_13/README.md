# Week 13 - MapReduce

This README will explain how to do the lab and homework of week 13.
The MapReduce assignments can be solved with racket.
To do so require mapreduce.rkt, which is from the cs61as_library, and data.rkt.
Data.rkt will provide files and file handling functions.

```racket
#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

(mapreduce (lambda (sentence)
             (list (make-kv-pair 'line 1)))
           +
           0
           data)

; -> '((line . 12))
```

## Labs

For the third assignment the function match? is needed.
The implementation is in "reference/berkeley_cs61a_material/lectures/1_3/mapreduce.scm".

## Homework

For the first assignment use data.
For the second assignment use email1 as data.

## Solutions

The solutions are in "missing_files/week13/solutions".
