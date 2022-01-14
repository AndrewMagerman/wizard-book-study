#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/match.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

; To count a sentence, take the first word, in a sentence, as a key.
; Reduce all sentences with the same key.

(define lines-in-files (mapreduce (lambda (input-kv-pair)
                                    (list (make-kv-pair (first (kv-value input-kv-pair)) 1)))
                                  +
                                  0
                                  data))

; Map the intermediate results by creating pairs with the same key "line".
; Reduce them in the second phase.

(mapreduce (lambda (input-kv-pair)
             (list (make-kv-pair 'line (kv-value input-kv-pair))))
           +
           0
           lines-in-files)
