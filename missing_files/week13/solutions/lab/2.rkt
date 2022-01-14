#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/match.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

; 2a
; The inner mapreduce or second mapreduce is needed to count every word by making a key value pair of the words in a sentence.
; Multiple occurences of a word in a sentence are reduced.
; Example:
; '(This is a sentence in a document) becomes '((This . 1) (is . 1) (a . 1) (sentence . 1) (in . 1) (a . 1) (document . 1)) after the map phase,
; and '((This . 1) (is . 1) (a . 2) (sentence . 1) (in . 1) (document . 1)) after the reduce phase.
;
; The outer or first mapreduce will count every word in a file.

(define words (mapreduce (lambda (input-kv-pair)
                           (mapreduce (lambda (word)
                                        (list (make-kv-pair word 1)))
                                      +
                                      0
                                      (kv-value input-kv-pair)))
                         +
                         0
                         data))

words

; 2b
(define minimum 2)
(for-each (lambda (kv-pair)
            (when (> (kv-value kv-pair) minimum)
              (displayln kv-pair)))
          words)

; 2c
(define unique-words (stream-filter (lambda (kv-pair)
                                      (= (kv-value kv-pair) 1))
                                    words))
