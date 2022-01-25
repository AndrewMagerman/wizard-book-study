#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/match.rkt"))
(require (file "~/projects/sicp/wizard-book-study/missing_files/week13/data.rkt"))

; 1a

(define (add-once element listing)
  (if (member element listing)
      listing
      (cons element listing)))

(define (make-inverted-index-stream files)
  (mapreduce (lambda (input-kv)
               (mapreduce (lambda (word)
                            (list (make-kv-pair word (kv-key input-kv))))
                          add-once
                          '()
                          (kv-value input-kv)))
             add-once
             '()
             files))

(make-inverted-index-stream data)

; 1b

(define (make-inverted-index-stream-n data n)
  (filter (lambda (kv)
            (let ([key-len (string-length (symbol->string (kv-key kv)))])
              (or (= key-len n)
                  (> key-len n))))
          (make-inverted-index-stream data)))
