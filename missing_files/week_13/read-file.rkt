#lang racket

(require threading)
(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))

(provide read-file-as-key-value)

(define (file-name path)
  (~> path
      file-name-from-path
      path->string
      string->symbol))

(define (read-file-by-lines path)
  (with-input-from-file path
    (lambda ()
      (for/list ([l (in-lines)])
        (map string->symbol (string-split l))))))

(define (read-file-as-key-value path)
  (let ([name (file-name path)]
        [lines (filter (lambda (line) (not (empty? line)))
                       (read-file-by-lines path))])
    (map (lambda (line) (make-kv-pair name line))
         lines)))
