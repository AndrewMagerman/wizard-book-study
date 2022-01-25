#lang racket

(require (file "~/projects/sicp/wizard-book-study/reference/cs61as_library/mapreduce-racket/mapreduce.rkt"))

(provide filename
         lines
         file->linelist
         mapreduce-paper
         data
         email1)

; Functions taken from wizard-book-study/reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf

(define filename car)
(define lines cdr)

(define (file->linelist file)
  (map (lambda (line)
         (make-kv-pair (filename file)
                       line))
       (lines file)))

(define file1 '((mapreduce-paper)
                (MapReduce: Simplified Data Processing on Large Clusters)
                (Jeffrey Dean and Sanjay Ghemawat)
                (jeff@google.com sanjay@google.com)
                (Google Inc)
                (Abstract)
                (MapReduce is a programming model and an associated implementation for processing and generating large data sets)
                (Users specify a map function that processes a key/value pair to generate a set of intermediate key/value pairs and a reduce function that merges all intermediate values associated with the same intermediate key)
                (Many real world tasks are expressible in this model as shown in the paper)))

(define file2 '((therac-25-paper)
                (An Investigation of the Therac-25 Accidents)
                (Computers are increasingly being introduced into safety-critical systems and as a consequence have been involved in accidents)
                (Some of the most widely cited software-related accidents in safety-critical systems involved a computerized radiation therapy machine called the Therac-25)
                (They have been described as the worst series of radiation accidents in the 35year history of medical accelerators)))

(define email1 '(("cs61a-tb" "cs61a-tc" "mapreduce" "mapreduce is great! lucky students!")
                 ("bot1337" "cs61a-ta" "free ipod now!" "buy herbal ipod enhancer!")
                 ("bot1338" "cs61c-tf" "free ipod now!" "buy herbal ipod enhancer!")
                 ("bot1338" "cs61c-xz" "free ipod now!" "buy herbal ipod enhancer!")
                 ("bot1338" "cs61c-xz" "free water now!" "buy water now!")
                 ("bot1338" "cs61c-xz" "free water now!" "buy water now!")
                 ("cs61a-ab" "cs61a-kp" "scheme" "An elegant language.")
                 ("cs61a-so" "cs61a-os" "keyboard" "Which keyboard would you recommend?")
                 ("cs61a-va" "cs61a-qu" "water" "drink water it's healthy!")
                 ("cs61a-qu" "cs61-va" "water" "I am drinking two liters of water every day!")))

(define mapreduce-paper (file->linelist file1))
(define data (append (file->linelist file2)
                     mapreduce-paper))
