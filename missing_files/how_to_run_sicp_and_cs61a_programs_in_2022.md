# How to run SICP and CS61a programs in 2022

During the course of the 'Coders Only' Study Group we ran into some difficulties
running the different Scheme programs in the SICP book and in the CS61a course
of 2010 delivery by Prof. Brian Harvey.

The scheme interpreter [UCB Scheme](https://people.eecs.berkeley.edu/~bh/61a-pages/Scheme/), used in the lectures,
is a modified version of [STklos](https://www.stklos.net/).

Unfortunately UCB Scheme is no longer maintained, so we have had to sometimes improvise


## DrRacket

DrRacket is the most up-to-date IDE for running Scheme and Lisp. 
A comfortable majority of exercises can be run using DrRacket.

### Install Racket

https://racket-lang.org/download/

### Install the simply-scheme package
`#lang racket`

type `(require (planet dyoo/simply-scheme:2:2))` in the interpreter

from:
http://planet.racket-lang.org/display.ss?package=simply-scheme.plt&owner=dyoo

Use 'Simply Scheme' as the selected language. Simply Scheme is a variant of Scheme written
by Prof. Brian Harvey and it includes all the word/sentence primitives used in the
CS61a course.


## stklos

## Introduction to STklos with Docker

### Pull the latest image from docker and start it

``` bash
$ docker pull stklos/stklos:1.60          # grab the 1.60 version of STklos
$ docker run -ti stklos/stklos:1.60       # and run it

...
stklos> (version)
"1.60"
stklos> (exit)
```

docs: https://stklos.net/

### Run STklos within a particular directory

Navigate to the directory with the code you want to run

```
cd wizard-book-study/missing_files/week10
docker run -v$(pwd):/home -ti stklos/stklos:1.60 stklos
```


# Specific changes

## CS61a project 3
The recent versions of Scheme are now case-sensitive, we have rewritten the project files in
the /project_3 subdirectory with correct casing


## CS61a Week 10 - instant messaging clients and server

please refer to the subdirectory /week10

## CS61a Week 11

Here is how to implement this using Racket:

https://docs.racket-lang.org/reference/streams.html
https://docs.racket-lang.org/reference/Delayed_Evaluation.html

`(require racket/stream)`

Labs 1:
`(require racket/promise)`

Labs 2:
type
`(require pfds/stream)` in the DrRacket Interpreter to install, then use at the top of the program

You will need to replace cons-stream with stream-cons

Labs 3 & 4:
`(require racket/stream)`
`(require racket/promise)`

The solutions are, bizzarely, under Week13


Function to show the first n elements in a stream using racket/stream.
Inspired by Brian Allison
https://wizardbook.wordpress.com/2010/12/20/exercise-3-56/
```
(define (show-stream s n)
  (if (zero? n)
      (displayln "done")
      (begin
        (show (stream-first s))
        (show-stream (stream-rest s) (- n 1) ))))
```

# Exercises 

## 2.6

~cs61a/lib/church-hint

can be found at https://inst.eecs.berkeley.edu/~cs61as/library/

This Wikipedia article was useful:
https://en.wikipedia.org/wiki/Church_encoding#Computation_with_Church_numerals

## 2.81

TODO - coercion table code?

## 3.39 et. al.
These require sicp-concurrency


```
(require (planet dyoo/sicp-concurrency:1:2/sicp-concurrency))
```

## Chapter 4

Following the recommendation of Ervin Varga 
https://evarga.gitbooks.io/solutions-guide-for-the-sicp-book/content/chapter4.html


To run the evaluator under DrRacket you must perform the following changes to the original sources:

- Add the `#lang planet neil/sicp` directive at the beginning of each file.
- Rename apply to apply-custom (or something else), as needed.



# Miscellaneous help

Easy way to draw box-and-pointer diagrams:

https://github.com/jackrosenthal/sdraw-racket/tree/HEAD
