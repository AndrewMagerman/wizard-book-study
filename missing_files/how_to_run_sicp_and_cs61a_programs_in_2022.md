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


# Miscellaneous help

Easy way to draw box-and-pointer diagrams:

https://github.com/jackrosenthal/sdraw-racket/tree/HEAD
