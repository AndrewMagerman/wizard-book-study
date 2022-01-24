# wizard-book-study

Let's study the Wizard book together.

![Wizard book picture](resources/Wizard-book-meetup.png)

The Wizard book (Structure and Interpretation of Computer Programs, a.k.a SICP)
is the book that is used for teaching CS at Berkeley and Stanford Universities. It's a challenging read, introducing
many fundamental concepts, and not for the faint of heart. It's one of these rare books where the reviews on Amazon are
either 5 or one star. Love it or hate it.

Here is an excerpt of the preface which I found particularly arresting:

> First, we want to establish the idea that a computer language is not just a  
> way of getting a computer to perform operations but rather that it is a novel
> formal medium for expressing ideas about methodology.
> Thus, **programs must be written for people to read**, and only incidentally
> for machines to execute.
> Second, we believe that the essential material to be addressed by a subject
> at this level is not
> the syntax of particular programming-language constructs, nor clever
> algorithms for computing particular
> functions efficiently, nor even the mathematical analysis of algorithms and
> the foundations of computing,
> but rather **the techniques used to control the intellectual complexity
> of large software systems**.


I tried to self-study about 5 years ago, but my brain froze around the middle, so this is why I'm organizing a group
study, in the hope that peer pressure, and our combined brainpower will help us stay on course.

The excellent website https://teachyourselfcs.com/ suggests using
[Brian Harvey's SICP lectures](https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E?sort=titleSorter)
and that is what we will follow there. I tried the MIT lectures but found them difficult.

The idea is: we study individually, but on an agreed schedule (i.e. watch Lectures 1,2,3 and do homework XXX), and have
a regular online meetup where we exchange our issues and help each other. Provisionally the schedule is every two weeks,
but we'll see what pace is comfortable/sustainable.

We will do the exams! (not the official ones)


# Reference

(Copied from [Federico Gelassi's github repository](https://github.com/fgalassi/cs61a-sp11))

# Books

## **Structure and Interpretation of Computer Programs, second edition**

Abelson, Harold | Sussman, Gerald Jay | Sussman, Julie

### free online version:

http://mitpress.mit.edu/sicp/

### Buy the hardcopy (recommended)

https://www.buchhaus.ch/de/detail/ISBN-9780262510875/Abelson-Harold/Structure-and-Interpretation-of-Computer-Programs-second-edition?bpmctrl=bpmrownr.1%7Cforeign.490271-1-0-0

## The Little Schemer

- Recommended book to learn lisp/scheme: 

https://www.buchhaus.ch/de/detail/ISBN-9780262560993/Friedman-Daniel-P./The-Little-Schemer-fourth-edition?bpmctrl=bpmrownr.1%7Cforeign.490271-1-0-0

## Simply Scheme

- Recommended, written by the professor doing our lectures

https://www.buchhaus.ch/de/buecher/fachbuecher/informatik/programmieren/detail/ISBN-9780262082815/Harvey-Brian-University-of-California-Author/Simply-Scheme


# The Lectures

## Recordings of the lectures by Brian Harvey

https://archive.org/details/ucberkeley-webcast-PL3E89002AA9B9879E

more convenient: in youtube

https://www.youtube.com/watch?v=4leZ1Ca4f0g&list=PLhMnuBfGeCDNgVzLPxF9o5UNKG1b-LFY9

## Homework

https://inst.eecs.berkeley.edu//~cs61a/reader/nodate-hw.pdf

### Homework Solutions from Brian Harvey
 
https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/

Other good resource:
https://evarga.gitbooks.io/solutions-guide-for-the-sicp-book/content/

### cs61 library
https://inst.eecs.berkeley.edu/~cs61as/library/

## Projects

https://inst.eecs.berkeley.edu//~cs61a/reader/vol1.html

## Exams

- midterms: https://inst.eecs.berkeley.edu//~cs61a/reader/vol2.html
- final: https://inst.eecs.berkeley.edu//~cs61a/reader/vol2.html

## Homepage CS61A

https://inst.eecs.berkeley.edu//~cs61a/sp11/

# Software

We are using Racket, a dialect of Scheme. Most of the exercises can be
done with the comfortable DrRacket IDE.

Some require https://stklos.net/



## Install Racket

https://racket-lang.org/download/

### Install the simply-scheme package
`#lang racket`
type `(require (planet dyoo/simply-scheme:2:2))` in the interpreter

from:
http://planet.racket-lang.org/display.ss?package=simply-scheme.plt&owner=dyoo

Use 'Simply Scheme' as the selected language

### Common Racket Errors
https://docs.google.com/document/d/1jGtldEcm_qPoHGknJOkWj1D4-doyBjDivaV_Vn7_Hxk/edit#

### Syntax highlighting

JetBrains Products have a Racket syntax highlighter!!!
- Search for `Racket` in the Plugin Marketplace
- Preferences/Editor/File Types : add *.scm under the Racket file 

# Forums and chats

## Discord Chat
https://discord.gg/sPTMqH5zYN

## subreddit
https://www.reddit.com/r/sicp/

## StackOverflow
https://stackoverflow.com/questions/tagged/sicp


# Reviews
https://onestepcode.com/sicp-review/

https://www.inchmeal.io/sicp/review.html


# Schedule:

## Week 1 (Functional Programming)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 1
- read the book Section 1.1, pages 1-31
- watch the lectures 1 & 2
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 1
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 1
- cross-check your homework (solutions/week1.txt)


We **review** this work on Review Meeting Week 1 on Wednesday 24th March, 18.30 CET

## Week 2 (Higher-order procedures)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 2
- read the book Section 1.3 - 1.2 skipped for now
- watch the lectures 3 & 4 & 5 & 6
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 2
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 2 including book exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46
- cross-check your homework (solutions/week2.txt)


We **review** this work on Review Meeting Week 2 on Wednesday 7th April, 18.30 CET

## Week 3 (Recursion and iteration)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 3
- read the book Section 1.2 through 1.2.4 (pages 31–47)
- watch the lectures 7 & 8
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 3
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 3 including book exercises 1.16, 1.35, 1.37, 1.38
- cross-check your homework (solutions/week3.txt)
- do Project 1 - reference/berkeley_cs61a_material/course_reader_vol_1/project_1


We **review** this work on Review Meeting Week 3 on Wednesday 28th April, 18.30 CET

## Week 4 (Data abstraction)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 4
- read the book Sections 2.1 and 2.2.1 (pages 79–106)
- watch the lectures 9 & 10 & 11
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 4
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 4 including book exercises 2.7, 2.8, 2.10, 2.12, 2.17, 2.20, 2.22, 2.23
- cross-check your homework (solutions/week4.txt)


We **review** this work on Review Meeting Week 4 on Wednesday 19th May, 18.30 CET

## Week 5 (Hierarchical data)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 5
- read the book Section 2.2.2–2.2.3, 2.3.1, 2.3.3
- watch the lectures 12 & 13 & 14
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 5
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 5 including book exercises 2.24, 2.26, 2.29, 2.30, 2.31, 2.32, 2.36, 2.37, 2.38, 2.54
- cross-check your homework (solutions/week5.txt)
- do First Midterm


We **review** this work on Review Meeting Week 5 on Wednesday 9th June, 19.00 CET

## Week 6 (Generic Operators)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 6
- read the book Sections 2.4 through 2.5.2 (pages 169–200)
- watch the lectures 16 & 17
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 6
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 6 including book exercises 2.74, 2.75, 2.76, 2.77, 2.79, 2.80, 2.81, 2.83
- cross-check your homework (solutions/week6.txt)
- do Project 2 - section 2.2.4 of the book - all exercises


We **review** this work on Review Meeting Week 6 on Wednesday 30th June, 19.00 CET

## Week 7 (Object-oriented programming)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 7
- read [Object-Oriented Programming—Above-the-line view](reference/berkeley_cs61a_material/course_reader_vol_2/oop/aboveline.pdf)
- watch the lectures 18 & 19 & 20
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 7
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 7
- cross-check your homework (solutions/week7.txt)


We **review** this work on Review Meeting Week 7 on Wednesday 21st July, 19.00 CET

## Week 8 (Assignment, state, environments)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 8
- read the book Section 3.1, 3.2
- read [Object-Oriented Programming—Below-the-line view](reference/berkeley_cs61a_material/course_reader_vol_2/oop/belowline.pdf)
- watch the lectures 21 & 22 & 23
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 8
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 8 including book exercises 3.3, 3.4, 3.7, 3.8, 3.10, 3.11
- cross-check your homework (solutions/week8.txt)
- do Second Midterm


We **review** this work on Review Meeting Week 8 on Wednesday 25th August, 19.00 CET

## Week 9 (Mutable data, vectors)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 9
- read the book Section 3.3.1–3
- watch the lectures 24 & 25 & 26
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 9
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 9 including book exercises 3.16, 3.17, 3.21, 3.25, 3.27
- cross-check your homework (solutions/week9.txt)
- do Project 3a (with a partner)


We **review** this work on Review Meeting Week 9 on Wednesday 15th September, 19.00 CET

## Week 10 (client/server, concurrency)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 10
- read the book Section 3.4
- watch the lectures 29 & 30 & 31 & 32
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 10
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 10 including book exercises 3.38, 3.39, 3.40, 3.41, 3.42, 3.44, 3.46, 3.48, 3.33 to 3.37
- cross-check your homework (solutions/week10.txt)
- do Project 3b (with a partner)


We **review** this work on Review Meeting Week 10 on Wednesday 6th October, 19.00 CET

## Week 11 (Streams)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 11
- read the book Section 3.5.1–3, 3.5.5
- watch the lectures 27 & 28 & 33 & 34
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 11
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 11 including book exercises 3.50, 3.51, 3.52, 3.53, 3.54, 3.55, 3.56, 3.64, 3.66, 3.68
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week13.txt)


We **review** this work on Review Meeting Week 11 on Wednesday 27th October, 19.00 CET

## Week 12 (Metacircular evaluator)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 12
- read the book Section 4.1.1–6
- read [Therac paper](reference/berkeley_cs61a_material/course_reader_vol_2/Therac-25.pdf)
- watch the lectures 35 & 36 & 37
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 12
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 12 including book exercises 4.3, 4.6, 4.7, 4.10
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week12.txt)
- do Third Midterm


We **review** this work on Review Meeting Week 12 on Monday 29th November, 19.00 CET

## Week 12b (Metacircular evaluator part 2)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 12b
- read the book Section 4.1.1–6 and MapReduce paper in course reader.
- read [MapReduce paper](reference/berkeley_cs61a_material/course_reader_vol_2/mapreduce-osdi04.pdf)
- watch the lectures 36 & 37
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 12b
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 12b including book exercises 4.11, 4.13, 4.14, 4.15
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week12.txt)


We **review** this work on Review Meeting Week 12b on Monday 13th December, 19.00 CET

## Week 12c (Project 4a - A1 A2 B1 B2)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 12c
- read the book logo.txt in reference/berkeley_cs61a_material/course_reader_vol_1/project_4
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 12c
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 12c
- cross-check your homework (no solutions)


We **review** this work on Review Meeting Week 12c on Monday 10th January 2022, 19.00 CET

## Week 13a (Analyzing evaluator, MapReduce)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 13a
- read [Therac paper](reference/berkeley_cs61a_material/course_reader_vol_2/Therac-25.pdf)
- watch the lectures 38 & 39
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 13a
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 13a including book exercises 4.22, 4.23, 4.24
- cross-check your homework (no solutions for Mapreduce, exercises reference/berkeley_cs61a_material/solutions/week12.txt)


We **review** this work on Review Meeting Week 13a on Monday 24th January 2022, 19.00

## Week 13b (Project 4 - 3 and 4)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 13b
- read the book logo.txt in reference/berkeley_cs61a_material/course_reader_vol_1/project_4
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 13b
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 13b
- cross-check your homework (no solutions unfortunately)
- do Project 4 (with a partner)


We **review** this work on Review Meeting Week 13b on Monday 7th February 2022, 19.00 CET

## Week 14a (lazy evaluator, nondeterministic evaluator)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 14a
- read the book Section 4.2, 4.3
- watch the lectures 40 & 41
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 14a
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 14a including book exercises 4.25, 4.26, 4.28, 4.42, 4.45, 4.49, 4.50, 4.52
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week14.txt)


We **review** this work on Review Meeting Week 14a on Monday 7th March 2022, 19.00 CET

## Week 14b (Project 4 - 5 and 6 and 7 )

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 13
- read the book logo.txt in reference/berkeley_cs61a_material/course_reader_vol_1/project_4
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 13
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 13
- cross-check your homework (solutions/week13.txt)
- do Project 4 (with a partner)


We **review** this work on Review Meeting Week 14b on TBD

## Week 14c (lazy evaluator, nondeterministic evaluator)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 14c
- read the book Section 4.2, 4.3
- watch the lectures 40 & 41
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 14c
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 14c including book exercises 4.30, 4.32, 4.33, 4.36, 4.47, 4.48
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week14.txt)
- do Project 4 (with a partner)
- do Final Exam


We **review** this work on Review Meeting Week 14c on TBD

## Week 15 (Logic programming)

- do [labs](reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf) for week 15
- read the book Section 4.4.1–3
- watch the lectures 42 & 43 & 44
- read the [course notes](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf) for week 15
- do [homework](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf) for week 15 including book exercises 4.56, 4.57, 4.58, 4.65
- cross-check your homework (reference/berkeley_cs61a_material/solutions/week15.txt)


We **review** this work on Review Meeting Week 15 on TBD

