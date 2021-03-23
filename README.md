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

# Schedule

## Meeting 1, Functional Programming:

### Date Wednesday 24th March, 18.30 CET

### Homework:

- install Dr. Racket incl. bekerley package
- log on to our Discord chat 
- read the book pages 1-31
- watch lectures 1 & 2 & 3 & 4
- read the course notes for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)  
- do homework for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)
  - you will need to read [word.txt](reference/berkeley_cs61a_material/course_reader_vol_2/word.txt)

Join the videochat: https://codersonly.whereby.com/hello


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

### Homework Solutions
 
https://people.eecs.berkeley.edu/~bh/61a-pages/Solutions/


## Projects

https://inst.eecs.berkeley.edu//~cs61a/reader/vol1.html

## Exams

- midterms: https://inst.eecs.berkeley.edu//~cs61a/reader/vol2.html
- final: https://inst.eecs.berkeley.edu//~cs61a/reader/vol2.html

## Homepage CS61A

https://inst.eecs.berkeley.edu//~cs61a/sp11/

# Software

We are using Racket, a dialect of Scheme.

## Install Racket

https://racket-lang.org/download/

### Install the simply-scheme package
type `(require (planet dyoo/simply-scheme:2:2))` in the interpreter

from:
http://planet.racket-lang.org/display.ss?package=simply-scheme.plt&owner=dyoo

Use 'Simply Scheme' as the selected language

### Common Racket Errors
https://docs.google.com/document/d/1jGtldEcm_qPoHGknJOkWj1D4-doyBjDivaV_Vn7_Hxk/edit#

JetBrains Products have a Racket syntax highlighter!!!

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


# Pipeline

## Meeting 1, Functional Programming:

### Date 24th March 2020, 18.30 CET

meetup:
https://www.meetup.com/de-DE/coders-only/events/276753430/

### Homework:

- install Dr. Racket incl. bekerley package
- log on to our Discord chat 
- read the book pages 1-31
- watch lectures 1 & 2
- read the course notes for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_2/notes.pdf)  
- do homework for [Week 1](reference/berkeley_cs61a_material/course_reader_vol_1/hw.pdf)
  - you will need to read [word.txt](reference/berkeley_cs61a_material/course_reader_vol_2/word.txt)
- do labs for week 1 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

## Meeting 2, Higher-order procedures:

Reading: Abelson & Sussman, Section 1.3

- do lab for week 2 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
- watch lectures 3 & 4 & 5 & 6
- do book exercises 1.31(a), 1.32(a), 1.33, 1.40, 1.41, 1.43, 1.46

### Date TBD

## Meeting 3, Recursion and iteration:

Reading: Abelson & Sussman, Section 1.2 through 1.2.4 (pages 31–47)

- do lab for week 3 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
- watch lectures 7 & 8

## Meeting 4, Data abstraction:

Reading: Abelson & Sussman, Sections 2.1 and 2.2.1 (pages 79–106)

- do lab for week 4 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
- watch lectures 9 & 10
- do project 1

## Meeting 5, Hierarchical data:

First Midterm

- do lab for week 5 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
Reading: Abelson & Sussman, Section 2.2.2–2.2.3, 2.3.1, 2.3.3

- watch lectures 12 & 13 & 14

## Meeting 6, Generic Operators:

- do lab for week 6 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
Reading: Abelson & Sussman, Sections 2.4 through 2.5.2 (pages 169–200)

- watch lectures 16 & 17

- do project 2

## Meeting 7, Object-oriented programming:

- do lab for week 7 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
Reading:
Read “Object-Oriented Programming—Above-the-line view”
([in course reader](reference/berkeley_cs61a_material/course_reader_vol_2/oop/aboveline.pdf)).

- watch lectures 18 & 19 & 20

## Meeting 8, Assignment, state, environments:


- do lab for week 8 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)
Reading: Abelson & Sussman, Section 3.1, 3.2

Also read “Object-Oriented Programming—Below-the-line view”
([in course reader](reference/berkeley_cs61a_material/course_reader_vol_2/oop/belowline.pdf)).

- watch lectures 21 & 22 & 23

## Meeting 9, Mutable data, vectors:

- do lab for week 9 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)


Reading: Abelson & Sussman, Section 3.3.1–3

- watch lectures 24 & 26

- do project 3a

## Meeting 10, client/server, concurrency:

- do lab for week 10 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

Reading: Abelson & Sussman, Section 3.4

- watch lectures 30 & 31 & 32

- do project 3b

## Meeting 11, Streams:

- do lab for week 11 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

Reading: Abelson & Sussman, Section 3.5.1–3, 3.5.5

- watch lectures 33 & 34 & 35

## Meeting 12, Metacircular evaluator:

Reading: Abelson & Sussman, 4.1.1–6

MapReduce paper in course reader.


- do lab for week 12 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

- watch lectures 36 & 37

## Meeting 13, Analyzing evaluator, MapReduce:

- do lab for week 13 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

Reading: Therac paper in course reader.

- watch lectures 38 & 39


## Meeting 14, lazy evaluator, nondeterministic evaluator:

- do lab for week 14 (reference/berkeley_cs61a_material/course_reader_vol_1/labs.pdf)

Reading: Abelson & Sussman, Section 4.2, 4.3

- watch lectures 42 & 43 & 44

- do project 4

# Archive

## Meeting 0, Kickstart:

### Wednesday 3rd March, 18.00, 45 minutes

Questions and Answers, Organisation, first steps

Meetup: https://www.meetup.com/de-DE/coders-only/events/276169051/

Join the videochat: https://codersonly.whereby.com/hello
