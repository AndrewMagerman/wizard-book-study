To run the LOGO interpreter with STKLOS in a Docker container:

docker run -v$(pwd):/home -ti stklos/stklos:1.60 stklos

Then load a number of files:

(load "simply-scheme.scm")
(load "obj-harun.rkt")
(load "logo.rkt")
(load "disable-turtle-graphics.scm")
(load "logo-meta.rkt")

Simply-scheme has the Brian Harvey extensions to Scheme without which procedures like word, sentance or butfirst won't exist.
obj-harun is the Fixed Class defining library that Harun fixed for STKLOS
logo.rkt and logo-meta.rkt are the files that build the LOGO interpreter which you have to enhance for the exercise

Start the interpreter with

(initialize-logo)

Then try these LOGO commands

? print 3
3
? print sum product 3 4 8
20
? print [this is [a nested] list]
this is [a nested] list
? print 3 print 4
3
4
? print equalp 4 6
false
