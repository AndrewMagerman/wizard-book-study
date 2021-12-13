To run the LOGO interpreter with STKLOS in a Docker container:

Change to the project_4 directory
cd wizard-book-study\reference\berkeley_cs61a_material\course_reader_vol_1\project_4

On Linux:
docker run -v$(pwd):/home -ti stklos/stklos:1.60 stklos

On Windows in PowerShell (VS Code):
docker run -v {PWD}:/home -ti stklos/stklos:1.60 stklos

On Windows in DOS:
docker run -v %cd%:/home -ti stklos/stklos:1.60 stklos

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


GOTCHA NOTE: (4 hours invested (wasted?))
in logo-meta.rkt procedures are added to the the-primitive-procedures table. If you change parts of the 
procedure like logo-pred in exercise B2 but don't restart the docker environment you simply add further
defintions for the same keys to the table. Since the original entry is still in the table the 
apply-primitive-procedure will fine the original one and will not bother to look further for your new procedure.
SO, TO BE SURE THAT YOUR LATEST CODE IS RUNNING, QUIT DOCKER CONTAINER AND START OVER!


To try Richard's solution:

docker run -v$(pwd):/home -ti stklos/stklos:1.60 stklos
(load "simply-scheme.scm")
(load "obj-harun.rkt")
(load "logo.rkt")
(load "disable-turtle-graphics.scm")
(load "richard-solution.scm") ;; must be loaded before logo-meta.rkt
(load "logo-meta.rkt")
(load "richard-solution.scm") ;; Yes a second time (logo-meta.rkt overwrites some functions)

(initialize-logo)

;; try the LOGO commands above
