# To run the LOGO interpreter with STKLOS in a Docker container:

```scheme
docker run -v$(pwd):/home -ti stklos/stklos:1.60 stklos
(load "simply-scheme.scm")
;(load "obj-harun.rkt")
(load "obj.rkt")
(load "logo.soln.scm")
(load "logo-meta.soln.scm")
(initialize-logo)
```

Then try these LOGO commands

```logo
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

? print (sum 4 5 6 7 8)
30
? print (sum 4 5 product 6 7 8)
59
? print (word "a "b "c)
abc

? print 2 * 5
10

? make "x 3


# Doesn't work
? print :x 

? to scope :x
> helper 5
> end
? to helper :y
> print (sentence :x :y)
> end
? scope 4
```
