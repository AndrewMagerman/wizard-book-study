# Minutes of Review Week 2

## General round-up

Nicolas asked all attendees what was their current status

Nicolas: behind on homework because of Easter, found three-week interval best.

Richard: Mathematical examples and exercises an unnecessary complexity. 
He misses while and for loops
recursion seems silly and unpractical

Giammi: The english used is advanced and difficult to read as a non-german speaker. 
Harun mentioned that the book exists in German: https://www.springer.com/de/book/9783642977275

Florence: Mentioned that she had good success at bundling reading+immediately doing
the relevant exercise

Harun: recommended to ignore the mathematical foundations of the excecises 
since these are incidental.

Nico: lacked time, a little behind, prefers working on weekends.

Andrew: Enjoyed the exercises, didn't really understand the counting change
program, nor the y combinator homework. Found that following the order (i.e. first labs, then reading, then lectures) helped.
Found that working on paper first was helpful, especially using the technique
of 'wishful thinking'

## Decision about pace:

We try keeping the two-week pace, in the assumption that the Easter weekend 
was the main cause of the lateness. We decide in the next meeting whether
it makes more sense to make the rhythm three weeks instead of two.

## Y Combinator

We spent 45 minutes trying to understand the code.

    (  (  (lambda (f) (lambda (n) (f f n)))

          (lambda (fun x)
    	(if (= x 0)
    	    1
    	    (* x (fun fun (- x 1)))))  )
       5)

Ale made a cool explanation here:
 https://gitlab.com/a.l.e/scheme-sicp-notes/-/blob/main/02/y-combinator.rk

Florence mentioned a blog explaining combinators:
https://mvanier.livejournal.com/2897.html
