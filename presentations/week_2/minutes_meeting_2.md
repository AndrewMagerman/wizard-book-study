# Minutes of Review Week 2

## General round-up

Nicolas asked all attendees what was their current status. The following notes are "soft" info, nothing crucial for the program, but might be very useful to get a sense of how everyone is progressing:

Nicolas: Up to date with book, only part-way through lectures and homework. Found three-week interval best, because he needs time distributed on 2 weekends to complete the course work, and if one weekend is busy with something else, there's no margin for error.

Jim: Already spent some time last year playing with Scheme, so not starting from zero. He’s a bit ahead for now. He’s more or less caught up with exercises this week. Did have trouble with a couple exercises. Used a couple tools that he’s not sure if he’s supposed to know at this point.

Richard: Programming language is new to him, very interesting concepts. Finds the mathematics quite challenging. Not his area of interest. Been programming a long time, very used to certain structures, mostly iterative (misses while and for loops). Interesting to get more into recursion which was not so usable when he started programming. Scheme language seems to be quite based on recursion.

Giammi: Watched the online class, tried to do the exercises and was not able to do them. Then decided to read chapter 1.2 to see if it would fill some gaps. Having difficulties to understand the language of the exercises, “what they want from him”. Math that he hasn’t seen in 25 years. Wasted some time to look at things and understand the sentences rather than do the actual programming. Over the weekend read an entire book about Lisp. In general so far, interesting but challenging.

Ale: These are mathematics which he’d never learned. Interesting, things he’s never done before. He could not finish everything due to Easter. Finds it really a lot of work between book, lessons, exercises... Generally interesting to do this. Similar experience to others.

Florence: A bit behind. First week didn’t finish the homework. This week she caught up with homework week 1 and lab week 2 but not homework week 2. She read the whole book section at the very beginning, but then when she went to the homework she’d already forgotten stuff. Maybe better to do the exercises directly after reading. Struggling most with the syntax, DrRacket… she’s used to being spoiled with IDEs.

Harun: Really struggled with the homework this week. Finds the mathematics hard, but thinks the best approach is to ignore the mathematics, take them as a given and just try to implement what they want. After struggling with the homework, he did manage to solve all the section 1.3 exercises. Fully caught up with the videos. Things are working well for him, and he is loving Scheme. He normally works with C and C++ but he’s really enjoying this, happy with the language.

Nico: behind quite a bit, managed to do a bit over Easter but didn’t manage to get to the exercises in the book. Did manage to do the labs. Struggling mostly with the time, not having time to do it. He likes the study group to keep moving along.

Andrew: Finding this to be quite a lot of work. But actually he wanted the group in order to have peer pressure to do things on time. So that’s working quite well.
Enjoyed the exercises, didn't really understand the counting change program, nor the y combinator homework. Enjoying the struggle, different from his day to day work which is more like “a knife going through butter". Here he’s really struggling and enjoying that.


## Insights

- Mind-blowing insight for Andrew: That a recursive procedure is not the same as a recursive process. You can have an iterative process in a recursive function.

- Missing the capacity to use global state. You can use block scope and define things inside a scope, but you still need to pass the variables around as arguments. It's an alien concept that when you’re iterating you’re passing around the indexes together with the stuff you’re actually calculating.

- Cool realisation: That for any recursive procedure you can also do it iteratively.


## Collected challenges and tips

- The english used is advanced and difficult to read as a non-english speaker.
	- Harun mentioned that the book exists in German: https://www.springer.com/de/book/9783642977275 and in other languages.

- The math is difficult and adds unnecessary complexity.
	- Great tip by Harun: Ignore the mathematics! take them as a given and just try to implement what they want you to implement.

- Hard to cover the different pieces of the program.
	- Recommended order: First labs, then reading, then lectures, then homework.
	- Alternative: A couple people recommend doing the exercises just as you are reading.

- Difficult exercises:
	- Andrew: Working on paper first is helpful, especially using the technique of 'wishful thinking': Start by what you know and how you’re hoping for it to work, leave some blanks. Then you keep building the puzzle, filling the gaps. Hugely satisfying when this works.


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