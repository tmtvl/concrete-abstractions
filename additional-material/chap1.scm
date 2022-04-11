;; This file contains excerpts from the textbook Concrete
;; Abstractions: An Introduction to Computer Science Using Scheme, by
;; Max Hailperin, Barbara Kaiser, and Karl Knight, Copyright (c) 1998
;; by the authors. Full text is available for free at
;; http://www.gustavus.edu/+max/concrete-abstractions.html

;; Chapter 1: Computer Science and Programming

;; 1.1  What's It All About?

;; 1.2  Programming in Scheme

12
;Value: 12

-7
;Value: -7

1/3
;Value: 1/3

3.1415927
;Value: 3.1415927

sqrt
;Value: #<procedure>

+
;Value: #<procedure>

(sqrt 9)
;Value: 3

(+ 3 6)
;Value: 9

(sqrt (+ 3 6))
;Value: 3

(/ (+ (* (- 17 14)
         5)
      6)
   7)

(define ark-volume (* (* 300 50) 30))

ark-volume
;Value: 450000

(/ ark-volume 8)
;Value: 56250

(+ 1.29 (* 5/100 1.29))
;Value: 1.3545

(+ 2.40 (* 5/100 2.40))
;Value: 2.52

(lambda (x) (+ x (* 5/100 x)))

(lambda (x) (+ x (* 5/100 x)))
;Value: #<procedure>

((lambda (x) (+ x (* 5/100 x))) 1.29)
;Value: 1.3545

((lambda (x) (+ x (* 5/100 x))) 2.40)
;Value: 2.52

(define f (lambda (x) (* x x)))

(define square
  (lambda (x) (* x x)))

(square 3)
;Value: 9

(square -10)
;Value: 100

(define f (lambda (x) (* x x)))
(define square f)

(f 7)
;Value:

(square 7)
;Value:

(define f (lambda (x) (+ x 2)))

(f 7)
;Value:

(square 7)
;Value:

(define cylinder-volume
  (lambda (radius height)
    (* (* 3.1415927 (square radius))
       height)))

(cylinder-volume 5 4)
;Value: 314.15927

(cylinder-volume 5 4)
(* (* 3.1415927 (square 5)) 4)
(* (* 3.1415927 (* 5 5)) 4)
(* (* 3.1415927 25) 4)
(* 78.5398175 4)
314.15927

(define tax
  (lambda (income)
    (if (< income 10000)
        0
        (* 20/100 income))))

(tax 30000)
(if (< 30000 10000) 0 (* 20/100 30000))
(if #f 0 (* 20/100 30000))
(* 20/100 30000)
6000

(define puzzle1
  (lambda (a b c)
    (+ a (if (> b c)
             b
             c))))

(define puzzle2
  (lambda (x)
    ((if (< x 0)
         -
         +)
     0 x)))

;; 1.3  An Application: Quilting

(stack rcross-bb corner-bb)
(quarter-turn-right test-bb)

(stack (stack rcross-bb corner-bb) test-bb)
(stack (stack rcross-bb corner-bb)
       (stack (quarter-turn-right test-bb) test-bb))

(define test-bb
  (filled-triangle 0 1 0 -1 1 -1))

(define nova-bb
  (overlay (filled-triangle 0 1 0 0 -1/2 0)
           (filled-triangle 0 0 0 1/2 1 0)))

;; Review Problems

(define f
  (lambda (x y)
    (if (even? x)
        7
        (* x y))))

(define foo
  (lambda (x y)
    (if _________
        (+ x y)
        (/ x y))))

(define ladder-height
  (lambda (ladder-length base-distance)
    (sqrt (- (square ladder-length)
             (square base-distance)))))
