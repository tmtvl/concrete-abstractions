(import (rnrs (6)))

;;; Exercise 1.1
(* 3 4)
;; 12
(* (+ 5 3)
   (- 5 3))
;; 16
(/ (+ (* (- 17 14)
		 5)
	  6)
   7)
;; 3

;;; Exercise 1.2
(define add-tax
  (lambda (x)
	"Returns the total price of an item with cost X."
	(+ x
	   (* x 5/100))))

(add-tax 1.29) ;; 1.3545
(add-tax 2.40) ;; 2.52

;;; Exercise 1.3
(define f
  (lambda (x)
	(* x x)))

(define square f)

(f 7) ;; 49
(square 7) ;; 49

(define f
  (lambda (x)
	(+ x 2)))

(f 7) ;; 9
(square 7) ;; 49

;;; Exercise 1.4
(define candy-temperature
  (lambda (temperature elevation)
	"Reduces the TEMPERATURE given in ambiguous degrees by 1 for every 500
feet of ELEVATION."
	(round (- temperature
			  (/ elevation 500)))))

(candy-temperature 244 5280) ;; 233

;;; Exercise 1.5
(define tax
  (lambda (income)
	"Calculates marginal tax rate for the given INCOME."
	(if (< income 10000)
		0
		(* (- income 10000)
		   20/100))))

(tax 9999) ;; 0
(tax 12500) ;; 500

;;; Exercise 1.6
(define turkey-servings
  (lambda (weight)
	"Calculates the servings that can be expected from a turkey of the given
WEIGHT."
	(if (< weight 12)
		(/ weight 3/4)
		(/ weight 1/2))))

(turkey-servings 9) ;; 12
(turkey-servings 12) ;; 24

;;; Exercise 1.7
(define add-max
  (lambda (base x y)
	"Adds the larger of X or Y to the provided BASE."
	(+ base
	   (if (> x y)
		   x
		   y))))

(define absolute-value
  (lambda (x)
	"Returns the absolute value of X."
	((if (< x 0)
		 -
		 +)
	 0 x)))

;;; Exercises 1.8 through 1.11 will be handled separately.
;;; Exercise 1.12
(define f
  (lambda (x y)
	(if (even? x)
		7
		(* x y))))

(f 1 16) ;; 16

;;; Exercise 1.13
(- (* (* 4 5)
	  9)
   7)
;; 173

;;; Exercise 1.14
(define average-two-numbers
  (lambda (x y)
	(/ (+ x y)
	   2)))

(average-two-numbers 20 200) ;; 110

;;; Exercise 1.15
(define foo
  (lambda (x y)
	(if (< y 1)
		(+ x y)
		(/ x y))))

(define foo
  (lambda (x y)
	(if (> y -1)
		(+ x y)
		(/ x y))))

(define foo
  (lambda (x y)
	(if (= y 0)
		(+ x y)
		(/ x y))))

(define foo
  (lambda (x y)
	(if (even? y)
		(+ x y)
		(/ x y))))

;;; Exercise 1.16
(define ladder-height
  (lambda (ladder-length ladder-distance)
	(sqrt (+ (square ladder-length)
			 (square ladder-distance)))))

(ladder-height 10 6) ;; 11.661903789690601
