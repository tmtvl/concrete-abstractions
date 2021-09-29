(import (rnrs (6)))

(define square
  (lambda (x)
	(* x x)))

;;; Exercises 5.1 through 5.3 will be handled seperately.
;;; Exercise 5.4
(define num-digits-in-satisfying
  (lambda (n test?)
	(cond ((< n 0)
		   (num-digits-in-satisfying (- n)
									 test?))
		  ((< n 10)
		   (if (test? n) 1 0))
		  ((test? (remainder n 10))
		   (+ (num-digits-in-satisfying (quotient n 10)
										test?)
			  1))
		  (else
		   (num-digits-in-satisfying (quotient n 10)
									 test?)))))

(define num-odd-digits
  (lambda (n)
	(num-digits-in-satisfying n odd?)))

(define num-6s
  (lambda (n)
	(num-digits-in-satisfying n
							  (lambda (n)
								(= n 6)))))

(define num-digits
  (lambda (n)
	(num-digits-in-satisfying n identity)))

;;; Exercise 5.5
(define num-digits-in-satisfying
  (lambda (n test?)
	(define count-digits-satisfying
	  (lambda (m digits)
		(if (< m 10)
			(+ digits
			   (if (test? m) 1 0))
			(count-digits-satisfying (quotient m 10)
									 (+ digits
										(if (test? (remainder m 10)) 1 0))))))

	(count-digits-satisfying (if (< n 0)
								 (- n)
								 n)
							 0)))

;;; Exercise 5.6
(define map-sum
  (lambda (f low high)
	(define sum-range
	  (lambda (x total)
		(if (> x high)
			total
			(sum-range (+ x 1)
					   (+ total
						  (f x))))))

	(sum-range low 0)))

(map-sum square 5 10) ;; 355
(map-sum sqrt 10 100) ;; 652.156946577112

;;; Exercise 5.7
(define make-exponentiator
  (lambda (e)
	(lambda (n)
	  (expt n e))))

(define square
  (make-exponentiator 2))

(define cube
  (make-exponentiator 3))

(square 4) ;; 16
(cube 4) ;; 64

;;; Exercise 5.8
(define make-down-combiner
  (lambda (f)
	(define countdown
	  (lambda (n result)
		(if (= n 1)
			result
			(countdown (- n 1)
					   (f n result)))))

	(lambda (n)
	  (countdown n 1))))

(define factorial
  (make-down-combiner *))

(define sum-of-first
  (make-down-combiner +))

(factorial 6) ;; 720
(sum-of-first 6) ;; 21

;;; Exercise 5.9
(define map-combine-range
  (lambda (mapper combinator initial)
	(define countdown
	  (lambda (x result)
		(if (= x 0)
			result
			(countdown (- x 1)
					   (combinator (mapper x)
								   result)))))

	(lambda (n)
	  (countdown n initial))))

(define factorial
  (map-combine-range identity * 1))

(define sum-of-first
  (map-combine-range identity + 0))

(define sum-of-squares
  (map-combine-range square + 0))

(define sum-of-cubes
  (map-combine-range cube + 0))

(factorial 6) ;; 720
(sum-of-first 6) ;; 21
(sum-of-squares 6) ;; 91
(sum-of-cubes 6) ;; 441

;;; Exercise 5.10
(define sum-of-digits
  (lambda (n)
	(define sum-plus
	  (lambda (n addend)
		(if (= n 0)
			addend
			(sum-plus (quotient n 10)
					  (+ addend
						 (remainder n 10))))))

	(sum-plus n 0)))

(define evenly-divisible-by?
  (lambda (x y)
	(= (remainder x y)
	   0)))

(define digits-divisible-by-17?
  (lambda (n)
	(evenly-divisible-by?
	 (sum-of-digits n)
	 17)))

;;; Exercise 5.11
(define map-sum-digits
  (lambda (f n)
	(define sum-map
	  (lambda (n index total)
		(if (= n 0)
			total
			(sum-map (quotient n 10)
					 (+ index 1)
					 (+ (f index
						   (remainder n 10))
						total)))))

	(sum-map n 1 0)))

(define verify
  (lambda (f m)
	(lambda (n)
	  (evenly-divisible-by? (map-sum-digits f n)
							m))))

(define check-isbn (verify * 11))

(check-isbn 0262010771) ;; #t

;;; Exercise 5.12
(define check-upc
  (verify (lambda (i d)
			(if (even? i)
				(* 3 d)
				d))
		  10))

;; Green tea with cranberry
(check-upc 27033197) ;; #t

;;; Exercise 5.13
(define check-cc
  (verify (lambda (i d)
			(cond ((odd? i)
				   d)
				  ((< d 5)
				   (* 2 d))
				  (else
				   (+ (* 2 d)
					  1))))
		  10))

(check-cc 6011302631452178) ;; #t
;; Guess the hypothetical order taker mistyped.

;;; Exercise 5.14
(define check-usps-mo
  (verify (lambda (i d)
			(if (= i 1)
				(- d)
				d))
		  9))

(check-usps-mo 48077469777) ;; #f
(check-usps-mo 48077462766) ;; #t

;; In the second number two arbitrary numbers with an index > 1 have been
;; swapped, which cannot be detected due to the sum of all digits excluding the
;; rightmost being position-independent. Ergo, 02446677786 (digits being sorted
;; in increasing order) would have the same sum.

;;; Exercise 5.15
(define make-function-with-exception
  (lambda (x y f)
	(lambda (n)
	  (if (= n x)
		  y
		  (f n)))))

(define usually-sqrt
  (make-function-with-exception 7 2 sqrt))

(usually-sqrt 9) ;; 3
(usually-sqrt 16) ;; 4
(usually-sqrt 7) ;; 2

;;; Exercise 5.16
(define compose2
  (lambda (f g)
	(lambda (x)
	  (f (g x)))))

((compose2 sqrt abs) -4) ;; 2

;;; Exercise 5.17
(define integer-in-range-where-smallest
  (lambda (f a b)
	(if (= a b)
		a
		(let ((smallest-place-after-a
			   (integer-in-range-where-smallest f
												(+ a 1)
												b)))
		  (if (< (f a)
				 (f smallest-place-after-a))
			  a
			  smallest-place-after-a)))))

(integer-in-range-where-smallest (lambda (x)
								   (- (* x x)
									  (* 2 x)))
								 0
								 4) ;; 1

;;; Exercise 5.18
;; A:
;; (mystery x) returns (* 3 (+ x 1)), therefore (mystery 4) returns
;; (* 3 (+ 1 4)), which evaluates to (* 3 5), which returns 15.
;; B:
;; ((make-scaled 2 (make-scaled 3 add-one)) 4) returns (* 2 (* 3 (+ 1 4))),
;; which evaluates to (* 2 (* 3 5)), which evaluates to (* 2 15), which returns
;; 30.

;;; Exercise 5.19
(define increasing-on-integer-range?
  (lambda (f l h)
	(define increase?
	  (lambda (x previous)
		(if (> x h)
			#t
			(let ((next (f x)))
			  (if (> next previous)
				  (increase? (+ x 1)
							 next)
				  #f)))))

	(increase? l
			   (- (f l)
				  1))))

(increasing-on-integer-range? (lambda (x)
								(- (* 6 x)
								   (* x x)))
							  0
							  3) ;; #t

(increasing-on-integer-range? (lambda (x)
								(- (* 5 x)
								   (* x x)))
							  0 4) ;; #f

;;; Exercise 5.20
;; A: Procedure of 2 arguments.
;; B: Procedure of 1 argument.
;; C: Error, trying to multiply a procedure with a number.
;; D: Number 20.
;; E: Error, insufficient arguments passed to procedure.
;; F: Number 27.

;;; Exercise 5.21
(define make-generator
  (lambda (f)
	(lambda (n)
	  (lambda (x)
		(f x n)))))

(define make-multiplier
  (make-generator *))

(define make-exponentiator
  (make-generator expt))

;;; Exercise 5.22
;; A:
;; (define halts? (lambda (f) (halts-on? f run-forever)))
;; B:
;; A halts-on? wouldn't work because it can be turned on itself.

;;; Exercise 5.23
(define make-averaged-procedure
  (lambda (f g)
	(lambda (x)
	  (/ (+ (f x)
			(g x))
		 2))))

(define double (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))
(define new-procedure
  (make-averaged-procedure double square))

(new-procedure 4) ;; 12
(new-procedure 6) ;; 24

;;; Exercise 5.24
;; A:
;; As we call the procedure twice when next-to-try (starting at 2) > n, it
;; doesn't get called when n = 1. Therefore for any positive integer n, f is
;; called (n - 1) * 2 times.
;; B:
;; O(2n), as it grows by 2 calls for every 1 increase of n.
;; C:
;; It could be rewritten to pass the value of (f smallest-so-far) along, so we
;; would only have to call f once for every increase of n.
