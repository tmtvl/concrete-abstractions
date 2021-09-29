;;; Exercise 3.1
(define factorial-product
  (lambda (iteration result limit)
	(if (> iteration limit)
		result
		(factorial-product
		 (+ iteration 1)
		 (* result iteration)
		 limit))))

(define factorial
  (lambda (n)
	(factorial-product 2 1 n)))

;;; Exercise 3.2
(define even-division-counter
  (lambda (numerator divisions)
	(if (even? numerator)
		(even-division-counter
		 (/ numerator 2)
		 (+ divisions 1))
		divisions)))

(define exponent-of-two
  (lambda (n)
	(even-division-counter n 0)))

;;; Exercise 3.3 will be handled seperately.
;;; Exercise 3.4
(define power-product
  (lambda (a b e)
	(if (= e 0)
		a
		(power-product (* a b)
					   b
					   (- e 1)))))

;; Base case: If e = 0 and a = 1, it follows from the way if expressions work
;; that we get 1 as result. As any b ^ 0 = 1, this means our theorem holds.

;; Induction hypothesis: Assume that a = b ^ x for n = e + x and
;; (power-product a b e) returns a * (b ^ e) for all e in the range 0 <= e <= n.

;; Inductive step: Now we find that (power-product 1 b n) will return the value
;; of (power-product (* 1 b) b (- n 1)), given our induction hypothesis, this
;; results in the value b * (b ^ (n - 1)) = b ^ n.

;; Conclusion: Therefore we find, by mathematical induction, that the value of
;; (power-product 1 b n) = b ^ n for any nonnegative integer n.

;;; Exercise 3.5
;; (define even-division-counter
;;   (lambda (numerator divisions)
;;     (if _
;;         _
;;         _)))

;; Base case: If numerator is not even and divisions = 0, it follows from the
;; way if expressions work that the result is 0.

;; Induction hypothesis: Assume that (even-division-counter numerator
;; divisions) results in divisions + 1 for any even numerator in the range 0 <
;; numerator < n and divisons for any odd numerator.

;; Inductive step: For any even integer n in (even-division-counter n 0) we
;; will return the value of (even-division-counter (n / 2) 1), which per our
;; induction hypothesis will return 1 + (even-division-counter (n / 2) 0).

;; Conclusion: Therefore we find that for any integer n, (even-division-counter
;; n 0) will return the even divisions of n.

;;; Exercise 3.6
(define divides?
  (lambda (x y)
	(= (remainder x y) 0)))

(define square
  (lambda (x)
	(* x x)))

(define sum-of-divisors
  (lambda (n)
	(define sum-from-plus
	  (lambda (candidate result)
		(let ((c-squared (square candidate)))
		  (cond ((> c-squared n)
				 result)
				((= c-squared n)
				 (+ result candidate))
				(else
				 (sum-from-plus
				  (+ candidate 1)
				  (if (divides? n candidate)
					  (+ result
						 candidate
						 (/ n candidate))
					  result)))))))

	(sum-from-plus 1 0)))

;;; Exercise 3.7
(define improve
  (lambda (approx)
	(+ 1
	   (/ 1 approx))))

;;; Exercise 3.8
(define approximate-golden-ratio
  (lambda (tolerance)
	(define good-enough?
	  (lambda (approximation)
		(< (/ 1
			  (square (denominator approximation)))
		   tolerance)))

	(define find-approximation-from
	  (lambda (starting-point)
		(if (good-enough? starting-point)
			starting-point
			(find-approximation-from (improve starting-point)))))

	(find-approximation-from 1)))

(approximate-golden-ratio (/ 1
							 (expt 10 79)))
;; 5972304273877744135569338397692020533504/
;; 3691087032412706639440686994833808526209

;;; Exercise 3.9
;; Position 1 becomes position n - 2
;; Position 2 becomes position n - 1

;;; Exercise 3.10
(define renumber
  (lambda (position n)
	(if (< position 3)
		(- (+ n position)
		   3)
		(- position 3))))

;;; Exercise 3.11
(define survives?
  (lambda (position n)
	(cond ((< n 3)
		   #t)
		  ((= position 3)
		   #f)
		  (else
		   (survives?
			(renumber position n)
			(- n 1))))))

;; Survives: 13 of 13
(survives? 13 13) ;; #t
;; Dies: 5 of 10
(survives? 5 10) ;; #f
;; Dies: 8 of 9
(survives? 8 9) ;; #f
;; Survives: 20 of 20
(survives? 20 20) ;; #t
;; Survives: 1 of 16
(survives? 1 16) ;; #t

;;; Exercise 3.12
(define first-survivor-after
  (lambda (position n)
	(let ((next (+ position 1)))
	  (cond ((> next n)
			 #f)
			((survives? next n)
			 next)
			(else
			 (first-survivor-after next n))))))

(first-survivor-after 0 40) ;; 13
(first-survivor-after 13 40) ;; 28

;;; Exercise 3.13
;; An iterative process, by halving even numbers we transform the problem into
;; a smaller one with the same solution.

;;; Exercise 3.14
;; A: It is a recursive process since we leave some work (the increment) for
;; when the smaller problem has been solved.
;; B:
(define closest-power
  (lambda (b n)
	(define quotient-step
	  (lambda (n result)
		(if (< n b)
			result
			(quotient-step (quotient n b)
						   (+ result 1)))))

	(quotient-step n 0)))

;;; Exercise 3.15
;; A:
;; (f 1)
;; (if (= 1 0)
;;     0
;;     (if (= 0 0)
;;         1))
;; (f 2)
;; (if (= 2 0)
;;     0
;;     (if (= 1 0)
;;         1
;;         (if (= 0 0)
;;             0)))
;; (f 3)
;; (if (= 3 0)
;;     0
;;     (if (= 2 0)
;;         1
;;         (f 1)))

;; B: Yes: (f 4) => 0, (f 5) => 1.
;; Even positive integers return 0, odd ones 1.

;; C: Iterative, if (f x) and (g (- x 1)) don't solve the problem we iterate
;; with (- x 2).

;;; Exercise 3.16
;; A:
;; (f 2)
;; (if (= 2 0)
;;     0
;;     (+ 1 (if (= 1 0)
;;              1
;;              (+ 1 (if (= 0 0)
;;                       0)))))
;; (f 3)
;; (if (= 3 0)
;;     0
;;     (+ 1 (if (= 2 0)
;;              1
;;              (+ 1 (if (= 1 0)
;;                       0
;;                       (+ 1 (if (= 0 0))
;;                                1)))))))
;; (f 4)
;; (if (= 4 0)
;;     0
;;     (+ 1 (if (= 3 0)
;;              1
;;              (+ 1 (f 2)))))

;; B: Recursive: first we solve the problem of (- n 1), then we add 1.

;; C: 6 and 6.

;;; Exercise 3.17
(define falling-factorial-power
  (lambda (n e)
	(define multiply-step
	  (lambda (step result)
		(if (= step e)
			result
			(multiply-step (+ step 1)
						   (* result
							  (- n step))))))

	(multiply-step 0 1)))

;;; Exercise 3.18
;; A:
(define power
  (lambda (b n)
	(define decrement-step
	  (lambda (n result)
		(if (= n 0)
			result
			(decrement-step (- n 1)
							(* result b)))))

	(define increment-step
	  (lambda (n result)
		(if (= n 0)
			result
			(increment-step (+ n 1)
							(/ result b)))))

	((if (negative? n)
		 increment-step
		 decrement-step)
	 n
	 1)))

;; B:
;; (power 2 -3)
;; (increment-step -3 1)
;; (increment-step -2 1/2)
;; (increment-step -1 1/4)
;; (increment-step 0 1/8)
;; Iterative.

;;; Exercise 3.19
;; Base case: (foo 0 a) returns a. Since 2 ^ 0 = 1 and 1 * a = a, the base case
;; holds.

;; Induction hypothesis: Assume that (foo k a) returns (2 ^ k) * a for all k in
;; the range 0 <= k < x.

;; Inductive step: (foo x a) will return (foo (- x 1) (+ a a)). Given our
;; induction hypothesis, this evaluates to (2 ^ (- n 1)) * 2a = (2 ^ n) * a.

;; Conclusion: Therefore (foo n a) returns (2 ^ n) * a for any nonnegative
;; integer a.

;;; Exercise 3.20
;; A:
;; (factorial 4)
;; (product 1 4)
;; (* 1 (product 2 4))
;; (* 1 (* 2 (product 3 4)))
;; (* 1 (* 2 (* 3 (product 4 4))))
;; (* 1 (* 2 (* 3 (* 4 (product 5 4)))))
;; (* 1 (* 2 (* 3 (* 4 1))))
;; (* 1 (* 2 (* 3 4)))
;; (* 1 (* 2 12))
;; (* 1 24)
;; 24

;; B:
;; Recursive, we solve the problem of (product (+ low 1) high) first, and then
;; we multiply that be low.

;; C:
;; Product computes the falling factorial power from low to the target high as
;; in (product x y) = (falling-factorial-power x (- x (- y 1))).
