(define (square x)
  (* x x))

;;; Exercise 2.1
(define power
  (lambda (base exponent)
	(if (zero? exponent)
		1
		(if (even? exponent)
			(power (square base)
				   (/ exponent 2))
			(* base
			   (power base
					  (- exponent 1)))))))

(power 3 3) ;; 27
(power 2 8) ;; 256
(= (power 7 4)
   (expt 7 4))
;; #t

;;; Exercise 2.2
;; Base case: (factorial 1) terminates with the value 1 because of the
;; evaluation rule for if. Because 1 = 1!, (factorial 1) computes the correct
;; value.

;; Induction hypothesis: Assume that (factorial k) terminates with the value k!
;; for all k in the range 1 <= k <= n.

;; Inductive step: Consider evaluating (factorial n), with n > 1. This will
;; terminate if the evaluation of (factorial (- n 1)) does and will have the
;; same value as (* n (factorial (- n 1))). Because (- n 1) evaluates to n - 1
;; and 1 <= n - 1 < n, we can therefore assume by our induction hypothesis that
;; (factorial (- n 1)) does terminate, with the value (n - 1)!. Therefore
;; (* n (factorial (- n 1))) evaluates to n * (n - 1)!. Because n * (n - 1)!
;; = n!, we can see that (factorial n) does terminate with the correct value
;; for any arbitrary n > 1, under the inductive hypothesis of correct operation
;; for smaller arguments.

;; Conclusion: Therefore, by mathematical induction on n, (factorial n)
;; terminates with the value n! for any positive integer n.

;;; Exercise 2.3
;; Base case: (square 0) terminates with the value 0 because of the evaluation
;; rule for if. Because 0^2 = 0, (square 0) computes the correct value.

;; Induction hypothesis: Assume that (square k) terminates with the value k^2
;; for all k in the range 0 <= k <= n.

;; Inductive step: Consider evaluating (square n), with n > 0. This will
;; terminate if the evaluation of (square (- n 2)) does and will have the same
;; value as (+ (square (- n 2)) (- (* 4 n) 4)). However, (- n 2) evaluates to
;; n - 2, which means that (- n 2) can result in the value -1, which is not
;; handled by our induction hypothesis. Therefore we can not guarantee that
;; (square (- n 2)) will terminate. Therefore we can not guarantee that
;; (square n) will terminate for any arbitrary n > 0.

;; Conclusion: Therefore, by mathematical induction on n, we can not guarantee
;; termination of (square n) for any nonnegative integer n.

;;; Exercise 2.4
;; (n / 2)^2 = (n / 2) * (n / 2) = (n * n)/(2 * 2) = (n^2)/4
(define square
  (lambda (n)
	(if (= n 0)
		0
		(if (even? n)
			(* (square (/ n 2))
			   4)
			(+ (square (- n 1))
			   (- (+ n n) 1))))))

;;; Exercise 2.5
(define multiply
  (lambda (x y)
	(cond ((= y 0) 0)
		  ((= y 1) x)
		  ((< y 0)
		   (multiply (- 0 x)
					 (- 0 y)))
		  (else
		   (+ x
			  (multiply x (- y 1)))))))p

;;; Exercise 2.6
;; subtract-the-first computes the negative of sum-of-first.
;; Multiplication and addition are order-agnostic operations.
;; Subtraction is an order-dependant operation.
;; Switching out the order in subtract-first gives the rounded up half of the
;; operand.

;;; Exercise 2.7
(define sum-integers-from-to
  (lambda (low high)
	(if (> low high)
		0
		(+ low
		   (sum-integers-from-to
			(+ low 1)
			high)))))

;;; Exercise 2.8
(define sum-of-squares
  (lambda (n)
	(if (= n 0)
		0
		(+ (square n)
		   (sum-of-squares (- n 1))))))

(define cube
  (lambda (n)
	(* n n n)))

(define sum-of-cubes
  (lambda (n)
	(if (= n 0)
		0
		(+ (cube n)
		   (sum-of-cubes (- n 1))))))

(define sum-of-powers
  (lambda (n p)
	(if (= n 0)
		0
		(+ (expt n p)
		   (sum-of-powers (- n 1)
						  p)))))

;;; Exercise 2.9
(define number-of-sixes
  (lambda (n)
	(cond ((= n 6)
		   1)
		  ((< n 0)
		   (number-of-sixes (- 0 n)))
		  ((< n 10)
		   0)
		  ((= (remainder n 10)
			  6)
		   (+ 1
			  (number-of-sixes (quotient n 10))))
		  (else
		   (number-of-sixes (quotient n 10))))))

(define number-of-ds
  (lambda (n d)
	(cond ((= n d)
		   1)
		  ((< n 0)
		   (number-of-ds (- 0 n)
						 d))
		  ((< n 10)
		   0)
		  ((= (remainder n 10)
			  d)
		   (+ 1
			  (number-of-ds (quotient n 10)
							d)))
		  (else
		   (number-of-ds (quotient n 10)
						 d)))))

;;; Exercise 2.10
(define number-of-odd-digits
  (lambda (n)
	(cond ((< n 0)
		   (number-of-odd-digits (- 0 n)))
		  ((< n 10)
		   (if (odd? n) 1 0))
		  ((odd? n)
		   (+ 1
			  (number-of-odd-digits (quotient n 10))))
		  (else
		   (number-of-odd-digits (quotient n 10))))))

;;; Exercise 2.11
(define sum-digits
  (lambda (n)
	(cond ((< n 0)
		   (- 0
			  (sum-digits (- 0 n))))
		  ((< n 10)
		   n)
		  (else
		   (+ (remainder n 10)
			  (sum-digits (quotient n 10)))))))

;;; Exercise 2.12
(define exponent-of-two
  (lambda (n)
	(if (odd? n)
		0
		(+ 1
		   (exponent-of-two (/ n 2))))))

;;; Exercises 2.13 through 2.15 will be handled separately.
;;; Exercise 2.16
;; Base case: (foo x 0) for any x != 1 terminates with the value 1 because of
;; the evaluation rule for if. Because ((x ^ (0+1)) - 1) / (x - 1) = (x - 1) /
;; (x - 1) = 1, (foo x 0) computes the correct value.

;; Induction hypothesis: Assume that (foo x k) terminates with the value ((x ^
;; (k + 1)) - 1) / (x - 1) for all k in the range 0 <= k <= n and all x != 1.

;; Inductive step: Consider evaluating (foo x n), with n > 0 and x != 1. This
;; will terminate if the evaluation of (foo x (- n 1)) does and will have the
;; same value as (+ (expt x n) (foo x (- n 1))). Because (- n 1) evaluates to
;; n - 1 and 0 <= n - 1 < n, we can therefore assume by our induction
;; hypothesis that (foo x (- n 1)) does terminate, with the value ((x ^ n) - 1)
;; / (x - 1). Therefore (+ (expt x n) (foo x (- n 1))) evaluates to (x ^ n) +
;; (((x ^ n) - 1) / (x - 1)). Because (x ^ n) + (((x ^ n) - 1) / (x - 1)) =
;; (((x ^ (n + 1)) - (x ^ n) + (x ^ n) - 1) / (x - 1)) = (((x ^ (n + 1)) - 1) /
;; (x - 1)), we can see that (foo x n) does terminate with the correct value
;; for any arbitrary n > 0, under the inductive hypothesis of correct operation
;; for smaller arguments.

;; Conclusion: Therefore, by mathematical induction on n, (foo x n) terminates
;; with the value (((x ^ (n + 1)) - 1) / (x - 1)) for any nonnegative integer n
;; and any x != 1.

;;; Exercise 2.17
(define presents-on-day
  (lambda (n)
	(if (= n 1)
		1
		(+ n
		   (presents-on-day (- n 1))))))

(define total-presents
  (lambda (day presents)
	(if (= day 1)
		1
		(+ presents
		   (total-presents (- day 1)
						   (- presents day))))))

(define presents-through-day
  (lambda (n)
	(total-presents
	 n
	 (presents-on-day n))))

;;; Exercise 2.18
;; Base case: (f 0) terminates with the value of 0 due to the evaluation of if.
;; Because 2 * 0 = 0, this means (f 0) calculates the correct value.

;; Induction hypothesis: Assume that (f k) terminates with the value 2k for all
;; k in the range 0 <= k < n.

;; Inductive step: (f n) terminates if (f (- n 1)) terminates and has the value
;; (+ 2 (f (- n 1))). Because (- n 1) terminates with the value n - 1 our
;; induction hypothesis provides (f (- n 1)) terminates with the value 2 * (n -
;; 1) as 0 <= n - 1 < n. Therefore (f n) terminates with the value (2 + (2 * (n
;; - 1))) = 2n for any positive integer n.

;; Conclusion: Therefore by mathematical induction (f n) terminates with the
;; value 2n for any nonnegative integer n.

;;; Exercise 2.19
;; Base case: (foo 0) terminates with the value 2 due to the evaluation of if.
;; Because 2 ^ (2 ^ 0) = 2 ^ 1 = 2, this means (foo 0) calculates the correct
;; value.

;; Induction hypothesis: Assume that (foo k) terminates with the value 2 ^ (2
;; ^ k) for all k in the range 0 <= k < n.

;; Inductive step: (foo n) terminates if (foo (- n 1)) terminates and has the
;; value (expt (foo (- n 1)) 2). Because (- n 1) terminates with the value n -
;; 1, our induction hypothesis provides that (f (- n 1)) terminates with the
;; value 2 ^ (2 ^ (n - 1)) as 0 <= n - 1 < n. Therefore (f n) terminates with
;; the value (2 ^ (2 ^ (n - 1))) ^ 2 = 2 ^ ((2 ^ (n - 1)) * 2) = 2 ^ (2 ^ n)
;; for any positive integer n.

;; Conclusion: Therefore by mathematical induction (foo n) terminates with the
;; value 2 ^ (2 ^ n) for any nonnegative integer n.

;;; Exercise 2.20
;; Base case: (f 0) terminates with the value 0 due to the evaluation of if.
;; Because 0 / (0 + 1) = 0, (f 0) calculates the correct value.

;; Induction hypothesis: Assume that (f k) terminates with the value k / (k +
;; 1) for all k in the range 0 <= k < n.

;; Inductive step: (f n) terminates if (f (- n 1)) terminates and has the value
;; (+ (f (- n 1)) (/ 1 (* n (+ n 1)))). Because (- n 1) terminates with the
;; value n - 1, our induction hypothesis provides that (f (- n 1)) terminates
;; with the value (n - 1) / ((n - 1) + 1) = (n - 1) / n as 0 <= n - 1 < n.
;; Therefore (f n) terminates with the value ((n - 1) / n) + (1 / (n * (n +
;; 1))) = (((n - 1) * (n + 1)) + 1) / (n * (n + 1)) = ((n ^ 2) - n + n - 1 + 1)
;; / ((n ^ 2) + n) = (n ^ 2) / ((n ^ 2) + n) = n / (n + 1) for any positive
;; integer n.

;; Conclusion: Therefore by mathematical induction (f n) terminates with the
;; value n / (n + 1) for any nonnegative integer n.

;;; Exercise 2.21
;; A: The image produced by stack-on-itself has twice the height of the input
;; image.

;; B:
;; Base case: (f image 0) returns the input image, which means the height is
;; equal. Because x * (2 ^ 0) = x, this means (f image 0) terminates with the
;; value an image that is 2 ^ 0 times the height of the input.

;; Induction hypothesis: Assume that (f image k) terminates with the value an
;; image that is 2 ^ k times the height of the input for all k in the range 0
;; <= k < n.

;; Inductive step: (f image n) terminates if (f image (- n 1)) terminates with
;; as value (stack-on-itself (f image (- n 1))). As (- n 1) = n - 1, our
;; induction theory provides that (f image (- n 1)) terminates with as value
;; an image that is 2 ^ (n - 1) the height of the input. Therefore
;; (stack-on-itself (f image (- n 1))) terminates with as value an image that
;; is twice the height of an image that is (2 ^ (n - 1)) the height of the
;; input. 2 * (2 ^ (n - 1)) = 2 ^ n, therefore (stack-on-itself (f image (- n
;; 1))) terminates with as value an image that is 2 ^ n times the height of the
;; input for any positive integer n.

;; Conclusion: Therefore (f image n) terminates with as value an image that is
;; 2 ^ n times the height of the input for any nonnegative integer n.

;;; Exercise 2.22
(define foo
  (lambda (n)
	(if (= n 0)
		0
		(+ (foo (- n 1))
		   (/ 1
			  (- (* 4
					(square n))
				 1))))))

;; A:
(foo 1) ;; 1/3
(foo 2) ;; 2/5
(foo 3) ;; 3/7

;; B:
;; Base case: (foo 0) terminates with the value 0 due to evaluation of if.
;; As 0 / ((2 * 0) + 1) = 0, (foo 0) calculates the value of 0 / ((2 * 0) + 1)
;; correctly.

;; Induction hypothesis: (foo k) terminates with the value k / (2k + 1) for all
;; k in the range 0 <= k < n.

;; Induction step: (foo n) terminates if (foo (- n 1)) terminates and has the
;; value (+ (foo (- n 1)) (/ 1 (- (* 4 (square n)) 1))). (- n 1) = n - 1 and
;; 0 <= n - 1 < n, therefore by our induction hypothesis (foo (- n 1))
;; terminates with the value (n - 1) / ((2 * (n - 1)) + 1) = (n - 1) / ((2 * n)
;; - 1). Therefore (foo n) terminates with the value:; ((n - 1) / ((2 * n) -
;; 1)) + (1 / ((4 * (n ^ 2)) - 1)) = n / ((2 * n) + 1) for any positive
;; integer n.

;; Conclusion: Therefore (foo n) terminates with the value n / ((2 * n) + 1)
;; for any nonnegative integer n.

;;; Exercise 2.23
(define (image-of-digit d)
  (cond ((= d 0)
		 zero-bb)
		((= d 1)
		 one-bb)
		((= d 2)
		 two-bb)
		((= d 3)
		 three-bb)
		((= d 4)
		 four-bb)
		((= d 5)
		 five-bb)
		((= d 6)
		 six-bb)
		((= d 7)
		 seven-bb)
		((= d 8)
		 eight-bb)
		((= d 9)
		 nine-bb)
		(else
		 (error "No image for such a digit."
				d))))

(define (image-of-number n)
  (cond ((< n 0)
		 (error "Number must be positive."
				n))
		((< n 10)
		 (image-of-digit n))
		(else
		 (side-by-side (image-of-number (quotient n 10))
					   (image-of-digit (remainder n 10))))))
