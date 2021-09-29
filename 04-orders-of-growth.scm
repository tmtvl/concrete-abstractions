(import (rnrs (6)))

(define square
  (lambda (x)
	(* x x)))

(define divisible?
  (lambda (x y)
	(= (remainder x y)
	   0)))

;;; Exercise 4.1
;; Each pair adds up to n + 1.
;; There are n / 2 pairs.
;; This makes it (n / 2) * (n + 1).
;; = (n * (n + 1)) / 2
;; = ((n ^ 2) + n) / 2

;;; Exercise 4.2
(define mod-expt
  (lambda (base exponent modulus)
	(define mod*
	  (lambda (x y)
		(remainder (* x y)
				   modulus)))

	(if (= exponent 0)
		1
		(if (even? exponent)
			(mod* (mod-expt base
							(/ exponent 2)
							modulus)
				  (mod-expt base
							(/ exponent 2)
							modulus))
			(mod* (mod-expt base
							(- exponent 1)
							modulus)
				  base)))))

;; A:
;; 1:
;; Base case:
;; (mod-expt b 0 m) does no multiplications, per the evaluation rules of if. As
;; 0 >= 0 our theorem holds.

;; Induction hypothesis:
;; (mod-expt b k m) does k multiplications for all k in the range 0 <= k < n.

;; Inductive step:
;; (mod-expt b n m) returns either (mod* (mod-expt b (/ n 2) m) (mod-expt b (/
;; n 2) m)) or (mod* (mod-expt b (- n 1) m) b), depending on whether n is even
;; or odd. If n is even we add one multiplication to twice (mod-expt b (/ n 2)
;; m), which per our induction hypothesis is n / 2 multiplications. Therefore
;; if n is even we get n + 1 multiplications.
;; If n is odd we add one multiplication to (mod-expt b (- n 1) m), which per
;; our induction hypothesis is n - 1 multiplications. Therefore if n is odd we
;; get n multiplications.

;; Conclusion:
;; Therefore we can prove through mathematical induction that we do at least n
;; multiplications for any nonnegative integer n.

;; 2:
;; Base case:
;; (mod-expt b 1 m) does one multiplication, by evaluation of if and the base
;; case of A1. As (2 * 1) - 1 <= 1, our theorem holds.

;; Induction hypothesis:
;; (mod-expt b k m) does 2k - 1 multiplications for all k in the range 1 <= k <
;; n.

;; Inductive step:
;; (mod-expt b n m) returns either (mod* (mod-expt b (/ n 2) m) (mod-expt b (/
;; n 2) m)) or (mod* (mod-expt b (- n 1) m) b), depending on whether n is even
;; or odd. If n is even we add one multiplication to twice (mod-expt b (/ n 2)
;; m), which per our induction hypothesis is (2 * (n / 2)) - 1 = n - 1
;; multiplications. Therefore if n is even we get (2 * (n - 1)) + 1 = 2n - 1
;; multiplications.
;; If n is odd we add one multiplication to (mod-expt b (- n 1) m), which per
;; our induction hypothesis is (2 * (n - 1)) - 1 = 2n - 3 multiplications.
;; Therefore if n is odd we get (2n - 3) + 1 = 2n - 2 multiplications.

;; Conclusion:
;; Therefore we can prove through mathematical induction that we do at most 2n
;; - 1 multiplications for any positive integer n.

;; B:
;; Base case:
;; (mod-expt b 0 m) does 0 multiplications, per the evaluation rule for if. As
;; 0 <= 2 * 0, our theorem holds.

;; Induction hypothesis:
;; (mod-expt b k m) does 2k multiplications for all k in the range 0 <= k < n.

;; Inductive step:
;; (mod-expt b n m) returns (mod* (mod-expt b (/ n 2) m) (mod-expt b (/
;; n 2) m)) or (mod* (mod-expt b (- n 1) m) b), depending on whether n is even
;; or odd. If n is even we add one multiplication to twice (mod-expt b (/ n 2)
;; m), which per our induction hypothesis is 2 * (n / 2) = n. However, as this
;; means we perform 2n + 1 multiplications our theorem fails.

;; Conclusion:
;; Therefore mathematical induction will not work if we want to prove that
;; (mod-expt b n m) performs at most 2n multiplications.

;;; Exercise 4.3
(define mod-expt
  (lambda (base exponent modulus)
	(define mod*
	  (lambda (x y)
		(remainder (* x y)
				   modulus)))

	(if (= exponent 0)
		1
		(if (even? exponent)
			(let ((x (mod-expt base
							   (/ exponent 2)
							   modulus)))
			  (mod* x x))
			(mod* (mod-expt base
							(- exponent 1)
							modulus)
				  base)))))

(define calculate-multiplications
  (lambda (exponent)
	(define decrement-exponent-step
	  (lambda (exponent result)
		(if (= exponent 0)
			result
			(decrement-exponent-step
			 (if (even? exponent)
				 (/ exponent 2)
				 (- exponent 1))
			 (+ result 1)))))

	(decrement-exponent-step exponent 0)))

;;; Exercise 4.4
(define mod-expt
  (lambda (base exponent modulus)
	(define mod*
	  (lambda (x y)
		(remainder (* x y)
				   modulus)))

	(define iterate
	  (lambda (b e result)
		(cond ((= e 0)
			   result)
			  ((even? e)
			   (iterate (square b)
						(/ e 2)
						result))
			  (else
			   (iterate b
						(- e 1)
						(mod* b result))))))

	(iterate base exponent 1)))

;;; Exercises 4.5 through 4.10 will be handled seperately.
;;; Exercise 4.11
;; A: (factorial n) performs n multiplications.
;; B: (factorial-sum1 n) performs (n / 2) * (n + 1) = (n ^ 2) + n / 2
;; multiplications.
;; C: (factorial-sum2 n) performs n multiplications.

;;; Exercise 4.12
(define ways-to-factor
  (lambda (n)
	(ways-to-factor-using-no-smaller-than n 2)))

(define ways-to-factor-using-no-smaller-than
  (lambda (n m)
	(define count-ways
	  (lambda (m result)
		(if (> (square m)
			   n)
			result
			(count-ways (+ m 1)
						(if (divisible? n m)
							(+ result 1)
							result)))))

	(count-ways m 0)))

;;; Exercise 4.13
;; O(n/2).

;;; Exercise 4.14
;; (factorial n) does n multiplications.
;; (bar n n) does n times (factorial n), for n^2 multiplications.
;; Therefore (foo n) does n^2 + n multiplications so O(n^2).

;;; Exercise 4.15
;; A:
(define maximum-weightings
  (lambda (n)
	(define subdivide-weight
	  (lambda (n result)
		(cond ((= n 1)
			   result)
			  ((even? n)
			   (subdivide-weight (/ n 2)
								 (+ result 1)))
			  (else
			   (subdivide-weight (/ (- n 1)
									2)
								 (+ result 1))))))

	(subdivide-weight n 0)))

;; B:
;; If there is 1 coin remaining, we are done.
;; If there are 2 coins remaining, weigh them and take the lighter one.
;; Else divide the pile into 3 parts: I, II, and III, where I and II are equal
;; in amount. Weigh I and II. If either is lighter, remove III and the heavier
;; of I and II and start again with the lighter. Otherwise remove I and II and
;; start again with III.

;; C:
(define maximum-weightings-with-thirds
  (lambda (n)
	(define subdivide-weight
	  (lambda (n result)
		(cond ((= n 1)
			   result)
			  ((= n 2)
			   (+ result 1))
			  (else
			   (let ((next (+ (quotient n 3)
							  (remainder n 3))))
				 (subdivide-weight next
								   (+ result 1)))))))

	(subdivide-weight n 0)))

;;; Exercise 4.16 will be handled seperately.
;;; Exercise 4.17
(define C
  (lambda (n k)
	(if (or (= k 0)
			(= k n))
		1
		(+ (C (- n 1)
			  (- k 1))
		   (C (- n 1)
			  k)))))

;;; Exercise 4.18
;; A:
(define sum-from-to
  (lambda (a b)
	(if (= a b)
		a
		(let ((midpoint (quotient (+ a b)
								  2)))
		  (+ (sum-from-to a midpoint)
			 (sum-from-to (+ midpoint 1)
						  b))))))

;; B:
;; In the base case of n being 1, we perform 0 additions.
;; Otherwise we perform (2 * (n/2 - 1)) + 2 = n additions, rounded down to the
;; nearest even number, for something like O(n) additions.

;;; Exercise 4.19
;; Well, we can see that the image is a fractal, so it will be recursive in a
;; way. As every level is surrounded by 3 smaller copies it will probably be
;; tree recursive, as it is natural to go down every branch without having to
;; keep track of what we have done.

;;; Exercise 4.20
;; A: Tree recursive, as we go down one branch before backtracking to go down
;; the next one.

;; B:
;; 1 | 1
;; 2 | 3
;; 3 | 7
;; 4 | 15
;; l(n) = 2 * l(n - 1) + 1
;; We add 1 for the mid-line which gets overlayed over the mirrored
;; subsections. As the subsections are mirrored we multiply them by 2.
;; l(5) = 31.
