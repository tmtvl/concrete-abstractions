(import (rnrs (6)))

(define square
  (lambda (x)
	(* x x)))

(define cube
  (lambda (x)
	(* x x x)))
;;; Exercise 7.1
;; A: We get the list (#procedure 2 3) because + refers to a procedure that
;; does the addition.
;; B:
(list '+ 2 3) ;; (+ 2 3)

;;; Exercise 7.2
;; (integers-from-to 7 1) would return the empty list. The exact integers that
;; (integers-from-to low high) returns are all the integers k in the range (low
;; <= k <= high). The procedure could be renamed integers-from-upto to indicate
;; that it only counts up. But depending on the exact needs of the user it may
;; be more useful to modify the procedure to count up or down depending on the
;; relation of low and high.

;;; Exercise 7.3
(define even-integers-from-to
  (lambda (from to)
	(let ((count-up? (< from to)))
	  (let ((step (if count-up? 2 -2))
			(passed? (if count-up? > <)))
		(define build-list
		  (lambda (n)
			(if (passed? n to)
				'()
				(cons n
					  (build-list (+ n step))))))

		(build-list (if (even? from)
						from
						(+ from
						   (/ step 2))))))))

(even-integers-from-to 3 9) ;; (4 6 8)
(even-integers-from-to 9 3) ;; (8 6 4)
(even-integers-from-to 5 5) ;; ()
(even-integers-from-to 8 8) ;; (8)

;;; Exercise 7.4
;; (integers-from-to 2 7) would produce the list (7 6 5 4 3 2). This is because
;; we cons every integer on the existing list, ergo (cons low '()), (cons (+
;; low 1) (cons low '())), ...

(define integers-from-to
  (lambda (low high)
	(define iter
	  (lambda (low lst)
		(if (> low high)
			lst
			(iter (+ low 1)
				  (cons low lst)))))

	(reverse (iter low '()))))

(integers-from-to 2 7) ;; (2 3 4 5 6 7)

;;; Exercise 7.5
;; A:
(define accumulate-non-empty-list
  (lambda (combinator)
	(lambda (lst)
	  (define accumulate
		(lambda (lst result)
		  (if (null? lst)
			  result
			  (accumulate (cdr lst)
						  (combinator (car lst)
									  result)))))

	  (if (null? lst)
		  (error 'accumulate-non-empty-list
				 "Provided list is empty!"
				 lst)
		  (accumulate (cdr lst)
					  (car lst))))))

;; B:
(define accumulate-list
  (lambda (combinator base)
	(lambda (lst)
	  (define accumulate
		(lambda (lst result)
		  (if (null? lst)
			  result
			  (accumulate (cdr lst)
						  (combinator (car lst)
									  result)))))

	  (accumulate lst base))))

;;; Exercise 7.6
;; A:
(define count-instances
  (lambda (element lst)
	(define count
	  (lambda (lst instances)
		(if (null? lst)
			instances
			(count (cdr lst)
				   (if (equal? (car lst)
							   element)
					   (+ instances 1)
					   instances)))))

	(count lst 0)))

;; B:
(define count-matches
  (lambda (pred? lst)
	(define count
	  (lambda (lst matches)
		(if (null? lst)
			matches
			(count (cdr lst)
				   (if (pred? (car lst))
					   (+ matches 1)
					   matches)))))

	(count lst 0)))

;;; Exercise 7.7
(list-ref '(1 2 3 4 5) 2) ;; 3
(list-ref '(a b c d) 3) ;; d

(define my-list-ref
  (lambda (lst pos)
	(define iter
	  (lambda (lrem prem)
		(cond ((null? lrem)
			   (error 'my-list-ref
					  "Position out of list bounds."
					  pos
					  lst))
			  ((= prem 0)
			   (car lrem))
			  (else
			   (iter (cdr lrem)
					 (- prem 1))))))

	(iter lst pos)))

(my-list-ref '(1 2 3 4 5) 2) ;; 3
(my-list-ref '(a b c d) 3) ;; d

;;; Exercise 7.8
;; A:
(define member?
  (lambda (element lst)
	(define iter
	  (lambda (lst)
		(cond ((null? lst)
			   #f)
			  ((equal? (car lst)
					   element)
			   #t)
			  (else
			   (iter (cdr lst))))))

	(iter lst)))

(member? 1 '(4 3 2 1)) ;; #t
(member? 'f '(a b c d)) ;; #f

;; B:
(define contains?
  (lambda (pred? lst)
	(define iter
	  (lambda (lst)
		(cond ((null? lst)
			   #f)
			  ((pred? (car lst))
			   #t)
			  (else
			   (iter (cdr lst))))))

	(iter lst)))

(contains? even? '(1 3 7 18)) ;; #t
(contains? number? '(a b c d)) ;; #f

;; C:
(define make-iterator
  (lambda (null-proc matcher? match-proc)
	(lambda (lst)
	  (define iter
		(lambda (lrem)
		  (cond ((null? lrem)
				 (null-proc lst))
				((matcher? (car lrem))
				 (match-proc lrem))
				(else
				 (iter (cdr lrem))))))

	  (iter lst))))

(define find-match
  (lambda (pred? lst)
	((make-iterator (lambda (_)
					  (error 'find-match
							 "No element matching predicate in list."
							 lst))
					pred?
					car)
	 lst)))

(find-match even? '(1 3 7 18)) ;; 18

;; D:
(define all-match?
  (lambda (pred? lst)
	((make-iterator (lambda (_) #t)
					(lambda (e)
					  (not (pred? e)))
					(lambda (_)
					  #f))
	 lst)))

(all-match? even? '(2 4 6 8 10)) ;; #t
(all-match? number? '(7 8 9 a b)) ;; #f

;; E:
;; If the element does not occur we could return #f. If it occurs more than
;; once it may be best to return the first index, as it will make it easier to
;; reach through cdr-ing.
(define index-of
  (lambda (pred? lst)
	(define iter
	  (lambda (lst pos)
		(cond ((null? lst)
			   #f)
			  ((pred? (car lst))
			   pos)
			  (else
			   (iter (cdr lst)
					 (+ pos 1))))))

	(iter lst 0)))

(define position
  (lambda (element lst)
	(index-of (lambda (x)
				(equal? x element))
			  lst)))

(position 50 '(10 20 30 40 50 3 2 1 50)) ;; 4
(position 1 '(a b c d)) ;; #f

;; F:
(define greatest-element
  (lambda (greater? lst)
	(define iter
	  (lambda (lst greatest)
		(if (null? lst)
			greatest
			(iter (cdr lst)
				  (if (greater? (car lst)
								greatest)
					  (car lst)
					  greatest)))))

	(iter (cdr lst) (car lst))))

(greatest-element > '(10 20 30 40 50 3 2 1)) ;; 50
(greatest-element < '(4 3 5 2 6 1 7 8 9)) ;; 1

;; G:
;; We will break ties by keeping the earliest index.
(define greatest-element-indices
  (lambda (greater? lst)
	(define iter
	  (lambda (lst greatest index indices)
		(let ((next-index (+ index 1)))
		  (cond ((null? lst)
				 indices)
				((greater? (car lst)
						   greatest)
				 (iter (cdr lst)
					   (car lst)
					   next-index
					   (list index)))
				((equal? (car lst)
						 greatest)
				 (iter (cdr lst)
					   greatest
					   next-index
					   (cons index indices)))
				(else
				 (iter (cdr lst)
					   greatest
					   next-index
					   indices))))))

	(iter (cdr lst)
		  (car lst)
		  1
		  '(0))))

(define earliest-greatest
  (lambda (greater? lst)
	(greatest-element <
					  (greatest-element-indices greater? lst))))

(define latest-greatest
  (lambda (greater? lst)
	(greatest-element >
					  (greatest-element-indices greater? lst))))

(earliest-greatest > '(10 20 30 40 50 3 2 1 50)) ;; 4
(latest-greatest > '(10 20 30 40 50 3 2 1 50)) ;; 8

;;; Exercise 7.9
;; A:
(define every-corresponding-integer-smaller?
  (lambda (smaller greater)
	(cond ((null? smaller)
		   #t)
		  ((>= (car smaller)
			   (car greater))
		   #f)
		  (else
		   (every-corresponding-integer-smaller?
			(cdr smaller)
			(cdr greater))))))

(every-corresponding-integer-smaller?
 '(1 2 3 4 5)
 '(2 3 4 5 6)) ;; #t
(every-corresponding-integer-smaller?
 '(5 3 2 1 1)
 '(8 5 3 2 1)) ;; #f

;; B:
(define lists-compare?
  (lambda (pred? a b)
	(define iter
	  (lambda (a b)
		(cond ((null? a)
			   #t)
			  ((not (pred? (car a)
						   (car b)))
			   #f)
			  (else
			   (iter (cdr a)
					 (cdr b))))))

	(iter a b)))

(define list-<
  (lambda (l1 l2)
	(lists-compare? < l1 l2)))

(list-< '(1 2 3 4) '(5 6 7 8)) ;; #t

;;; Exercise 7.10
(define my-list-tail
  (lambda (lst n)
	(define iter
	  (lambda (lrem nrem)
		(cond ((= nrem 0)
			   lrem)
			  ((null? lrem)
			   (error 'my-list-tail
					  "Provided number is out of bounds."
					  n
					  lst))
			  (else
			   (iter (cdr lrem)
					 (- nrem 1))))))

	(iter lst n)))

(my-list-tail '(1 2 3 4 5 6 7 8) 4) ;; (5 6 7 8)

;;; Exercise 7.11
;; A:
(define in-order?
  (lambda (integers)
	(define verify
	  (lambda (remaining previous)
		(cond ((null? remaining)
			   #t)
			  ((< (car remaining)
				  previous)
			   #f)
			  (else
			   (verify (cdr remaining)
					   (car remaining))))))

	(verify (cdr integers)
			(car integers))))

(in-order? '(1 2 3 4 5)) ;; #t
(in-order? '(2 5 6 7 8 3 10 11)) ;; #f

;; B:
(define first-elements-of
  (lambda (n lst)
	(list-head lst n)))

(define interleave
  (lambda (lst1 lst2)
	(if (null? lst1)
		lst2
		(cons (car lst1)
			  (interleave lst2
						  (cdr lst1))))))

(define shuffle
  (lambda (deck size)
	(let ((half (quotient (+ size 1)
						  2)))
	  (interleave (first-elements-of half deck)
				  (list-tail deck half)))))

(define shuffle-number
  (lambda (n)
	(define shuffle-until-in-order
	  (lambda (deck shuffles)
		(if (in-order? deck)
			shuffles
			(shuffle-until-in-order (shuffle deck n)
									(+ shuffles 1)))))

	(shuffle-until-in-order (shuffle (integers-from-to 1 n) n)
							1)))

(shuffle-number 52) ;; 8
(shuffle-number 1) ;; 1
(shuffle-number 32) ;; 5

;;; Exercise 7.12
;; We'll have a constructor (make-deck size elements) that makes a deck with
;; the size and elements. Then we can add a (generate-deck size) procedure that
;; makes a new ordered deck. After that we'll make the selectors (deck-size
;; deck) and (deck-elements deck) and use them in the shuffle procedure.
(define make-deck
  (lambda (size elements)
	(cons elements
		  size)))

(define generate-deck
  (lambda (size)
	(make-deck size
			   (integers-from-to 1 size))))

(define deck-size
  (lambda (deck)
	(cdr deck)))

(define deck-elements
  (lambda (deck)
	(car deck)))

(define shuffle
  (lambda (deck)
	(let ((half (quotient (+ (deck-size deck)
							 1)
						  2)))
	  (make-deck (deck-size deck)
				 (interleave (first-elements-of half
												(deck-elements deck))
							 (list-tail (deck-elements deck)
										half))))))

(define shuffle-number
  (lambda (n)
	(define shuffle-until-in-order
	  (lambda (deck shuffles)
		(if (in-order? (deck-elements deck))
			shuffles
			(shuffle-until-in-order (shuffle deck)
									(+ shuffles 1)))))

	(shuffle-until-in-order (shuffle (generate-deck n))
							1)))

(shuffle-number 59) ;; 58

;;; Exercise 7.13
;; A:
(define first-n-squares
  (lambda (n)
	(map square
		 (integers-from-to 1 n))))

(first-n-squares 15)
;; (1 4 9 16 25 36 49 64 81 100 121 144 169 196 225)

;; B:
(define map-first-n-integers
  (lambda (proc)
	(lambda (n)
	  (map proc
		   (integers-from-to 1 n)))))

(define double
  (lambda (x)
	(* x 2)))

(define first-n-even-integers
  (map-first-n-integers double))

(first-n-even-integers 10)
;; (2 4 6 8 10 12 14 16 18 20)

;; C:
(define sevens
  (map-first-n-integers (lambda (_) 7)))

(sevens 4) ;; (7 7 7 7)

;; D:
(define list-of-lists
  (lambda (l)
	(map (lambda (n)
		   (integers-from-to 1 n))
		 l)))

(list-of-lists '(1 5 3))
;; ((1) (1 2 3 4 5) (1 2 3))

;;; Exercise 7.14
(define my-map
  (lambda (proc l)
	(if (null? l)
		'()
		(cons (proc (car l))
			  (my-map proc
					  (cdr l))))))

;;; Exercise 7.15
;; When the amount is 0 there is 1 possible combination: no prizes.
;; When the amount is less than 0 there are no possible combinations.
;; When the prize list is null there are no possible combinations.
(define count-combos
  (lambda (prize-list amount)
	(cond ((= amount 0)
		   1)
		  ((< amount 0)
		   0)
		  ((null? prize-list)
		   0)
		  (else
		   (+ (count-combos prize-list
							(- amount
							   (car prize-list)))
			  (count-combos (cdr prize-list)
							amount))))))

(count-combos '(5 3 1 1) 5) ;; 10

;;; Exercise 7.16
;; Which one is best depends on the distribution of prize values. In the case
;; provided every positive integer up to 10 has a number of prizes. Therefore
;; representing it as a list of numbers is more succinct than using pairs.
;; However, if we were considering the case where the cost of a prize is in a
;; currency the distribution may be more sparse. Then using pairs would prevent
;; us having to parse large stretches of zeroes.
;; In the case provided we may as well simply pass 10 numbers, representing the
;; amount of prizes at 1, 2, ..., 10 tickets.

;;; Exercise 7.17
(define construct-prize-list
  (lambda (quantities)
	(define build-list
	  (lambda (amounts value res)
		(cond ((null? amounts)
			   res)
			  ((= (car amounts)
				  0)
			   (build-list (cdr amounts)
						   (+ value 1)
						   res))
			  (else
			   (build-list (cons (- (car amounts)
									1)
								 (cdr amounts))
						   value
						   (cons value res))))))

	(build-list quantities 1 '())))

(construct-prize-list '(2 0 1 0 1)) ;; (5 3 1 1)
(count-combos (construct-prize-list '(2 4 3 3 4 3 4 2 3 9))
			  10)
;; 1778

;;; Exercise 7.18
(define sum
  (lambda (integers)
	(define S
	  (lambda (is res)
		(if (null? is)
			res
			(S (cdr is)
			   (+ (car is)
				  res)))))

	(S integers 0)))

(define number-of-possible-combinations
  (lambda (prize-list max-amount)
	(sum (map (lambda (n)
				(count-combos prize-list n))
			  (integers-from-to 1 max-amount)))))

(number-of-possible-combinations '(5 3 1 1)
								 4)
;; 17

;;; Exercise 7.19
(define count-limited-combos
  (lambda (prize-list amount)
	(cond ((= amount 0)
		   1)
		  ((< amount 0)
		   0)
		  ((null? prize-list)
		   0)
		  (else
		   (+ (count-limited-combos (cdr prize-list)
									(- amount
									   (car prize-list)))
			  (count-limited-combos (cdr prize-list)
									amount))))))

(count-limited-combos '(5 3 1 1) 6) ;; 2

;;; Exercise 7.20
(define number-of-limited-combinations
  (lambda (prize-list max-amount)
	(sum (map (lambda (n)
				(count-limited-combos prize-list n))
			  (integers-from-to 1 max-amount)))))

(number-of-limited-combinations '(5 3 1 1)
								4)
;; 6

;;; Exercise 7.21
;; United States of America coinage.
(define penny 1)
(define nickel 5)
(define dime 10)
(define quarter 25)

(define make-change
  (lambda (amount)
	(define iter
	  (lambda (amount coins remaining-partitions result)
		(define continue-partitioning
		  (lambda (result)
			(if (null? remaining-partitions)
				result
				(let ((new-partition (car remaining-partitions)))
				  (iter (car new-partition)
						(cdr new-partition)
						(cdr remaining-partitions)
						result)))))

		(cond ((or (null? coins)
				   (< amount 0))
			   (continue-partitioning result))
			  ((= amount 0)
			   (continue-partitioning (+ result 1)))
			  (else
			   (iter (- amount
						(car coins))
					 coins
					 (cons (cons amount       ; We basically store the
								 (cdr coins)) ; continuation on the heap.
						   remaining-partitions)
					 result)))))

	(iter amount
		  (list quarter dime nickel penny)
		  '()
		  0)))

(make-change 50) ;; 49

;;; Exercise 7.22
(define make-movie
  (lambda (title director year-made actors)
	(list title director year-made actors)))

(define movie-title car)
(define movie-director cadr)
(define movie-year-made caddr)
(define movie-actors cadddr)

(define our-movie-database
  (list (make-movie '(amarcord)
					'(federico fellini)
					1974
					'((magali noel)
					  (bruno zanin)
					  (pupella maggio)
					  (armando drancia)))
		(make-movie '(the big easy)
					'(jim mcbride)
					1987
					'((dennis quaid)
					  (ellen barkin)
					  (ned beatty)
					  (lisa jane persky)
					  (john goodman)
					  (charles ludlam)))
		(make-movie '(the godfather)
					'(francis ford coppola)
					1972
					'((marlon brando)
					  (al pacino)
					  (james caan)
					  (robert duvall)
					  (diane keaton)))
		(make-movie '(boyz in the hood)
					'(john singleton)
					1991
					'((cuba gooding jr.)
					  (ice cube)
					  (larry fishburne)
					  (tyra ferrell)
					  (morris chestnut)))))

;; A:
(define movies-made-in-year
  (lambda (movies year-made)
	(filter (lambda (movie)
			  (= (movie-year-made movie)
				 year-made))
			movies)))

(movies-made-in-year our-movie-database 1972)
;; (((the godfather)
;;   (francis ford coppola)
;;   1972
;;   ((marlon brando)
;;    (al pacino)
;;    (james caan)
;;    (robert duvall)
;;    (diane keaton))))

;; B:
(filter (lambda (movie)
		  (equal? (movie-director movie)
				  '(john singleton)))
		our-movie-database)
;; (((boyz n the hood)
;;   (john singleton)
;;   1991
;;   ((cuba gooding jr.)
;;    (ice cube)
;;    (larry fishburne)
;;    (tyra ferrell)
;;    (morris chestnut))))

;; C:
(define movies-directed-by
  (lambda (movies director)
	(filter (lambda (movie)
			  (equal? (movie-director movie)
					  director))
			movies)))

(movies-directed-by our-movie-database '(jim mcbride))
;; (((the big easy)
;;   (jim mcbride)
;;   1987
;;   ((dennis quaid)
;;    (ellen barkin)
;;    (ned beatty)
;;    (lisa jane persky)
;;    (john goodman)
;;    (charles ludlam))))

;; D:
(define movies-with-actor
  (lambda (movies actor)
	(filter (lambda (movie)
			  (member actor
					  (movie-actors movie)))
			movies)))

(movies-with-actor our-movie-database '(armando drancia))
;; (((amarcord)
;;   (federico fellini)
;;   1974
;;   ((magali noel)
;;    (bruno zanin)
;;    (pupella maggio)
;;    (armando drancia))))

;;; Exercise 7.23
(define movie-titles
  (lambda (movies)
	(map movie-title movies)))

(define titles-of-movies-satisfying
  (lambda (movies pred?)
	(movie-titles
	 (filter pred? movies))))

(titles-of-movies-satisfying our-movie-database
							 (lambda (movie)
							   (= (movie-year-made movie)
								  1974)))
;; ((amarcord))

;;; Exercise 7.24
(define movies-satisfying
  (lambda (movies pred? selector)
	(map selector
		 (filter pred? movies))))

(movies-satisfying our-movie-database
				   (lambda (movie)
					 (= (movie-year-made movie)
						1974))
				   movie-title)
;; ((amarcord))

;;; Exercise 7.25
(define make-pattern/action
  (lambda (pattern action)
	(cons pattern action)))

(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie)
					  title))
			movie-director)))
		(make-pattern/action
		 '(who acted in ...)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie)
					  title))
			movie-actors)))))

;; Other possible patterns could be '(which movies were directed by ...),
;; '(which movies were released in ...), ...

;;; Exercise 7.26
(define substitutions-in-to-match
  (lambda (pattern question)
	(define skip-to-wildcard
	  (lambda (p q)
		(if (or (null? p)
				(equal? (car p)
						'...))
			q
			(skip-to-wildcard (cdr p)
							  (cdr q)))))

	(list (skip-to-wildcard pattern question))))

(substitutions-in-to-match '(foo ...) '(foo bar baz))
;; ((bar baz))

;;; Exercise 7.27
(define pattern car)
(define action cdr)

(define matches?
  (lambda (pattern question)
	(cond ((null? pattern)
		   (null? question))
		  ((null? question)
		   #f)
		  ((equal? (car pattern)
				   '...)
		   #t)
		  ((equal? (car pattern)
				   (car question))
		   (matches? (cdr pattern)
					 (cdr question)))
		  (else
		   #f))))

(define answer-by-pattern
  (lambda (query p/a-list)
	(cond ((null? p/a-list)
		   (display '(i do not understand)))
		  ((matches? (pattern (car p/a-list))
					 query)
		   (let ((subs (substitutions-in-to-match
						(pattern (car p/a-list))
						query)))
			 (let ((result (apply (action (car p/a-list))
								  subs)))
			   (if (null? result)
				   (display '(i do not know))
				   (display result)))))
		  (else
		   (answer-by-pattern query
							  (cdr p/a-list))))))

(define exit?
  (lambda (query)
	(member query
			'((bye)
			  (quit)
			  (exit)
			  (so long)
			  (farewell)))))

(define query-loop
  (lambda ()
	(newline)
	(newline)
	(let ((query (read)))
	  (cond ((exit? query)
			 (display '(see you later)))
			(else
			 (answer-by-pattern query movie-p/a-list)
			 (query-loop))))))

;;; Exercise 7.28
;; If there are no elements in the list it should return the empty list.
;; If there are two or more elements it should just return the list.
(define the-only-element-in
  (lambda (lst)
	(cond ((null? lst)
		   '())
		  ((null? (cdr lst))
		   (car lst))
		  (else
		   lst))))

(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie)
					  title))
			movie-director)))
		(make-pattern/action
		 '(who acted in ...)
		 (lambda (title)
		   (the-only-element-in
			(movies-satisfying
			 our-movie-database
			 (lambda (movie)
			   (equal? (movie-title movie)
					   title))
			 movie-actors))))))

;;; Exercise 7.29
(define matches?
  (lambda (pattern question)
	(cond ((null? pattern)
		   (null? question))
		  ((null? question)
		   #f)
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (matches? (cdr pattern)
						 (cdr question))
			   #f))
		  ((equal? (car pattern)
				   '...)
		   #t)
		  ((or (equal? (car pattern)
					   '_)
			   (equal? (car pattern)
					   (car question)))
		   (matches? (cdr pattern)
					 (cdr question)))
		  (else
		   #f))))

;;; Exercise 7.30
(define substitutions-in-to-match
  (lambda (pattern question)
	(define gather-substitutions
	  (lambda (p q)
		(cond ((null? p)
			   '())
			  ((equal? (car p)
					   '...)
			   q)
			  ((or (equal? (car p)
						   '_)
				   (list? (car p)))
			   (cons (car q)
					 (gather-substitutions (cdr p)
										   (cdr q))))
			  (else
			   (gather-substitutions (cdr p)
									 (cdr q))))))

	(gather-substitutions pattern question)))

(substitutions-in-to-match '(through _ and _)
						   '(through rain and sleet))
;; (rain sleet)
(substitutions-in-to-match '(through _ and ...)
						   '(through thick and thin))
;; (thick thin)
(substitutions-in-to-match '(_ (versus and meets) ...)
						   '(einstein meets queen))
;; (einstein meets queen)

;;; Exercise 7.31
;; (lambda (noun verb relation year)
;;   (movies-satisfying
;;    our-movie-database
;;    (lambda (movie)
;;      ((cond ((equal? relation 'in)
;;              =)
;; 			((equal? relation 'before)
;;              <)
;; 			((equal? relation 'after)
;;              >)
;; 			((equal? relation 'since)
;;              >=)
;; 			(else
;; 			 (error 'match-year
;; 					"Unknown relation."
;; 					relation)))
;; 	  (movie-year-made movie)
;; 	  year))))

;;; Exercise 7.32
(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (the-only-element-in
			(movies-satisfying
			 our-movie-database
			 (lambda (movie)
			   (equal? (movie-title movie)
					   title))
			 movie-director))))
		(make-pattern/action
		 '(what (movie movies) (was were) made (in before after since) _)
		 (lambda (noun verb relation year)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  ((cond ((equal? relation 'in)
					  =)
					 ((equal? relation 'before)
					  <)
					 ((equal? relation 'after)
					  >)
					 ((equal? relation 'since)
					  >=)
					 (else
					  (error 'match-year
							 "Unknown relation."
							 relation)))
			   (movie-year-made movie)
			   year))
			movie-title)))
		(make-pattern/action
		 '(what (movie movies) (was were) made between _ and _)
		 (lambda (noun verb year1 year2)
		   (let ((since (min year1 year2))
				 (to (max year1 year2)))
			 (movies-satisfying
			  our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie)
						 since)
					 (<= (movie-year-made movie)
						 to)))
			  movie-title))))))

;;; Exercise 7.33
(define equal-title-sans-articles?
  (lambda (title-a title-b)
	(define no-article?
	  (lambda (word)
		(not (or (equal? word 'the)
				 (equal? word 'a) ;; Maybe silly for things like Project A.
				 (equal? word 'an)))))

	(equal? (filter no-article? title-a)
			(filter no-article? title-b))))

;; I would use this predicate in the pattern/actions list.

;;; Exercise 7.34
(define matches-sans-ellipsis?
  (lambda (pattern question)
	(cond ((null? pattern)
		   #t)
		  ((or (null? question)
			   (equal? (car pattern)
					   '...))
		   #f)
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (matches-sans-ellipsis? (cdr pattern)
									   (cdr question))
			   #f))
		  ((or (equal? (car pattern)
					   '_)
			   (equal? (car pattern)
					   (car question)))
		   (matches-sans-ellipsis? (cdr pattern)
								   (cdr question)))
		  (else
		   #f))))

(define matches?
  (lambda (pattern question)
	(cond ((null? pattern)
		   (null? question))
		  ((null? question)
		   #f)
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (matches? (cdr pattern)
						 (cdr question))
			   #f))
		  ((equal? (car pattern)
				   '...)
		   (matches-sans-ellipsis? (reverse (cdr pattern))
								   (reverse (cdr question))))
		  ((or (equal? (car pattern)
					   '_)
			   (equal? (car pattern)
					   (car question)))
		   (matches? (cdr pattern)
					 (cdr question)))
		  (else
		   #f))))

(define substitutions-in-to-match
  (lambda (pattern question)
	(define gather-substitutions
	  (lambda (p q)
		(cond ((null? p)
			   (if (null? q)
				   '()
				   (list q)))
			  ((equal? (car p)
					   '...)
			   (reverse
				(map (lambda (sub)
					   (if (list? sub)
						   (reverse sub)))
					 (gather-substitutions (reverse (cdr p))
										   (reverse q)))))
			  ((or (equal? (car p)
						   '_)
				   (list? (car p)))
			   (cons (car q)
					 (gather-substitutions (cdr p)
										   (cdr q))))
			  (else
			   (gather-substitutions (cdr p)
									 (cdr q))))))

	(gather-substitutions pattern question)))

(matches? '(what movies did ... act in)
		  '(what movies did buster keaton act in)) ;; #t
(matches? '(what movies were made between ... and ...)
		  '(what movies were made between 1037 and 1067)) ;; #f
(substitutions-in-to-match '(what movies did ... act in)
						   '(what movies did buster keaton act in))
;; ((buster keaton))

(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (the-only-element-in
			(movies-satisfying
			 our-movie-database
			 (lambda (movie)
			   (equal? (movie-title movie)
					   title))
			 movie-director))))
		(make-pattern/action
		 '(what (movie movies) (was were) made (in before after since) _)
		 (lambda (noun verb relation year)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  ((cond ((equal? relation 'in)
					  =)
					 ((equal? relation 'before)
					  <)
					 ((equal? relation 'after)
					  >)
					 ((equal? relation 'since)
					  >=)
					 (else
					  (error 'match-year
							 "Unknown relation."
							 relation)))
			   (movie-year-made movie)
			   year))
			movie-title)))
		(make-pattern/action
		 '(what (movie movies) (was were) made between _ and _)
		 (lambda (noun verb year1 year2)
		   (let ((since (min year1 year2))
				 (to (max year1 year2)))
			 (movies-satisfying
			  our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie)
						 since)
					 (<= (movie-year-made movie)
						 to)))
			  movie-title))))
		(make-pattern/action
		 '(when was ... made)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie)
					  title))
			movie-year-made)))))

;;; Exercise 7.35
(define gather-...-substitutions
  (lambda (pattern question substitutions)
	(define all-valid-permutations
	  (lambda (question trail permutations)
		(if (null? question)
			(if (null? (cadr permutations))
				(car permutations)
				permutations)
			(let ((new-trail (cons (car question)
								   trail)))
			  (if (equal? (car question)
						  (car pattern))
				  (let ((continuations (gather-substitutions pattern
															 question
															 '())))
					(all-valid-permutations (cdr question)
											new-trail
											(if continuations
												(list
												 (cons trail
													   (car permutations))
												 (if (null? continuations)
													 (cadr permutations)
													 (cons continuations
														   (cadr permutations))))
												permutations)))
				  (all-valid-permutations (cdr question)
										  new-trail
										  permutations))))))

	(if (null? pattern)
		question
		(all-valid-permutations (cdr question)
								(list (car question))
								'(() ())))))

(define gather-substitutions
  (lambda (pattern question substitutions)
	(define recur
	  (lambda (substitutions)
		(gather-substitutions (cdr pattern)
							  (cdr question)
							  substitutions)))

	(define continue
	  (lambda ()
		(recur substitutions)))

	(define take-and-continue
	  (lambda ()
		(recur (cons (car question)
					 substitutions))))

	(cond ((null? pattern)
		   (if (null? question)
			   (reverse substitutions)
			   #f))
		  ((null? question)
		   #f)
		  ((equal? (car pattern)
				   '...)
		   (let ((...-subs (gather-...-substitutions (cdr pattern)
													 question
													 '())))
			 (cond ((null? ...-subs)
					#f)
				   ((null? substitutions)
					...-subs)
				   (else
					(append (reverse substitutions)
							...-subs)))))
		  ((equal? (car pattern)
				   '_)
		   (take-and-continue))
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (take-and-continue)
			   #f))
		  ((equal? (car pattern)
				   (car question))
		   (continue))
		  (else
		   #f))))

(define substitutions-in-to-match
  (lambda (pattern question)
	(let ((subs (gather-substitutions pattern question '())))
	  (if subs subs '()))))

(substitutions-in-to-match '(who directed ...)
						   '(who directed the godfather))
;; (the godfather)
(substitutions-in-to-match '(do you have ... in ...)
						   '(do you have boyz in the hood in the store))
;; (((hood the in boyz) (boyz)) ((the store) ((the hood in the store))))
(substitutions-in-to-match '(what movies did ... act in)
						   '(what movies did buster keaton act in))
;; ((keaton buster))

(define answer-by-pattern
  (lambda (query p/a-list)
	(if (null? p/a-list)
		(display '(i do not understand))
		(let ((subs (substitutions-in-to-match
						(pattern (car p/a-list))
						query)))
		  (if (null? subs)
			  (answer-by-pattern query
								 (cdr p/a-list))
			  (let ((result (apply (action (car p/a-list))
								   subs)))
				(if (null? result)
					(display '(i do not know))
					(display result))))))))

;;; Exercise 7.36
(define movie-p/a-list
  (list (make-pattern/action
		 '(who is the director of ...)
		 (lambda (title)
		   (the-only-element-in
			(movies-satisfying
			 our-movie-database
			 (lambda (movie)
			   (equal? (movie-title movie)
					   title))
			 movie-director))))
		(make-pattern/action
		 '(what (movie movies) (was were) made (in before after since) _)
		 (lambda (noun verb relation year)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  ((cond ((equal? relation 'in)
					  =)
					 ((equal? relation 'before)
					  <)
					 ((equal? relation 'after)
					  >)
					 ((equal? relation 'since)
					  >=)
					 (else
					  (error 'match-year
							 "Unknown relation."
							 relation)))
			   (movie-year-made movie)
			   year))
			movie-title)))
		(make-pattern/action
		 '(what (movie movies) (was were) made between _ and _)
		 (lambda (noun verb year1 year2)
		   (let ((since (min year1 year2))
				 (to (max year1 year2)))
			 (movies-satisfying
			  our-movie-database
			  (lambda (movie)
				(and (>= (movie-year-made movie)
						 since)
					 (<= (movie-year-made movie)
						 to)))
			  movie-title))))
		(make-pattern/action
		 '(when was ... made)
		 (lambda (title)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-title movie)
					  title))
			movie-year-made)))
		(make-pattern/action
		 '(who played in movies directed by ...)
		 (lambda (director)
		   (movies-satisfying
			our-movie-database
			(lambda (movie)
			  (equal? (movie-director movie)
					  director))
			movie-actors)))
		(make-pattern/action
		 '(who played (together alongside) with ...)
		 (lambda (actor)
		   our-movie-database
		   (lambda (movie)
			 (member actor
					 (movie-actors movie)))
		   movie-actors))))

;;; Exercise 7.37
(define gather-substitutions
  (lambda (pattern question substitutions)
	(define recur
	  (lambda (substitutions)
		(gather-substitutions (cdr pattern)
							  (cdr question)
							  substitutions)))

	(define continue
	  (lambda ()
		(recur substitutions)))

	(define take-and-continue
	  (lambda ()
		(recur (cons (car question)
					 substitutions))))

	(cond ((null? pattern)
		   (if (null? question)
			   (reverse substitutions)
			   #f))
		  ((null? question)
		   #f)
		  ((equal? (car pattern)
				   '...)
		   (let ((...-subs (gather-...-substitutions (cdr pattern)
													 question
													 '())))
			 (cond ((null? ...-subs)
					#f)
				   ((null? substitutions)
					...-subs)
				   (else
					(append (reverse substitutions)
							...-subs)))))
		  ((equal? (car pattern)
				   'number?)
		   (if (number? (car question))
			   (take-and-continue)
			   '()))
		  ((equal? (car pattern)
				   '_)
		   (take-and-continue))
		  ((list? (car pattern))
		   (if (member (car question)
					   (car pattern))
			   (take-and-continue)
			   #f))
		  ((equal? (car pattern)
				   (car question))
		   (continue))
		  (else
		   #f))))

(substitutions-in-to-match '(greater than number?)
						   '(greater than 37))
;; (37)

;;; Exercise 7.38
(define match-alternatives
  (lambda (alternatives question)
	(define gather-alternative
	  (lambda (alternative question result)
		(cond ((null? alternative)
			   (cons (reverse result)
					 question))
			  ((equal? (car alternative)
					   (car question))
			   (gather-alternative (cdr alternative)
								   (cdr question)
								   (cons (car question)
										 result)))
			  (else
			   '()))))

	(cond ((null? alternatives)
		   '())
		  ((equal? (car alternatives)
				   (car question))
		   (cons (list (car question))
				 (cdr question)))
		  ((list? (car alternatives))
		   (let ((res (gather-alternative (car alternatives)
										  question
										  '())))
			 (if (null? res)
				 (match-alternatives (cdr alternatives)
									 question)
				 res)))
		  (else
		   (match-alternatives (cdr alternatives)
							   question)))))

(define gather-substitutions
  (lambda (pattern question substitutions)
	(define recur
	  (lambda (substitutions)
		(gather-substitutions (cdr pattern)
							  (cdr question)
							  substitutions)))

	(define continue
	  (lambda ()
		(recur substitutions)))

	(define take-and-continue
	  (lambda ()
		(recur (cons (car question)
					 substitutions))))

	(cond ((null? pattern)
		   (if (null? question)
			   (reverse substitutions)
			   #f))
		  ((null? question)
		   #f)
		  ((equal? (car pattern)
				   '...)
		   (let ((...-subs (gather-...-substitutions (cdr pattern)
													 question
													 '())))
			 (cond ((null? ...-subs)
					#f)
				   ((null? substitutions)
					...-subs)
				   (else
					(append (reverse substitutions)
							...-subs)))))
		  ((equal? (car pattern)
				   'number?)
		   (if (number? (car question))
			   (take-and-continue)
			   '()))
		  ((equal? (car pattern)
				   '_)
		   (take-and-continue))
		  ((list? (car pattern))
		   (let ((the-match (match-alternatives (car pattern)
												question)))
			 (if (null? the-match)
				 '()
				 (gather-substitutions (cdr pattern)
									   (cdr the-match)
									   (cons (car the-match)
											 substitutions)))))
		  ((equal? (car pattern)
				   (car question))
		   (continue))
		  (else
		   #f))))

(substitutions-in-to-match '(the ((pig was) (pigs were)) _)
						   '(the pig was old))
;; ((pig was) old)

;;; Exercise 7.39
;; Base case:
;; (sevens 0) returns an empty list, due to the evaluation rules of if.

;; Induction hypothesis:
;; (sevens k) returns a list of length k, for all k in the range 0 <= k < n.

;; Inductive step:
;; For any positive integer n, (sevens n) returns (cons 7 (sevens (- n 1))).
;; Per our induction hypothesis, this means we cons 7 onto a list of length n -
;; 1. Therefore we create a list of length (n - 1) + 1 = n.

;; Conclusion:
;; Therefore we can prove through mathematical induction that (sevens n) will
;; return a list of length n for any nonnegative integer n.

;;; Exercise 7.40
(define function-sum
  (lambda (list-of-functions)
	(lambda (x)
	  (define sum
		(lambda (functions result)
		  (if (null? functions)
			  result
			  (sum (cdr functions)
				   (+ ((car functions) x)
					  result)))))

	  (sum list-of-functions 0))))

((function-sum (list square cube)) 2) ;; 12

;;; Exercise 7.41
(define square-sum
  (lambda (lst)
	(define sum
	  (lambda (lst result)
		(if (null? lst)
			result
			(sum (cdr lst)
				 (+ (square (car lst))
					result)))))

	(sum lst 0)))

;;; Exercise 7.42
(define apply-all
  (lambda (procs n)
	(map (lambda (proc)
		   (proc n))
		 procs)))

(apply-all (list sqrt square cube) 4)
;; (2 16 64)

;;; Exercise 7.43
;; Base case:
;; (seventeens 0) returns a list of length 0 = 2 * 0. Therefore our theorem
;; holds.

;; Induction hypothesis:
;; (seventeens k) returns a list of length 2 * k for all k in the range 0 <= k
;; <= n.

;; Inductive step:
;; (seventeens n) returns (cons 17 (cons 17 (seventeens (- n 1)))). Per our
;; induction hypothesis, (seventeens (- n 1)) returns a list of length (n - 1)
;; * 2 = (2 * n) - 2. On that we cons 17 twice, increasing the length of the
;; list by 2. (2 * n) - 2 + 2 = 2 * n.

;; Conclusion:
;; Therefore we can prove through mathematical induction that for any
;; nonnegative integer n, (seventeens n) returns a list of length 2 * n.

;;; Exercise 7.44
;; A: (length lst) does n cdrs where n is the amount of elements in lst.

;; B: last will make (min 1 n) calls to length where n is the amount of
;; elements in lst.

;; C: Last will make O(n ^ 2) calls to cdr through the combination of direct
;; calls and calls through length.

;; D: total number of cdrs = (min 1 (n * n)).

;;; Exercise 7.45
(define make-couple
  (lambda (x y)
	(cons x y)))

(define smaller
  (lambda (couple)
	(min (car couple)
		 (cdr couple))))

(define larger
  (lambda (couple)
	(max (car couple)
		 (cdr couple))))

(define make-couple
  (lambda (x y)
	(cons (min x y)
		  (max x y))))

(define smaller
  (lambda (couple)
	(car couple)))

(define larger
  (lambda (couple)
	(cdr couple)))

;;; Exercise 7.46
(define make-list-scaler
  (lambda (scale)
	(lambda (lst)
	  (map (lambda (x)
			 (* x scale))
		   lst))))

(define scale-by-5
  (make-list-scaler 5))

(scale-by-5 '(1 2 3 4))
;; (5 10 15 20)

;;; Exercise 7.47
(define map-2
  (lambda (proc lst1 lst2)
	(define iter
	  (lambda (l1 l2 res)
		(if (null? l1)
			res
			(iter (cdr l1)
				  (cdr l2)
				  (cons (proc (car l1)
							  (car l2))
						res)))))

	(reverse (iter lst1 lst2 '()))))

(map-2 + '(1 2 3) '(2 0 -5))
;; (3 2 -2)
(map-2 * '(1 2 3) '(2 0 -5))
;; (2 0 -15)

;;; Exercise 7.48
;; (sub1-each '(5 4 3))
;; (help '(5 4 3) '())
;; (if (null? '(5 4 3))
;;     (reverse '())
;;     (help (cdr '(5 4 3))
;;           (cons (- (car '(5 4 3)) 1) '())))
;; (help '(4 3) '(4))
;; (if (null? '(4 3))
;;     (reverse '(4))
;;     (help (cdr '(4 3))
;;           (cons (- (car '(4 3)) 1) '(4))))
;; (help '(3) '(3 4))
;; (if (null? '(3))
;;     (reverse '(3 4))
;;     (help (cdr '(3))
;;           (cons (- (car '(3)) 1) '(3 4))))
;; (help '() '(2 3 4))
;; (if (null? '())
;;     (reverse '(2 3 4))
;;     (help (cdr '())
;;           (cons (- (car '()) 1) '(2 3 4))))
;; '(4 3 2)
;; This is an iterative process.

;;; Exercise 7.49
(define all-are
  (lambda (pred?)
	(lambda (lst)
	  (null? (filter (lambda (x)
					   (not (pred? x)))
					 lst)))))

((all-are positive?) '(1 2 3 4)) ;; #t
((all-are even?) '(2 4 5 6 8)) ;; #f

;;; Exercise 7.50
;; A:
;; repeat generates a recursive process as it leaves the cons step for later,
;; after the smaller work has been done.

;; B:
(define repeat
  (lambda (num times)
	(define iter
	  (lambda (times res)
		(if (= times 0)
			res
			(iter (- times 1)
				  (cons num res)))))

	(iter times '())))

(repeat 17 5)
;; (17 17 17 17 17)

;;; Exercise 7.51
;; A:
;; (expand '(3 ho merry-xmas))
;; (cons 'ho
;;       (expand (cons 2 '(ho merry-xmas))))
;; (cons 'ho
;;       (cons 'ho
;;             (expand (cons 1 '(ho merry-xmas)))))
;; (cons 'ho
;;       (cons 'ho
;;             (cons 'ho (expand '(merry-xmas)))))
;; (cons 'ho
;;       (cons 'ho
;;             (cons 'ho '(merry-xmas))))
;; (cons 'ho
;;       (cons 'ho '(ho merry-xmas)))
;; (cons 'ho '(ho ho merry-xmas))
;; '(ho ho ho merry-xmas)

;; B:
;; Recursive, as it holds off on the consing until after the cdr has been
;; constructed.

;;; Exercise 7.52
(define make-list-combiner
  (lambda (proc)
	(lambda (lst1 lst2)
	  (map proc lst1 lst2))))

(define list+
  (make-list-combiner +))

(define list*
  (make-list-combiner *))

(list+ '(1 2 3) '(2 4 6))
;; (3 6 9)
(list* '(1 2 3) '(2 4 6))
;; (2 8 18)
