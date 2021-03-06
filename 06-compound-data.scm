(import (rnrs (6))
		(srfi srfi-27))

(define evenly-divisble-by?
  (lambda (x y)
	(= (remainder x y)
	   0)))

(define random
  (lambda (n)
	(random-integer n)))

(define square
  (lambda (x)
	(* x x)))

;;; Exercise 6.1
;; The advantage of having three operations, of which one is
;; remove-coins-from-pile with a limit is that it is easier to change the limit
;; of coins that can be taken from a pile. The disadvantage is that we have to
;; clearly express when the limit is being exceeded and we may have to take
;; care to handle that situation if it comes up.
;; The advantage to having five operations, of which three happen to be the
;; remove-one, remove-two, and remove-three operations is that it is no longer
;; possible for an illegal action to occur. The downside is that we need to add
;; additional operations if we want to increase the limit or delete them to
;; decrease the limit. There may also be a disadvantage of having duplicate
;; code which would need to be updated if we change other aspects of the game.

;;; Exercise 6.2
;; Our gamekeeper would just say "oh, there aren't that many coins on the
;; pile," and remove the remainder of the pile. I would add an "is-valid?"
;; parameter to prompt so it can recurse if the player gives an invalid
;; request. This way we can also make sure the player doesn't ask to remove
;; coins from a non-existant or empty pile. That's why I think it's better to
;; do the error checking in prompt. And the is-valid? parameter would of course
;; be a procedure that we pass the answer to and which returns #t or #f.

;;; Exercise 6.3
;; To change who wins we can simply change which player we check for in
;; announce-winner.

;;; Exercise 6.4
(define make-game-state
  ;; assumes no more than 99 coins per pile
  (lambda (n m)
	(+ (* n 100)
	   m)))

(define size-of-pile
  (lambda (game-state pile-number)
	(if (= pile-number 1)
		(quotient game-state 100)
		(remainder game-state 100))))

;;; Exercise 6.5
(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
	(let ((pile-size (size-of-pile game-state pile-number)))
	  (if (= pile-number 1)
		  (make-game-state (- pile-size
							  (min num-coins pile-size))
						   (size-of-pile game-state 2))
		  (make-game-state (size-of-pile game-state 1)
						   (- pile-size
							  (min num-coins pile-size)))))))

;;; Exercise 6.6
;; Besides querying for a parameter that doesn't cause an error or falling back
;; to an assumed logical default, if an error were to occur we could also end
;; the game, skip the current turn, or we could in theory ignore the error and
;; just keep going with an illegal state. That last option is thoroughly
;; undesirable though.

;;; Exercise 6.7
(define exponent-of-in
  (lambda (n m)
	(define divide-while-divisible
	  (lambda (numerator divisions)
		(if (evenly-divisble-by? numerator n)
			(divide-while-divisible (/ numerator n)
									(+ divisions 1))
			divisions)))

	(divide-while-divisible m 0)))

;;; Exercise 6.8
;; (size-of-pile (make-game-state n m) 1)
;; = ((lambda (x) (if (odd? x) n m)) 1)
;; = (if (odd? 1) n m)
;; = n
;; (size-of-pile (make-game-state n m) 2)
;; = ((lambda (x) (if (odd? x) n m)) 2)
;; = (if (odd? 2) n m)
;; = m

;;; Exercise 6.9
;; Writing numbers after each other:
;; abc = 100 * a + 10 * b + c
;; or in RPN: 100 a * 10 b * + c +
;; Consing together:
;; (car (cons a (cons b c))) = a
;; (cdr (cons a (cons b c))) = (cons b c)
;; (car (cdr (cons a (cons b c)))) = b
;; (cdr (cdr (cons a (cons b c)))) = c

;;; Exercise 6.10
;; (size-of-pile (make-game-state n m k) 1)
;; = (car (cdr (cons k (cons n m))))
;; = (car (cons n m))
;; = n
;; (size-of-pile (make-game-state n m k) 2)
;; = (cdr (cdr (cons k (cons n m))))
;; = (cdr (cons n m))
;; = m
;; (size-of-pile (make-game-state n m k) 3)
;; = (car (cons k (cons n m)))
;; = k

;;; Exercise 6.11
(define make-game-state
  (lambda (n m k)
	(cons k (cons n m))))

(define size-of-pile
  (lambda (game-state pile-number)
	(cond ((= pile-number 3)
		   (car game-state))
		  ((= pile-number 1)
		   (car (cdr game-state)))
		  (else
		   (cdr (cdr game-state))))))

(define remove-coins-from-pile
  (lambda (game-state num-coins pile-number)
	(let ((size-1 (size-of-pile game-state 1))
		  (size-2 (size-of-pile game-state 2))
		  (size-3 (size-of-pile game-state 3)))
	  (cond ((= pile-number 1)
			 (make-game-state (- size-1
								 (min size-1 num-coins))
							  size-2
							  size-3))
			((= pile-number 2)
			 (make-game-state size-1
							  (- size-2
								 (min size-2 num-coins))
							  size-3))
			(else
			 (make-game-state size-1
							  size-2
							  (- size-3
								 (min size-3 num-coins))))))))

(define total-size
  (lambda (game-state)
	(+ (size-of-pile game-state 1)
	   (size-of-pile game-state 2)
	   (size-of-pile game-state 3))))

(define display-game-state
  (lambda (game-state)
	(newline)
	(newline)
	(display "  Pile 1: ")
	(display (size-of-pile game-state 1))
	(newline)
	(display "  Pile 2: ")
	(display (size-of-pile game-state 2))
	(newline)
	(display "  Pile 3: ")
	(display (size-of-pile game-state 3))
	(newline)
	(newline)))

;;; Exercise 6.12
;; When pile 1 has 0 coins left the computer will keep reducing pile 2, going
;; into the negatives if it reaches 0 coins before pile 3 reaches 0.

(define computer-move
  (lambda (game-state)
	(let ((pile (cond ((> (size-of-pile game-state 1)
						  0)
					   1)
					  ((> (size-of-pile game-state 2)
						  0)
					   2)
					  (else
					   3))))
	  (display "I take 1 coin from pile ")
	  (display pile)
	  (newline)
	  (remove-coins-from-pile game-state 1 pile))))

;;; Exercise 6.13
;; A: (make-move-instruction num-coins pile-number) will return a move
;; instruction inst. Then we can access num-coins and pile-number with the
;; procedures (move-instruction-num-coins inst) and
;; (move-instruction-pile-number).
;; (move-instruction-num-coins (make-move-instruction n m)) = n.
;; (move-instruction-pile-number (make-move-instruction n m)) = m.

;; B:
(define make-move-instruction
  (lambda (num-coins pile-number)
	(cons num-coins pile-number)))

(define move-instruction-num-coins
  (lambda (inst)
	(car inst)))

(define move-instruction-pile-number
  (lambda (inst)
	(cdr inst)))

;; C:
(define make-game-state
  (lambda (n m)
	(cons n m)))

(define size-of-pile
  (lambda (game-state pile-number)
	(if (= pile-number 1)
		(car game-state)
		(cdr game-state))))

(define next-game-state
  (lambda (game-state inst)
	(let ((pile-number (move-instruction-pile-number inst))
		  (pile-1 (size-of-pile game-state 1))
		  (pile-2 (size-of-pile game-state 2)))
	  (if (= pile-number 1)
		  (make-game-state (- pile-1
							  (min (move-instruction-num-coins inst)
								   pile-1))
						   pile-2)
		  (make-game-state pile-1
						   (- pile-2
							  (min (move-instruction-num-coins inst)
								   pile-2)))))))

(define display-game-state
  (lambda (game-state)
	(newline)
	(newline)
	(display "  Pile 1: ")
	(display (size-of-pile game-state 1))
	(newline)
	(display "  Pile 2: ")
	(display (size-of-pile game-state 2))
	(newline)
	(newline)))

(define total-size
  (lambda (game-state)
	(+ (size-of-pile game-state 1)
	   (size-of-pile game-state 2))))

(define over?
  (lambda (game-state)
	(= (total-size game-state)
	   0)))

(define announce-winner
  (lambda (player)
	(if (equal? player 'human)
		(display "You lose. Better luck next time!")
		(display "You win. Conglaturations!"))
	(newline)))

(define prompt
  (lambda (prompt-string legal?)
	(newline)
	(display prompt-string)
	(newline)
	(let ((response (read)))
	  (if (legal? response)
		  response
		  (prompt prompt-string legal?)))))

(define human-move
  (lambda (game-state)
	(let ((pile-number
		   (prompt "Which pile would you like to take coins from? (1 or 2)"
				   (lambda (n)
					 (or (= n 1)
						 (= n 2))))))
	  (let ((num-coins
			 (prompt (string-append
					  "How many coins would you like to take? (Max "
					  (number->string (size-of-pile game-state pile-number))
					  ")")
					 (lambda (n)
					   (and (>= n 0)
							(<= n
								(size-of-pile game-state pile-number)))))))
		(next-game-state game-state
						 (make-move-instruction num-coins pile-number))))))

(define computer-move
  (lambda (game-state)
	(let ((pile-number (if (= (size-of-pile game-state 1)
							  0)
						   2
						   1)))
	  (display "I'll take 1 coin from pile ")
	  (display pile-number)
	  (display ".")
	  (newline)
	  (next-game-state game-state
					   (make-move-instruction 1 pile-number)))))

(define play-with-turns
  (lambda (game-state player)
	(display-game-state game-state)
	(cond ((over? game-state)
		   (announce-winner player))
		  ((equal? player 'human)
		   (play-with-turns (human-move game-state)
							'computer))
		  ((equal? player 'computer)
		   (play-with-turns (computer-move game-state)
							'human))
		  (else
		   (error 'play-with-turns
				  "Player was neither human nor computer"
				  player)))))

;;; Exercise 6.14
(define computer-move
  (lambda (game-state strategy)
	(let ((move (strategy game-state)))
	  (display "I'll take ")
	  (display (move-instruction-num-coins move))
	  (display " from pile ")
	  (display (move-instruction-pile-number move))
	  (display ".")
	  (newline)
	  (next-game-state game-state move))))

(define play-with-turns
  (lambda (game-state player strategy)
	(define play
	  (lambda (game-state player)
		(display-game-state game-state)
		(cond ((over? game-state)
			   (announce-winner player))
			  ((equal? player 'human)
			   (play (human-move game-state)
					 'computer))
			  ((equal? player 'computer)
			   (play (computer-move game-state strategy)
					 'human))
			  (else
			   (error 'play-with-turns
					  "Player was neither human nor computer"
					  player)))))

	(play game-state player)))

;;; Exercise 6.15
(define take-all-of-first-nonempty
  (lambda (game-state)
	(let ((size-1 (size-of-pile game-state 1)))
	  (if (= size-1 0)
		  (make-move-instruction (size-of-pile game-state 2)
								 2)
		  (make-move-instruction size-1 1)))))

;;; Exercise 6.16
(define take-one-from-random-pile
  (lambda (game-state)
	(let ((pile-number (+ (random 2)
						  1)))
	  (if (= (size-of-pile game-state pile-number)
			 0)
		  (make-move-instruction 1
								 (- 3 pile-number))
		  (make-move-instruction 1
								 pile-number)))))

;;; Exercise 6.17
(define take-random-from-random-pile
  (lambda (game-state)
	(let ((size-1 (size-of-pile game-state 1))
		  (size-2 (size-of-pile game-state 2))
		  (random-pile (+ (random 2)
						  1)))
	  (cond ((= size-1 0)
			 (make-move-instruction 2
									(+ (random size-2)
									   1)))
			((= size-2 0)
			 (make-move-instruction 1
									(+ (random size-1)
									   1)))
			(else
			 (make-move-instruction random-pile
									(+ (random (if (= random-pile 1)
												   size-1
												   size-2))
									   1)))))))

;;; Exercise 6.18
(define simple-strategy
  (lambda (game-state)
	(make-move-instruction 1
						   (if (= (size-of-pile game-state 1)
								  0)
							   2
							   1))))

(define chocolate-square-strategy
  (lambda (game-state)
	(let ((size-1 (size-of-pile game-state 1))
		  (size-2 (size-of-pile game-state 2)))
	  (cond ((> size-1 size-2)
			 (make-move-instruction (- size-1 size-2)
									1))
			((> size-2 size-1)
			 (make-move-instruction (- size-2 size-1)
									2))
			(else
			 ;; Technically we should resign, but that isn't implemented.
			 (simple-strategy game-state))))))

;;; Exercise 6.19
(define random-mix-of
  (lambda (strategy-a strategy-b)
	(lambda (game-state)
	  ((if (= (random 2)
			  0)
		   strategy-a
		   strategy-b)
	   game-state))))

;;; Exercise 6.20
(define computer-play-with-turns
  (lambda (game-state strategy-a strategy-b)
	(define announce-winner
	  (lambda (victor)
		(display "Player ")
		(display victor)
		(display " wins.")
		(newline)))

	(define play
	  (lambda (game-state player opponent)
		(display-game-state game-state)
		(if (over? game-state)
			(announce-winner opponent)
			(play (computer-move game-state
								 (if (equal? player 'a)
									 strategy-a
									 strategy-b))
				  opponent
				  player))))

	(play game-state 'a 'b)))

;;; Exercise 6.21
;; (computer-play-with-turns gs ask-the-human simple-strategy) would be
;; equivalent to our original implementation.
;; (computer-play-with-turns gs sa ask-the-human) would be a regular game where
;; the computer plays first.
;; (computer-play-with-turns gs ask-the-human ask-the-human) would be a
;; hot-seat two-player game.

;;; Exercise 6.22
;; A:
;; (define mid-point
;;   (lambda (interval)
;;     (/ (+ (upper-endpoint interval)
;;           (lower-endpoint interval))
;;        2)))

;; B:
;; (define right-half
;;   (lambda (interval)
;;     (make-interval (mid-point interval)
;;                    (upper-endpoint interval))))

;;; Exercise 6.23
;; A:
(define sum-3d-vectors
  (lambda (a b)
    (make-3d-vector (+ (x-coord a)
                       (x-coord b))
                    (+ (y-coord a)
                       (y-coord b))
                    (+ (z-coord a)
                       (z-coord b)))))

(define 3d-vector-dot-product
  (lambda (a b)
    (+ (* (x-coord a)
          (x-coord b))
       (* (y-coord a)
          (y-coord b))
       (* (z-coord a)
          (z-coord b)))))

(define scale-3d-vector
  (lambda (vec scale)
    (make-3d-vector (* (x-coord vec)
                       scale)
                    (* (y-coord vec)
                       scale)
                    (* (z-coord vec)
                       scale))))

;; B:
(define make-3d-vector
  (lambda (x y z)
	(cons x (cons y z))))

(define x-coord
  (lambda (vec)
	(car vec)))

(define y-coord
  (lambda (vec)
	(car (cdr vec))))

(define z-coord
  (lambda (vec)
	(cdr (cdr vec))))

;;; Exercise 6.24
(define make-schedule-item
  (lambda (room course time)
	(lambda (x)
	  (cond ((equal? x 'room)
			 room)
			((equal? x 'course)
			 course)
			((equal? x time)
			 'time)
			(else
			 (error 'make-schedule-item
					"Selector is room, course, nor time."
					x))))))

(define room
  (lambda (schedule-item)
	(schedule-item 'room)))

(define course
  (lambda (schedule-item)
	(schedule-item 'course)))

(define time
  (lambda (schedule-item)
	(schedule-item 'time)))

;;; Exercise 6.25
(define make-game-state-comparator
  (lambda (cmp?)
	(lambda (gs-a gs-b)
	  (cmp? (total-size gs-a)
			(total-size gs-b)))))

(define game-state-<
  (make-game-state-comparator <))

(game-state-< (make-game-state 3 7)
			  (make-game-state 1 12))
;; #t

(define game-state->
  (make-game-state-comparator >))

(game-state-> (make-game-state 3 7)
			  (make-game-state 1 12))
;; #f

(game-state-> (make-game-state 13 7)
			  (make-game-state 1 12))
;; #t

;;; Exercise 6.26
;; A:
(define make-point
  (lambda (x y)
	(cons x y)))

(define x-coord
  (lambda (point)
	(car point)))

(define y-coord
  (lambda (point)
	(cdr point)))

;; B:
(define distance
  (lambda (p-a p-b)
	(sqrt (+ (square (abs (- (x-coord p-a)
							 (x-coord p-b))))
			 (square (abs (- (y-coord p-a)
							 (y-coord p-b))))))))

(define pt-1
  (make-point -1 -1))

(define pt-2
  (make-point -1 1))

(distance pt-1 pt-2) ;; 2
