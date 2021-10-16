(import (rnrs (6)))

(define sequence-from-to
  (lambda (low high)
	(lambda (op)
	  (cond ((equal? op 'empty-sequence?)
			 (> low high))
			((equal? op 'head)
			 low)
			((equal? op 'tail)
			 (sequence-from-to (+ low 1)
							   high))
			((equal? op 'sequence-length)
			 (if (> low high)
				 0
				 (+ (- high low)
					1)))
			(else
			 (error 'sequence-from-to
					"Illegal sequence operation"
					op))))))

(define seq-1 (sequence-from-to 3 100))

(seq-1 'head) ;; 3
(seq-1 'sequence-length) ;; 98
(seq-1 'tail) ;; #<procedure 7fea8c200840 at <unknown port>:138:8 (op)>
((seq-1 'tail) 'head) ;; 4

(define head
  (lambda (sequence)
	(sequence 'head)))

(define tail
  (lambda (sequence)
	(sequence 'tail)))

(define empty-sequence?
  (lambda (sequence)
	(sequence 'empty-sequence?)))

(define sequence-length
  (lambda (sequence)
	(sequence 'sequence-length)))

(head seq-1) ;; 3
(sequence-length seq-1) ;; 98
(head (tail seq-1)) ;; 4

;;; Exercise 9.1
(define sequence->list
  (lambda (sequence)
	(if (empty-sequence? sequence)
		'()
		(cons (head sequence)
			  (sequence->list (tail sequence))))))

(sequence->list seq-1)
;; (3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
;;  30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54
;;  55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
;; 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100)

;;; Exercise 9.2
;; A:
(define sequence-with-from-by
  (lambda (len start inc)
	(lambda (op)
	  (cond ((equal? op 'empty-sequence?)
			 (= len 0))
			((equal? op 'head)
			 start)
			((equal? op 'tail)
			 (sequence-with-from-by (- len 1)
									(+ start inc)
									inc))
			((equal? op 'sequence-length)
			 len)
			(else
			 (error 'sequence-with-from-by
					"Illegal sequence operation"
					op))))))

(sequence->list (sequence-with-from-by 5 6 -1))
;; (6 5 4 3 2)

;; B:
(define sequence-from-to
  (lambda (low high)
	(sequence-with-from-by (+ (- high low)
							  1)
						   low
						   1)))

(sequence->list (sequence-from-to 2 5))
;; (2 3 4 5)

;; C:
(define sequence-from-to-with
  (lambda (low high len)
	(if (= len 1)
		(sequence-with-from-by 1
							   low
							   (- high low))
		(let ((inc (/ (- high low)
					  (- len 1))))
		  (sequence-with-from-by len low inc)))))

(sequence->list (sequence-from-to-with 5 11 4))
;; (5 7 9 11)

(define list->sequence
  (lambda (lst)
	(lambda (op)
	  (cond ((equal? op 'empty-sequence?)
			 (null? lst))
			((equal? op 'head)
			 (car lst))
			((equal? op 'tail)
			 (list->sequence (cdr lst)))
			((equal? op 'sequence-length)
			 (length lst))
			(else
			 (error 'list->sequence
					"Illegal sequence operation"
					op))))))

(define seq-2 (sequence-with-from-by 6 5 -1))
(define seq-3 (list->sequence '(4 3 7 9)))

(head seq-2) ;; 5
(head seq-3) ;; 4

;;; Exercise 9.3
(define empty-sequence
  (lambda ()
	(list->sequence '())))

;;; Exercise 9.4
;; A:
(define list->sequence
  (lambda (lst)
	(let ((len (length lst)))
	  (lambda (op)
		(cond ((equal? op 'empty-sequence?)
			   (null? lst))
			  ((equal? op 'head)
			   (car lst))
			  ((equal? op 'tail)
			   (list->sequence (cdr lst)))
			  ((equal? op 'sequence-length)
			   len)
			  (else
			   (error 'list->sequence
					  "Illegal sequence operation"
					  op)))))))

;; B:
(define list-of-length->sequence
  (lambda (lst len)
	(lambda (op)
	  (cond ((equal? op 'empty-sequence?)
			 (null? lst))
			((equal? op 'head)
			 (car lst))
			((equal? op 'tail)
			 (list-of-length->sequence (cdr lst)
									   (- len 1)))
			((equal? op 'sequence-length)
			 len)
			(else
			 (error 'list-of-length->sequence
					"Illegal sequence operation"
					op))))))

(define list->sequence
  (lambda (lst)
	(list-of-length->sequence lst
							  (length lst))))
