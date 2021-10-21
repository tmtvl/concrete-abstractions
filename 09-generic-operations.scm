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
			((equal? op 'sequence-ref)
			 (lambda (n)
			   (if (and (<= 0 n)
						(<= n
							(- high low)))
				   (+ low n)
				   (error 'sequence-from-to
						  "Index out of range"
						  n))))))))

(define sequence-ref
  (lambda (sequence n)
	((sequence 'sequence-ref) n)))

(sequence-ref (sequence-from-to 5 10)
			  3)
;; 8

;;; Exercise 9.5
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
			((equal? op 'sequence-ref)
			 (lambda (n)
			   (list-ref lst n)))
			(else
			 (error 'list-of-length->sequence
					"Illegal sequence operation"
					op))))))

(define list->sequence
  (lambda (lst)
	(list-of-length->sequence lst
							  (length lst))))

(sequence-ref (list->sequence '(4 2 5 7 1 3 8 6))
			  3)
;; 7

(define sequence-cons
  (lambda (head tail)
	(let ((new-length (+ 1
						 (sequence-length tail))))
	  (lambda (op)
		(cond ((equal? op 'empty-sequence)
			   #f)
			  ((equal? op 'head)
			   head)
			  ((equal? op 'tail)
			   tail)
			  ((equal? op 'sequence-length)
			   new-length)
			  ((equal? op 'sequence-ref)
			   (lambda (n)
				 (if (= n 0)
					 head
					 (sequence-ref tail
								   (- n 1)))))
			  (else
			   (error 'sequence-cons
					  "Illegal sequence operation"
					  op)))))))

;;; Exercise 9.6
(define sequence-map
  (lambda (proc seq)
	(lambda (op)
	  (cond ((eq? op 'empty-sequence?)
			 (empty-sequence? seq))
			((eq? op 'head)
			 (proc (head seq)))
			((eq? op 'tail)
			 (sequence-map proc
						   (tail seq)))
			((eq? op 'sequence-length)
			 (sequence-length seq))
			((eq? op 'sequence-ref)
			 (lambda (n)
			   (sequence-ref seq n)))
			(else
			 (error 'sequence-map
					"Illegal sequence operation"
					op))))))

(define sequence-append
  (lambda (seq-1 seq-2)
	(cond ((empty-sequence? seq-1)
		   seq-2)
		  ((empty-sequence? seq-2)
		   seq-1)
		  (else
		   (let ((seq-1-length (sequence-length seq-1))
				 (seq-2-length (sequence-length seq-2)))
			 (lambda (op)
			   (cond ((equal? op 'empty-sequence?)
					  #f)
					 ((equal? op 'head)
					  (head seq-1))
					 ((equal? op 'tail)
					  (sequence-append (tail seq-1)
									   seq-2))
					 ((equal? op 'sequence-length)
					  (+ seq-1-length seq-2-length))
					 ((equal? op 'sequence-ref)
					  (lambda (n)
						(if (< n seq-1-length)
							(sequence-ref seq-1 n)
							(sequence-ref seq-2
										  (- n seq-1-length)))))
					 (else
					  (error 'sequence-append
							 "Illegal sequence operation"
							 op)))))))))

(define tagged-datum
  (lambda (type value)
	(cons type value)))

(define type car)
(define contents cdr)

;;; Exercise 9.7
(define list->tagged-list
  (lambda (lst tag)
	(map (lambda (elt)
		   (tagged-datum tag elt))
		 lst)))

(define movie?
  (lambda (x)
	(equal? (type x)
			'movie)))

(define book?
  (lambda (x)
	(equal? (type x)
			'book)))

(define cd?
  (lambda (x)
	(equal? (type x)
			'cd)))

;;; Exercise 9.8
;; A:
;; This is a silly thing, how would we handle Ludwig van Beethoven,
;; '(beethoven ludwig van)? How about 平信彦? '(taira nobuhiko no)?
(define family-name-last
  (lambda (name)
	(define insert-at-end
	  (lambda (name family-name)
		(if (null? name)
			(cons family-name '())
			(cons (car name)
				  (insert-at-end (cdr name)
								 family-name)))))

	(insert-at-end (car name)
				   (cdr name))))

;; B:
(define creator
  (lambda (x)
	(cond ((movie? x)
		   (movie-director x))
		  ((cd? x)
		   (family-name-last
			(cd-artist x)))
		  ((book? x)
		   (family-name-last
			(book-author x)))
		  (else
		   (error 'creator
				  "Couldn't find the creator of the provided media"
				  x)))))

;;; Exercise 9.9
;; The testable value approach has the downside that it could interfere with
;; correct operation of the database if it can contain records that have the
;; testable value in a field (e.g. a movie that has no actors or a book that
;; has no identifiable author). This is just a hypothetical scenario that would
;; have other ways of handling the issue, though, and the only true difference
;; is where the extra check has to be done.

;;; Exercise 9.10
;; A:
;; Due to the architecture of our system as written up to now we would need to
;; add an additional clause to every type check.
;; B:
;; We would need to do the entire type check in the generic operation.
;; C:
;; We would need to write a procedure that does the type check and selects the
;; relevant data.

(define make-type
  (lambda (name operation-table)
	(cons name operation-table)))

(define type-name car)
(define type-operation-table cdr)

(define make-movie
  (lambda (title director year-made actors)
	(list title director year-made actors)))

(define movie-title car)
(define movie-director cadr)
(define movie-year-made caddr)
(define movie-actors cadddr)

(define display-movie
  (lambda (movie)
	(display (movie-title movie))
	(newline)
	(display "directed by: ")
	(display (movie-director movie))
	(newline)
	(display "in: ")
	(display (movie-year-made movie))
	(newline)
	(display "starring: ")
	(display (movie-actors movie))
	(newline)))

(define movie
  (make-type 'movie
			 (make-table
			  '(title year-made director actors creator display-item)
			  (list movie-title movie-year-made movie-director
					movie-actors movie-director display-movie))))

;; (define database
;;   (append (list->tagged-list movies movie)
;; 		  (list->tagged-list books book)
;; 		  (list->tagged-list cds cd)))

(define make-table
  (lambda (keys value)
	(cons keys values)))

(define table-find
  (lambda (table key what-if-found what-if-not)
	(define loop
	  (lambda (keys values)
		(cond ((null? keys)
			   (what-if-not))
			  ((equal? key
					   (car keys))
			   (what-if-found (car values)))
			  (else
			   (loop (cdr keys)
					 (cdr values))))))

	(loop (car table)
		  (cdr table))))

(define operate
  (lambda (operation-name value)
	(table-find (type-operation-table (type value))
				operation-name
				(lambda (procedure)
				  (procedure (contents value)))
				(lambda ()
				  (error 'operate
						 "No way of doing operation on type"
						 operation-name
						 (type-name (type value)))))))

(define title
  (lambda (tagged-datum)
	(operate 'title tagged-datum)))

;;; Exercise 9.11
(define movie?
  (lambda (type)
	(eq? (type-name type)
		 'movie)))

;;; Exercise 9.12
;; A:
;; Now adding a new type simply means writing the applicable predicates and
;; adding the items to the database.
;; B:
;; Adding the new field is very simple as we already have our default error
;; message defined.
;; C:
;; Adding a field to a type simply means writing the predicate and adding it to
;; the operation table.

;;; Exercise 9.13
;; A:
;; I would create a function generator that we can pass a procedure which
;; provides the data we are interested in and which transforms it if needed.
;; For a CD it would make sense to have an album-artists operation.
;; B:
;; Changing the movie database to support a list of directors would allow us to
;; add movies to the database which have multiple directors. The disadvantage
;; would be needing to convert the legacy data to the new format. The advantage
;; of the transformation approach is that generalising it would allow us to add
;; transformers for other fields. For example we could unify chapters of a book
;; and tracks on a CD, where tracks can have collaborating artists while book
;; chapters don't tend to have different authors than the book proper. The main
;; disadvantage of transformers being that it could cause queries to return
;; certain data that lacks important contextual information.
