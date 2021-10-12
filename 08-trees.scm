(import (rnrs (6)))

(define make-video-record
  (lambda (title director year-made actors)
	(vector title director year-made actors)))

(define video-title
  (lambda (video)
	(vector-ref video 0)))

(define video-director
  (lambda (video)
	(vector-ref video 1)))

(define video-year-made
  (lambda (video)
	(vector-ref video 2)))

(define video-actors
  (lambda (video)
	(vector-ref video 3)))

(define make-empty-tree
  (lambda ()
	'()))

(define make-nonempty-tree
  (lambda (root left-subtree right-subtree)
	(list root left-subtree right-subtree)))

(define empty-tree? null?)
(define root car)
(define left-subtree cadr)
(define right-subtree caddr)

(define test-tree
  (make-nonempty-tree
   3
   (make-nonempty-tree
	2
	(make-nonempty-tree
	 1
	 (make-empty-tree)
	 (make-empty-tree))
	(make-empty-tree))
   (make-nonempty-tree
	5
	(make-nonempty-tree
	 4
	 (make-empty-tree)
	 (make-empty-tree))
	(make-nonempty-tree
	 6
	 (make-empty-tree)
	 (make-empty-tree)))))

(define in?
  (lambda (value tree)
	(cond ((empty-tree? tree)
		   #f)
		  ((= value
			  (root tree))
		   #t)
		  ((< value
			  (root tree))
		   (in? value
				(left-subtree tree)))
		  (else
		   (in? value
				(right-subtree tree))))))

;;; Exercise 8.1
(define minimum
  (lambda (tree)
	(define look-for-minimum
	  (lambda (subtree)
		(if (empty-tree? (left-subtree tree))
			(root tree)
			(look-for-minimum (left-subtree tree)))))

	(if (empty-tree? tree)
	    tree
		(look-for-minimum tree))))

;;; Exercise 8.2
(define number-of-nodes
  (lambda (tree)
	(define count-nodes
	  (lambda (tree other-nodes past-nodes)
		(let ((nodes (+ past-nodes 1)))
		  (cond ((not (empty-tree? tree))
				 (count-nodes (left-subtree tree)
							  (cons (right-subtree tree)
									other-nodes)
							  nodes))
				((null? other-nodes)
				 nodes)
				(else
				 (count-nodes (car other-nodes)
							  (cdr other-nodes)
							  nodes))))))

	(count-nodes tree '() 0)))

(define compare-by-director
  (lambda (video-record name)
	(define name->string
	  (lambda (name)
		(string-join
		 (map symbol->string name))))

	(let ((director-name
		   (name->string (video-director video-record)))
		  (name-string (name->string name)))
	  (cond ((string< director-name name-string)
			 '<)
			((string> director-name name-string)
			 '>)
			(else
			 '=)))))

(define list-by-key
  (lambda (key-value comparator tree)
	(if (empty-tree? tree)
		'()
		(let ((comparison-result (comparator (root tree)
											 key-value)))
		  (cond
		   ((equal? comparison-result '=)
			(cons (root tree)
				  (append (list-by-key key-value
									   comparator
									   (left-subtree tree))
						  (list-by-key key-value
									   comparator
									   (right-subtree tree)))))
		   ((equal? comparison-result '<)
			(list-by-key key-value
						 comparator
						 (right-subtree tree)))
		   (else
			(list-by-key key-value
						 comparator
						 (left-subtree tree))))))))

(define preorder-onto
  (lambda (tree list)
	(if (empty-tree? tree)
		list
		(cons (root tree)
			  (preorder-onto (left-subtree tree)
							 (preorder-onto (right-subtree tree)
											list))))))

(define preorder
  (lambda (tree)
	(preorder-onto tree '())))

(preorder test-tree)
;; (3 2 1 5 4 6)

;;; Exercise 8.3
(define list-by-key
  (lambda (key-value comparator tree)
	(define append-by-key
	  (lambda (tree list)
		(if (empty-tree? tree)
			list
			(let ((comparison-result (comparator (root tree)
												 key-value)))
			  (cond ((equal? comparison-result '=)
					 (cons (root tree)
						   (append-by-key (left-subtree tree)
										  (append-by-key (right-subtree tree)
														 list))))
					((equal? comparison-result '<)
					 (append-by-key (right-subtree tree)
									list))
					(else
					 (append-by-key (left-subtree tree)
									list)))))))

	(append-by-key tree '())))

(define inorder
  (lambda (tree)
	(if (empty-tree? tree)
		'()
		(append (inorder (left-subtree tree))
				(cons (root tree)
					  (inorder (right-subtree tree)))))))

;;; Exercise 8.4
(define inorder
  (lambda (tree)
	(define inorder-onto
	  (lambda (tree list)
		(if (empty-tree? tree)
			list
			(inorder-onto (left-subtree tree)
						  (cons (root tree)
								(inorder-onto (right-subtree tree)
											  list))))))

	(inorder-onto tree '())))

(inorder test-tree)
;; (1 2 3 4 5 6)

;;; Exercise 8.5
(define postorder
  (lambda (tree)
	(define postorder-onto
	  (lambda (tree list)
		(if (empty-tree? tree)
			list
			(postorder-onto (right-subtree tree)
							(postorder-onto (left-subtree tree)
											(cons (root tree)
												  list))))))

	(postorder-onto tree '())))

(postorder test-tree)
;; (6 4 5 1 2 3)

;;; Exercise 8.6
(define insert
  (lambda (number number-tree)
	(cond ((empty-tree? number-tree)
		   (make-nonempty-tree number
							   (make-empty-tree)
							   (make-empty-tree)))
		  ((< number
			  (root number-tree))
		   (make-nonempty-tree (root number-tree)
							   (insert number
									   (left-subtree number-tree))
							   (right-subtree number-tree)))
		  (else
		   (make-nonempty-tree (root number-tree)
							   (left-subtree number-tree)
							   (insert number
									   (right-subtree number-tree)))))))

(insert 3 (make-empty-tree))
;; (3 () ())
(insert 7 test-tree)
;; (3 (2 (1 () ()) ()) (5 (4 () ()) (6 () (7 () ()))))

;;; Exercise 8.7
(define list->bstree
  (lambda (list-of-numbers)
	(define insert-onto
	  (lambda (lst tree)
		(if (null? lst)
			tree
			(insert-onto (cdr lst)
						 (insert (car lst)
								 tree)))))

	(insert-onto list-of-numbers
				 (make-empty-tree))))

(list->bstree '(1 2 3 4 5 6))
;; (1 () (2 () (3 () (4 () (5 () (6 () ()))))))
;; 1
;;  \
;;   2
;;    \
;;     3
;;      \
;;       4
;;        \
;;         5
;;          \
;;           6

(list->bstree '(3 2 5 1 4 6))
;; (3 (2 (1 () ()) ()) (5 (4 () ()) (6 () ())))
;;     3
;;    / \
;;   2   5
;;  /   / \
;; 1   4   6

(list->bstree '(1 6 5 2 4 3))
;; (1 () (6 (5 (2 () (4 (3 () ()) ())) ()) ()))
;;   1
;;    \
;;     6
;;    /
;;   5
;;  / \
;; 2   4
;;    /
;;   3

;; For a short, bushy tree we want the values to be mixed up, preferably with
;; the averages in front. For a tall, skinny tree we want the values to be
;; ordered.

;;; Exercise 8.8
(define leaf?
  (lambda (tree)
	(and (not (empty-tree? tree))
		 (empty-tree? (left-subtree tree))
		 (empty-tree? (right-subtree tree)))))

;;; Exercise 8.9
(define tree-height
  (lambda (tree)
	(if (empty-tree? tree)
		0
		(+ (max (tree-height (left-subtree tree))
				(tree-height (right-subtree tree)))
		   1))))

(tree-height test-tree)
;; 3

;;; Exercise 8.10
;; Base case:
;; A tree of height 0, per evaluation of if, occurs if the tree is empty, and
;; thus consists of only a null node. As (2 ^ (0 + 1)) - 1 = 2 - 1 = 1, our
;; theorem holds.

;; Induction theory:
;; A tree of height k has (2 ^ (k + 1)) - 1 nodes for any k in the range 0 <= k
;; < h.

;; Inductive step:
;; A tree of height h has 1 node + 2 times the nodes of a tree of height h - 1.
;; Per our inductive step a tree of height h - 1 has (2 ^ ((h - 1) + 1)) - 1 =
;; (2 ^ h) - 1 nodes. Therefore a tree of height h has (2 * ((2 ^ h) - 1)) + 1
;; = ((2 ^ (h + 1)) - 2) + 1 = (2 ^ (h + 1)) - 1 nodes.

;; Conclusion:
;; Therefore we can prove through mathematical induction that a tree of height
;; h has (2 ^ (h + 1)) - 1 nodes for any nonnegative integer h.

;;; Exercise 8.11
;; nodes(h)
;; |= 1 if h = 0.
;; |= 1 + m * nodes(h - 1) if h > 0.
;;
;; nodes(h) = (m ^ h) + (m ^ (h - 1)) + ... + m ^ 2 + m + 1.

;;; Exercise 8.12
(define take
  (lambda (n lst)
	(if (= n 0)
		'()
		(cons (car lst)
			  (take (- n 1)
					(cdr lst))))))

(define drop
  (lambda (n lst)
	(if (= n 0)
		lst
		(drop (- n 1)
			  (cdr lst)))))

(define split
  (lambda (n lst)
	(cons (take n lst)
		  (drop n lst))))

(define sorted-list->min-height-bstree
  (lambda (lst)
	(cond ((null? lst)
		   (make-empty-tree))
		  ((null? (cdr lst))
		   (insert (car lst)
				   (make-empty-tree)))
		  (else
		   (let ((lsts (split (quotient (length lst)
										2)
							  lst)))
			 (make-nonempty-tree (cadr lsts)
								 (sorted-list->min-height-bstree
								  (car lsts))
								 (sorted-list->min-height-bstree
								  (cddr lsts))))))))

(sorted-list->min-height-bstree '(1 2 3 4 5 6))
;; (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) ()))
;;      4
;;    _/ \_
;;   2     6
;;  / \   /
;; 1   3 5

;;; Exercise 8.13
(define optimize-bstree
  (lambda (tree)
	(sorted-list->min-height-bstree
	 (inorder tree))))

(define long-tree
  (list->bstree '(1 2 3 4 5 6)))
long-tree ;; (1 () (2 () (3 () (4 () (5 () (6 () ()))))))
(optimize-bstree long-tree)
;; (4 (2 (1 () ()) (3 () ())) (6 (5 () ()) ()))

;;; Exercise 8.14
(let ((sort (lambda (lst) (inorder (list->bstree lst)))))
  (sort '(5 7 1 4 2 9 8 6 3)))
;; (1 2 3 4 5 6 7 8 9)
