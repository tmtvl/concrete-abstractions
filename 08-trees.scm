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

;;; Exercise 8.15
(define make-constant
  (lambda (x)
	x))

(define constant? number?)

(define make-expr
  (lambda (left-operand operator right-operand)
	(list left-operand operator right-operand)))

(define operator cadr)
(define left-operand car)
(define right-operand caddr)

(define look-up-value
  (lambda (name)
	(cond ((equal? name '+)
		   +)
		  ((equal? name '*)
		   *)
		  ((equal? name '-)
		   -)
		  ((equal? name '/)
		   /)
		  (else
		   (error 'look-up-value
				  "Unrecognised name"
				  name)))))

(define evaluate
  (lambda (expr)
	(cond ((constant? expr)
		   expr)
		  (else
		   ((look-up-value (operator expr))
			(evaluate (left-operand expr))
			(evaluate (right-operand expr)))))))

(evaluate
 (make-expr
  1
  '+
  (make-expr
   2
   '*
   (make-expr 3
			  '-
			  '5))))
;; -3

;;; Exercise 8.16
(define all-operators
  (lambda (expr)
	(define enumerate-operators
	  (lambda (expr-to-check remaining-exprs operators)
		(cond ((not (constant? expr-to-check))
			   (enumerate-operators (left-operand expr-to-check)
									(cons (right-operand expr-to-check)
										  remaining-exprs)
									(if (member (operator expr-to-check)
												operators)
										operators
										(cons (operator expr-to-check)
											  operators))))
			  ((null? remaining-exprs)
			   operators)
			  (else
			   (enumerate-operators (car remaining-exprs)
									(cdr remaining-exprs)
									operators)))))

	(enumerate-operators expr '() '())))

(all-operators '(1 + (2 * (3 - 5))))
;; (- * +)

;;; Exercise 8.17
(define count-operations
  (lambda (expr)
	(define enumerate-operations
	  (lambda (expr-to-check remaining-exprs operations)
		(cond ((not (constant? expr-to-check))
			   (enumerate-operations (left-operand expr-to-check)
									 (cons (right-operand expr-to-check)
										   remaining-exprs)
									 (+ operations 1)))
			  ((null? remaining-exprs)
			   operations)
			  (else
			   (enumerate-operations (car remaining-exprs)
									 (cdr remaining-exprs)
									 operations)))))

	(enumerate-operations expr '() 0)))

(count-operations '(1 + (2 * (3 - 5))))
;; 3


(define make-empty-trie
  (lambda ()
	'()))

(define make-nonempty-trie
  (lambda (root-values ordered-subtries)
	(list root-values ordered-subtries)))

(define empty-trie? null?)

(define root-values car)

(define subtries cadr)

(define subtrie-with-label
  (lambda (trie label)
	(list-ref (subtries trie)
			  (- label 2))))

(define make-person
  (lambda (name phone-number)
	(list name phone-number)))

(define name car)

(define phone-number cadr)

(define menu
  (lambda ()
	(newline)
	(display "Enter the name, one digit at a time.")
	(newline)
	(display "Indicate you are done by 0.")
	(newline)))

(define display-phone-numbers
  (lambda (people)
	(define display-loop
	  (lambda (people)
		(cond ((null? people)
			   'done)
			  (else (newline)
					(display (name (car people)))
					(display "'s phone number is: ")
					(display (phone-number (car people)))
					(display-loop (cdr people))))))

	(if (null? people)
		(display "Sorry, we can't find that name.")
		(display-loop people))))

(define look-up-phone-number
  (lambda (phone-trie)
	(newline)
	(if (empty-trie? phone-trie)
		(display "Sorry, we can't find that name.")
		(let ((user-input (read)))
		  (if (= user-input 0)
			  (display-phone-numbers (root-values phone-trie))
			  (look-up-phone-number (subtrie-with-label
									 phone-trie
									 user-input)))))))

(define look-up-with-menu
  (lambda (phone-trie)
	(menu)
	(look-up-phone-number phone-trie)))

(define test-trie
  (make-nonempty-trie '(1 2)
					  (list (make-nonempty-trie '(3 4)
												(list (make-empty-trie)
													  (make-empty-trie)))
							(make-empty-trie))))

;;; Exercise 8.18
(define number-in-trie
  (lambda (trie)
	(if (empty-trie? trie)
		0
		(apply +
			   (cons (length (root-values trie))
					 (map number-in-trie
						  (subtries trie)))))))

(number-in-trie test-trie)
;; 4

;;; Exercise 8.19
(define values-in-trie
  (lambda (trie)
	(if (empty-trie? trie)
		'()
		(apply append
			   (cons (root-values trie)
					 (map values-in-trie
						  (subtries trie)))))))

(values-in-trie test-trie)
;; (1 2 3 4)

;;; Exercise 8.19
;; A:
(define look-up-phone-number
  (lambda (phone-trie)
	(newline)
	(cond ((empty-trie? phone-trie)
		   (display "Sorry, we can't find that name."))
		  ((< (number-in-trie phone-trie)
			  2)
		   (display-phone-numbers (values-in-trie phone-trie)))
		  (else
		   (let ((user-input (read)))
			 (if (= user-input 0)
				 (display-phone-numbers (root-values phone-trie))
				 (look-up-phone-number (subtrie-with-label
										phone-trie
										user-input))))))))
;; B:
(define menu
  (lambda ()
	(newline)
	(display "Enter the name, one digit at a time.")
	(newline)
	(display "Indicate you are done by 0.")
	(newline)
	(display "Display the possible names with 1.")
	(newline)))

(define look-up-phone-number
  (lambda (phone-trie)
	(newline)
	(cond ((empty-trie? phone-trie)
		   (display "Sorry, we can't find that name."))
		  ((< (number-in-trie phone-trie)
			  2)
		   (display-phone-numbers (values-in-trie phone-trie)))
		  (else
		   (let ((user-input (read)))
			 (cond ((= user-input 0)
					(display-phone-numbers (root-values phone-trie)))
				   ((= user-input 1)
					(display (map name
								  (values-in-trie phone-trie)))
					(look-up-phone-number phone-trie))
				   (else
					(look-up-phone-number (subtrie-with-label
										   phone-trie
										   user-input)))))))))

;;; Exercise 8.21
(define letter->number
  (lambda (letter)
	(cond ((member letter
				   '(A a B b C c))
		   2)
		  ((member letter
				   '(D d E e F f))
		   3)
		  ((member letter
				   '(G g H h I i))
		   4)
		  ((member letter
				   '(J j K k L l))
		   5)
		  ((member letter
				   '(M m N n O o))
		   6)
		  ((member letter
				   '(P p Q q R r S s))
		   7)
		  ((member letter
				   '(T t U u V v))
		   8)
		  ((member letter
				   '(W w X x Y y Z z))
		   9)
		  (else
		   (error 'letter->number
				  "Unknown letter."
				  letter)))))

(map letter->number
	 '(a l y s s a))
;; (2 5 9 7 7 2)

;;; Exercise 8.22
(define explode-symbol
  (lambda (sym)
	(map string->symbol
		 (map string
			  (string->list
			   (symbol->string sym))))))

(define name->labels
  (lambda (name)
	(map letter->number
		 (explode-symbol name))))

(name->labels 'ritter)
;; (7 4 8 8 3 7)

(define make-labeled-value
  (lambda (labels value)
	(list labels value)))

(define labels car)

(define value cadr)

;;; Exercise 8.23
(define empty-labels?
  (lambda (labeled-value)
	(null? (labels labeled-value))))

;;; Exercise 8.24
(define first-label
  (lambda (labeled-value)
	(car (labels labeled-value))))

;;; Exercise 8.25
(define strip-one-label
  (lambda (labeled-value)
	(make-labeled-value
	 (cdr (labels labeled-value))
	 (value labeled-value))))

(define labeled-ritter
  (make-labeled-value '(7 4 8 8 3 7)
					  (make-person 'ritter
								   7479)))

(labels (strip-one-label labeled-ritter))
;; (4 8 8 3 7)
(name (value (strip-one-label labeled-ritter)))
;; ritter
(phone-number (value (strip-one-label labeled-ritter)))
;; 7479

;;; Exercise 8.26
(define value->labeled-value
  (lambda (person)
	(make-labeled-value (name->labels (name person))
						person)))

;;; Exercise 8.27
(define values-with-first-label
  (lambda (values label)
	(define first-label=
	  (lambda (labeled-value)
		(= (car (labels labeled-value))
		   label)))

	(map strip-one-label
		 (filter first-label= values))))

(values-with-first-label
 (map value->labeled-value
	  (list (make-person 'abby 1137)
			(make-person 'adonis 9595)
			(make-person 'benjamin 4762)
			(make-person 'gary 1712)
			(make-person 'gwen 2854)
			(make-person 'harry 5634)
			(make-person 'kate 8462)
			(make-person 'margaret 7344)
			(make-person 'victor 8265)))
 4)
;; (((2 7 9) (gary 1712)) ((9 3 6) (gwen 2854)) ((2 7 7 9) (harry 5634)))

;;; Exercise 8.28
(define categorize-by-first-label
  (lambda (values)
	(define loop-categories
	  (lambda (label)
		(if (> label 9)
			'()
			(cons (values-with-first-label values
										   label)
				  (loop-categories (+ label 1))))))

	(loop-categories 2)))

;;; Exercise 8.29
(define filter-split
  (lambda (pred? lst)
	(define fs-iter
	  (lambda (lst conc)
		(if (null? lst)
			(conc '() '())
			(fs-iter (cdr lst)
					 (lambda (hits misses)
					   (if (pred? (car lst))
						   (conc (cons (car lst)
									   hits)
								 misses)
						   (conc hits
								 (cons (car lst)
									   misses))))))))

	(fs-iter lst cons)))

(define labeled-values->trie
  (lambda (labeled-values)
	(if (null? labeled-values)
		(make-empty-trie)
		(let ((split-values (filter-split empty-labels? labeled-values)))
		  (make-nonempty-trie (map value (car split-values))
							  (map labeled-values->trie
								   (categorize-by-first-label
									(cdr split-values))))))))

(define values->trie
  (lambda (values)
	(labeled-values->trie (map value->labeled-value
							   values))))

(define phone-trie
  (values->trie (list (make-person 'lindt      7483)
					  (make-person 'cadbury    7464)
					  (make-person 'wilbur     7466)
					  (make-person 'hershey    7482)
					  (make-person 'spruengly  7009)
					  (make-person 'merkens    7469)
					  (make-person 'baker      7465)
					  (make-person 'ghiradelli 7476)
					  (make-person 'tobler     7481)
					  (make-person 'suchard    7654)
					  (make-person 'callebaut  7480)
					  (make-person 'ritter     7479)
					  (make-person 'maillard   7477)
					  (make-person 'see        7463)
					  (make-person 'perugina   7007))))

;;; Exercise 8.30
(define successor-of-in-or
  (lambda (value bst if-none)
	(cond ((empty-tree? bst)
		   if-none)
		  ((<= (root bst)
			   value)
		   (successor-of-in-or value
							   (right-subtree bst)
							   if-none))
		  (else
		   (successor-of-in-or value
							   (left-subtree bst)
							   (root bst))))))

;;; Exercise 8.31
(define count-nodes-between
  (lambda (bst lower upper)
	(cond ((empty-tree? bst)
		   0)
		  ((< (root bst)
			  lower)
		   (count-nodes-between (right-subtree bst)
								lower
								upper))
		  ((> (root bst)
			  upper)
		   (count-nodes-between (left-subtree bst)
								lower
								upper))
		  (else
		   (+ 1
			  (count-nodes-between (left-subtree bst)
								   lower
								   upper)
			  (count-nodes-between (right-subtree bst)
								   lower
								   upper))))))

;;; Exercise 8.32
(define list-nodes-between
  (lambda (bst lower upper)
	(define between-onto
	  (lambda (bst lst)
		(cond ((empty-tree? bst)
			   lst)
			  ((< (root bst)
				  lower)
			   (between-onto (right-subtree bst)
							 lst))
			  ((> (root bst)
				  upper)
			   (between-onto (left-subtree bst)
							 lst))
			  (else
			   (between-onto (left-subtree bst)
							 (cons (root bst)
								   (between-onto (right-subtree bst)
												 lst)))))))

	(between-onto bst '())))
