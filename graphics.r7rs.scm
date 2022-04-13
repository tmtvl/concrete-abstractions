;;; R7RS version of the graphics procedures in fungraph.scm.
;;; See additional-material/fungraph.scm for more information.
;;; If you have ghostscript installed, you can convert .eps files to .jpg
;;; images like this:
;;; `gs -sDEVICE=jpeg -dJPEGQ=100 -dEPSCrop -dNOPAUSE -dBATCH -dSAFER -r300 \
;;;     -sOutputFile=out.jpg input.eps'
(define-library (graphics)
  (import (scheme base)
		  (scheme case-lambda)
		  (scheme file)
		  (scheme write))
  (export make-image image? image-proc image-width image-height
		  save-image-as-epsf
		  line
		  ps-image
		  quarter-turn-right
		  mirror-image
		  invert
		  overlay
		  resize-image
		  stack
		  filled-triangle)
  (begin
	(define-record-type <image>
	  (make-image proc width height)
	  image?
	  (proc image-proc set-image-proc!)
	  (width image-width set-image-width!)
	  (height image-height set-image-height!))

	(define *default-image-size* 100)

	(define save-image-as-epsf
	  (let ((left-margin 50)
			(bottom-margin 50))
		(lambda (image filename)
		  (if (not (image? image))
			  (error "argument to save-image-as-epsf is not an image."
					 image)
			  (with-output-to-file filename
				(lambda ()
				  (display "%!PS-Adobe-3.0 EPSF-3.0")
				  (newline)
				  (display "%%BoundingBox: ")
				  (display left-margin)
				  (display " ")
				  (display bottom-margin)
				  (display " ")
				  (display (+ left-margin
							  (image-width image)))
				  (display " ")
				  (display (+ bottom-margin
							  (image-height image)))
				  (newline)
				  ((image-proc image)
				   (lambda (x y)
					 (+ (* (+ x 1)
						   (image-width image)
						   0.5)
						left-margin))
				   (lambda (x y)
					 (+ (* (+ y 1)
						   (image-height image)
						   0.5)
						bottom-margin)))
				  (newline)
				  (display "showpage")
				  (newline)))))))

	(define line
	  (case-lambda
		((x0 y0 x1 y1)
		 (line x0 y0 x1 y1 *default-image-size* *default-image-size*))
		((x0 y0 x1 y1 width)
		 (line x0 y0 x1 y1 width *default-image-size*))
		((x0 y0 x1 y1 width height)
		 (cond ((not (real? x0))
				(error "x0 argument to line isn't a real number."
					   x0))
			   ((not (real? x1))
				(error "x1 argument to line isn't a real number."
					   x1))
			   ((not (real? y0))
				(error "y0 argument to line isn't a real number."
					   y0))
			   ((not (real? y1))
				(error "y1 argument to line isn't a real number."
					   y1))
			   ((not (and (integer? height)
						  (positive? height)))
				(error "height argument to line isn't a positive integer."
					   height))
			   ((not (and (integer? width)
						  (positive? width)))
				(error "width argument to line isn't a positive integer."
					   width))
			   (else
				(make-image (lambda (xt yt)
							  (newline)
							  (display "newpath ")
							  (display (inexact (xt x0 y0)))
							  (display " ")
							  (display (inexact (yt x0 y0)))
							  (display " moveto ")
							  (display (inexact (xt x1 y1)))
							  (display " ")
							  (display (inexact (yt x1 y1)))
							  (display " lineto stroke")
							  (newline))
							width
							height))))))

	(define ps-image
	  (case-lambda
		((ps)
		 (ps-image ps *default-image-size* *default-image-size*))
		((ps width)
		 (ps-image ps width *default-image-size*))
		((ps width height)
		 (cond ((not (string? ps))
				(error "ps argument to ps-image isn't a string."
					   ps))
			   ((not (and (integer? height)
						  (positive? height)))
				(error "height argument to ps-image isn't a positive integer."
					   height))
			   ((not (and (integer? width)
						  (positive? width)))
				(error "width argument to ps-image isn't a positive integer."
					   width))
			   (else
				(make-image (lambda (xt yt)
							  (let* ((tx (xt 0 0))
									 (ty (yt 0 0))
									 (x->x (- (xt 1 0)
											  tx))
									 (x->y (- (yt 1 0)
											  ty))
									 (y->x (- (xt 0 1)
											  tx))
									 (y->y (- (yt 0 1)
											  ty)))
								(newline)
								(display "save [")
								(display (inexact x->x))
								(display " ")
								(display (inexact x->y))
								(display " ")
								(display (inexact y->x))
								(display " ")
								(display (inexact y->y))
								(display " ")
								(display (inexact tx))
								(display " ")
								(display (inexact ty))
								(display "] concat ")
								(display ps)
								(display " restore")
								(newline)))
							width
							height))))))

	(define (quarter-turn-right image)
	  (if (not (image? image))
		  (error "argument to quarter-turn-right not an image."
				 image)
		  (make-image (lambda (xt yt)
						((image-proc image)
						 (lambda (x y)
						   (xt y
							   (- x)))
						 (lambda (x y)
						   (yt y
							   (- x)))))
					  (image-height image)
					  (image-width image))))

	(define (mirror-image image)
	  (if (not (image? image))
		  (error "argument to mirror-image not an image."
				 image)
		  (make-image (lambda (xt yt)
						((image-proc image)
						 (lambda (x y)
						   (xt (- x)
							   y))
						 (lambda (x y)
						   (yt (- x)
							   y))))
					  (image-width image)
					  (image-height image))))

	(define (invert image)
	  (if (not (image? image))
		  (error "argument to invert not an image.")
		  (make-image (lambda (xt yt)
						(let ((x0 (string-append (number->string
												  (inexact (xt -1 -1)))
												 " "))
							  (y0 (string-append (number->string
												  (inexact (yt -1 -1)))
												 " "))
							  (x1 (string-append (number->string
												  (inexact (xt 1 1)))
												 " "))
							  (y1 (string-append (number->string
												  (inexact (yt 1 1)))
												 " ")))
						  (newline)
						  (display "newpath ")
						  (display x0)
						  (display y0)
						  (display "moveto ")
						  (display x0)
						  (display y1)
						  (display "lineto ")
						  (display x1)
						  (display y1)
						  (display "lineto ")
						  (display x1)
						  (display y0)
						  (display "lineto closepath fill gsave 1 currentgray sub setgray")
						  ((image-proc image) xt yt)
						  (display "grestore")
						  (newline)))
					  (image-width image)
					  (image-height image))))

	(define (overlay image . images)
	  (if (not (image? image))
		  (error "argument to overlay not an image."
				 image)
		  (let ((w (image-width image))
				(h (image-height image)))
			(for-each
			 (lambda (i)
			   (if (not (image? i))
				   (error "argument to overlay not an image."
						  i))
			   (if (not (and (= (image-width i)
								w)
							 (= (image-height i)
								h)))
				   (error "Only images of equal size can be overlayed."
						  (cons image images))))
			 images)
			(make-image
			 (lambda (xt yt)
			   (for-each
				(lambda (image)
				  ((image-proc image) xt yt))
				(cons image images)))
			 w
			 h))))

	(define resize-image
	  (case-lambda
		((image)
		 (resize-image image *default-image-size* *default-image-size*))
		((image width)
		 (resize-image image width *default-image-size*))
		((image width height)
		 (cond ((not (image? image))
				(error "image argument to resize-image isn't a string."
					   image))
			   ((not (and (integer? height)
						  (positive? height)))
				(error "height argument to ps-image isn't a positive integer."
					   height))
			   ((not (and (integer? width)
						  (positive? width)))
				(error "width argument to ps-image isn't a positive integer."
					   width))
			   (else
				(make-image (image-proc image)
							width
							height))))))

	(define (stack top . rest)
	  (define (stack2 top bottom)
		(cond ((not (image? top))
			   (error "argument to stack is not an image."
					  top))
			  ((not (image? bottom))
			   (error "argument to stack is not an image."
					  bottom))
			  ((not (= (image-width top)
					   (image-width bottom)))
			   (error "Attempt to stack images of different widths."
					  (list top bottom)))
			  (else
			   (let* ((th (image-height top))
					  (bh (image-height bottom))
					  (h (+ th bh))
					  (inexact-h (inexact h))
					  (tscale (/ th inexact-h))
					  (bscale (/ bh inexact-h)))
				 (make-image
				  (lambda (xt yt)
					((image-proc top)
					 (lambda (x y)
					   (xt x
						   (+ (* tscale y)
							  bscale)))
					 (lambda (x y)
					   (yt x
						   (+ (* tscale y)
							  bscale))))
					((image-proc bottom)
					 (lambda (x y)
					   (xt x
						   (- (* bscale y)
							  tscale)))
					 (lambda (x y)
					   (yt x
						   (- (* bscale y)
							  tscale)))))
				  (image-width top)
				  h)))))

	  (let loop ((image top)
				 (images rest))
		(if (null? images)
			image
			(loop (stack2 image
						  (car images))
				  (cdr images)))))

	(define filled-triangle
	  (case-lambda
		((x0 y0 x1 y1 x2 y2)
		 (filled-triangle x0 y0 x1 y1 x2 y2 *default-image-size*
						  *default-image-size*))
		((x0 y0 x1 y1 x2 y2 width)
		 (filled-triangle x0 y0 x1 y1 x2 y2 width *default-image-size*))
		((x0 y0 x1 y1 x2 y2 width height)
		 (cond ((not (real? x0))
				(error "x0 argument to filled-triangle isn't a real number."
					   x0))
			   ((not (<= -1 x0 1))
				(error "x0 argument to filled-triangle isn't in the -1 to 1 range."
					   x0))
			   ((not (real? x1))
				(error "x1 argument to filled-triangle isn't a real number."
					   x1))
			   ((not (<= -1 x1 1))
				(error "x1 argument to filled-triangle isn't in the -1 to 1 range."
					   x1))
			   ((not (real? x2))
				(error "x2 argument to filled-triangle isn't a real number."
					   x2))
			   ((not (<= -1 x2 1))
				(error "x2 argument to filled-triangle isn't in the -1 to 1 range."
					   x2))
			   ((not (real? y0))
				(error "y0 argument to filled-triangle isn't a real number."
					   y0))
			   ((not (<= -1 y0 1))
				(error "y0 argument to filled-triangle isn't in the -1 to 1 range."
					   y0))
			   ((not (real? y1))
				(error "y1 argument to filled-triangle isn't a real number."
					   y1))
			   ((not (<= -1 y1 1))
				(error "y1 argument to filled-triangle isn't in the -1 to 1 range."
					   y1))
			   ((not (real? y2))
				(error "y2 argument to filled-triangle isn't a real number."
					   y2))
			   ((not (<= -1 y2 1))
				(error "y2 argument to filled-triangle isn't in the -1 to 1 range."
					   y2))
			   ((not (and (integer? height)
						  (positive? height)))
				(error "height argument to filled-triangle isn't a positive integer."
					   height))
			   ((not (and (integer? width)
						  (positive? width)))
				(error "width argument to filled-triangle isn't a positive integer."
					   width))
			   (else
				(ps-image (string-append (number->string (inexact x0))
										 " "
										 (number->string (inexact y0))
										 " moveto "
										 (number->string (inexact x1))
										 " "
										 (number->string (inexact y1))
										 " lineto "
										 (number->string (inexact x2))
										 " "
										 (number->string (inexact y2))
										 " lineto closepath fill")
						  width
						  height))))))))

;;; Basic building blocks for quilting.
;;; See additional-material/quilting.scm for more information.
(define-library (quilting)
  (import (scheme base)
		  (graphics))
  (export test-bb nova-bb bitw-bb)
  (begin
	(define test-bb
	  (filled-triangle 0 1 0 -1 1 -1))

	(define nova-bb
	  (overlay (filled-triangle 0 1 0 0 -1/2 0)
			   (filled-triangle 0 0 0 1/2 1 0)))

	(define bitw-bb
	  (overlay (overlay (filled-triangle -1 1 0 1 -1/2 1/2)
						(filled-triangle -1 -1 0 -1 -1 0))
			   (overlay (filled-triangle 1 1 1 0 0 0)
						(filled-triangle 0 0 1 0 1/2 -1/2))))))

(import (scheme base)
		(graphics)
		(quilting))

;;; Exercise 1.8
;; The `stack' primitive puts images in a vertical column, first image on top,
;; last image on bottom.
;; The `quarter-turn-right' primitive turns an image 90 degrees clockwise.

;;; Exercise 1.9
(define (half-turn image)
  (quarter-turn-right
   (quarter-turn-right image)))

(define (quarter-turn-left image)
  (half-turn
   (quarter-turn-right image)))

;; (define (spin-to-win image)
;;   (half-turn
;;    (half-turn image)))

(define (side-by-side image-a image-b)
  (quarter-turn-left
   (stack
	(quarter-turn-right image-a)
	(quarter-turn-right image-b))))

;;; Exercise 1.10
(define (flip-stack image)
  (stack
   (half-turn image)
   image))

(define (pinwheel image)
  (flip-stack
   (quarter-turn-right
	(stack
	 (half-turn image)
	 (quarter-turn-left image)))))

;;; Exercise 1.11
;; a.
(define rcross-bb
  (overlay (filled-triangle -1 1 -1/2 1/2 1 1)
		   (filled-triangle -1/2 1/2 1/2 1/2 1 1)
		   (filled-triangle 1/2 1/2 1/2 -1/2 1 1)
		   (filled-triangle 1/2 -1/2 1 -1 1 1)
		   (filled-triangle -1/2 -1/2 -1/2 1/2 1/2 -1/2)))

(define corner-bb
  (filled-triangle 1 0 0 1 1 1))
;; b.
(define mitsubishi
  (overlay (filled-triangle -1/3 1/3 0 1 1/3 1/3)
		   (filled-triangle -1/3 1/3 0 -1/3 1/3 1/3)
		   (filled-triangle -1 -1 -2/3 -1/3 -1/3 -1)
		   (filled-triangle -2/3 -1/3 -1/3 -1 0 -1/3)
		   (filled-triangle 0 -1/3 1/3 -1 2/3 -1/3)
		   (filled-triangle 1/3 -1 2/3 -1/3 1 -1)))

;;; Exercise 2.13
(define (stack-copies-of n image)
  (if (< n 2)
	  image
	  (stack image
			 (stack-copies-of (- n 1)
							  image))))

;;; Exercise 2.14
(define (quilt image w h)
  (cond ((> h 1)
		 (quilt (stack-copies-of h image)
				w
				1))
		((> w 1)
		 (side-by-side image
					   (quilt image
							  (- w 1)
							  h)))
		(else
		 image)))

;;; Exercise 2.15
(define (checkerboard-stack-copies-of n image)
  (if (< n 2)
	  image
	  (stack (if (even? n)
				 (invert image)
				 image)
			 (checkerboard-stack-copies-of (- n 1)
										   image))))

(define (checkerboard image w h)
  (cond ((> h 1)
		 (checkerboard (checkerboard-stack-copies-of h image)
					   w
					   1))
		((> w 1)
		 (side-by-side (checkerboard image
									 (- w 1)
									 h)
					   (if (even? w)
						   (invert image)
						   image)))
		(else
		 image)))
