;; This file defines the functional graphics procedures assumed by the textbook
;; Concrete Abstractions: An Introduction to Computer Science Using Scheme
;; by Max Hailperin, Barbara Kaiser, and Karl Knight.
;;
;; This file uses only R4RS standard features of Scheme.  As such, there is
;; no way it can actually cause images to be displayed.  Instead, it provides
;; a procedure called save-image-as-epsf that allows you to write an image
;; out to an Encapsulated PostScript file.  You can then use a system-specific
;; method to either view that file on your screen or print it out to a
;; PostScript printer.   For example, to write an image containing a single
;; diagonal line out to a file with the name line.eps, you could do
;;   (save-image-as-epsf (line -1 -1 1 1) "line.eps")
;;
;; In some Scheme systems, you might be able to write a procedure that
;; automates the process of saving the image out to a file, viewing
;; that file, and then deleting it afterwards.  In some Scheme systems
;; you might even be able to make this happen automagically whenever a
;; value is displayed that is an image (i.e., satisfies the image?
;; predicate).
;;
;; This file defines all the graphics procedures assumed in the textbook,
;; plus some extensions:
;;  (1) (save-image-as-epsf image filename) described above
;;  (2) The line and filled-triangle procedures can optionally be given
;;      one or two additional arguments beyond those described in the book.
;;      If one is given, it specifies the width and height of the image,
;;      while if two are given, the first specifies the width and the second
;;      the height.  In either case, the unit of measure is the point, i.e.,
;;      1/72 of an inch.  If no size is specified, the width and height are
;;      taken as the value of default-image-size. This is defined below as
;;      100, but this can be redefined if you want images consistently bigger
;;      or smaller.
;;  (3) There is one additional way to make an image: ps-image takes a string
;;      of PostScript code and turns it into an image.  Again, one or two
;;      additional arguments can be given to specify width and height.
;;  (4) Overlay and stack are not restricted to two arguments, but rather
;;      can take one or more.
;;  (5) There is a resize-image procedure that takes as arguments an image
;;      and optionally one or two integers to specify width and height.
;;      It returns a new image that is the specified size (or default size)
;;      produced by suitably stretching or shrinking the provided image.
;;  (6) There is a mirror-image procedure that takes an image as argument.
;;      Like quarter-turn-right or invert, this takes an image and makes
;;      another, related image.  In the case of mirror-image, the new image
;;      is the same size as the original, and is formed by flipping the 
;;      original image around a vertical axis, as though it were viewed in a
;;      mirror.
;;
;; This file written by Max Hailperin <max@gustavus.edu>.
;; Last revision March 26, 1998.

;; An image is a three-component data-structure, containing a procedure for
;; outputting the image's PostScript version, a width, and a height.  We
;; use make-image as a constructor, image? as a predicate, and image-proc,
;; image-width, and image-height as selectors.  The image-proc is given two
;; procedural arguments for transforming the x and y coordinates.  Each
;; transformer procedure should be given the x and y coordinates in the
;; -1 to 1 range, and they return the transformed x and y coordinates to
;; use in the output PostScript.

(define save-image-as-epsf
  (let ((left-margin 50) ; margins make printing work better
        (bottom-margin 50))
    (lambda (image filename)
      (if (not (image? image))
          (error "argument to save-image-as-epsf not an image" image))
      (with-output-to-file filename
        (lambda ()
          (display "%!PS-Adobe-3.0 EPSF-3.0")
          (newline)
          (display "%%BoundingBox: ")
          (display left-margin)
          (display " ")
          (display bottom-margin)
          (display " ")
          (display (+ left-margin (image-width image)))
          (display " ")
          (display (+ bottom-margin (image-height image)))
          (newline)
          ((image-proc image)
           (lambda (x y) (+ (* (+ x 1) (image-width image) .5)
                            left-margin))
           (lambda (x y) (+ (* (+ y 1) (image-height image) .5)
                            bottom-margin)))
          (newline)
          (display "showpage")
          (newline))))))

;; We represent the image data structure as a four-element vector,
;; with the first element being a tag to identify it as an image.

(define make-image #f) ; set below
(define image? #f)     ; set below

(let ((image-tag (list 'image)))
  (set! make-image
        (lambda (proc width height)
          (vector image-tag proc width height)))
  (set! image?
        (lambda (obj)
          (and (vector? obj)
               (= (vector-length obj) 4)
               (eq? (vector-ref obj 0) image-tag)))))

(define image-proc
  (lambda (image)
    (if (image? image)
        (vector-ref image 1)
        (error "Argument to image-proc not an image"))))

(define image-width
  (lambda (image)
    (if (image? image)
        (vector-ref image 2)
        (error "Argument to image-width not an image"))))

(define image-height
  (lambda (image)
    (if (image? image)
        (vector-ref image 3)
        (error "Argument to image-height not an image"))))

(define default-image-size 100)

(define (line x0 y0 x1 y1 . wh)
  (if (not (real? x0))
      (error "x0 argument to line not a real" x0))
  (if (not (real? x1))
      (error "x1 argument to line not a real" x1))
  (if (not (real? y0))
      (error "y0 argument to line not a real" y0))
  (if (not (real? y1))
      (error "y1 argument to line not a real" y1))
  (let ((width default-image-size)
        (height default-image-size))
    (if (not (null? wh))
        (begin (set! width (car wh))
               (if (not (null? (cdr wh)))
                   (begin (set! height (cadr wh))
                          (if (not (null? (cddr wh)))
                              (error "too many argument to line")))
                   (set! height width))))
    (if (not (and (integer? height)
                  (integer? width)
                  (exact? height)
                  (exact? width)
                  (> height 0)
                  (> width 0)))
        (error "illegal size specification in line" wh))
    (make-image (lambda (xt yt)
                  (newline)
                  (display "newpath ")
                  (display (exact->inexact (xt x0 y0)))
                  (display " ")
                  (display (exact->inexact (yt x0 y0)))
                  (display " moveto ")
                  (display (exact->inexact (xt x1 y1)))
                  (display " ")
                  (display (exact->inexact (yt x1 y1)))
                  (display " lineto stroke")
                  (newline))
                width height)))

(define (ps-image ps . wh) ; makes an image from a PostScript string
  (if (not (string? ps))
      (error "argument not a string in ps-image" ps))
  (let ((width default-image-size)
        (height default-image-size))
    (if (not (null? wh))
        (begin (set! width (car wh))
               (if (not (null? (cdr wh)))
                   (begin (set! height (cadr wh))
                          (if (not (null? (cddr wh)))
                              (error "too many argument to ps-image")))
                   (set! height width))))
    (if (not (and (integer? height)
                  (integer? width)
                  (exact? height)
                  (exact? width)
                  (> height 0)
                  (> width 0)))
        (error "illegal size specification in ps-image" wh))
    (make-image (lambda (xt yt)
                  (let* ((tx (xt 0 0))
                         (ty (yt 0 0))
                         (x->x (- (xt 1 0) tx))
                         (x->y (- (yt 1 0) ty))
                         (y->x (- (xt 0 1) tx))
                         (y->y (- (yt 0 1) ty)))
                    (newline)
                    (display "save [")
                    (display (exact->inexact x->x))
                    (display " ")
                    (display (exact->inexact x->y))
                    (display " ")
                    (display (exact->inexact y->x))
                    (display " ")
                    (display (exact->inexact y->y))
                    (display " ")
                    (display (exact->inexact tx))
                    (display " ")
                    (display (exact->inexact ty))
                    (display "] concat ")
                    (display ps)
                    (display " restore")
                    (newline)))
                width height)))

(define (quarter-turn-right image)
  (if (not (image? image))
      (error "argument to quarter-turn-right not an image" image))
  (make-image (lambda (xt yt)
                ((image-proc image) (lambda (x y) (xt y (- x)))
                                    (lambda (x y) (yt y (- x)))))
              (image-height image)
              (image-width image)))

(define (mirror-image image)
  (if (not (image? image))
      (error "argument to mirror-image not an image" image))
  (make-image (lambda (xt yt)
                ((image-proc image) (lambda (x y) (xt (- x) y))
                                    (lambda (x y) (yt (- x) y))))
              (image-width image)
              (image-height image)))

(define (invert image)
  (if (not (image? image))
      (error "argument to invert not an image" image))
  (make-image (lambda (xt yt)
                (let ((x0 (string-append (number->string
                                          (exact->inexact (xt -1 -1)))
                                         " "))
                      (y0 (string-append (number->string
                                          (exact->inexact (yt -1 -1)))
                                         " "))
                      (x1 (string-append (number->string
                                          (exact->inexact (xt 1 1)))
                                         " "))
                      (y1 (string-append (number->string
                                          (exact->inexact (yt 1 1)))
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
              (image-height image)))

(define (overlay image . images)
  (if (not (image? image))
      (error "argument to overlay not an image" image))
  (let ((w (image-width image))
        (h (image-height image)))
    (for-each
     (lambda (i)
       (if (not (image? i))
           (error "argument to overlay not an image" i))
       (if (not (and (= (image-width i) w)
                     (= (image-height i) h)))
           (error "Only images of equal size can be overlayed"
                  (cons image images))))
     images)
    (make-image
     (lambda (xt yt)
       (for-each
        (lambda (image) ((image-proc image) xt yt))
        (cons image images)))
     w h)))

(define (resize-image image . wh)
  (if (not (image? image))
      (error "argument to resize-image not an image" image))
  (let ((width default-image-size)
        (height default-image-size))
    (if (not (null? wh))
        (begin (set! width (car wh))
               (if (not (null? (cdr wh)))
                   (begin (set! height (cadr wh))
                          (if (not (null? (cddr wh)))
                              (error "too many argument to resize-image")))
                   (set! height width))))
    (if (not (and (integer? height)
                  (integer? width)
                  (exact? height)
                  (exact? width)
                  (> height 0)
                  (> width 0)))
        (error "illegal size specification in resize-image" wh))
    (make-image (image-proc image) width height)))

(define (stack top . rest)
  (define (stack2 top bottom)
    (if (not (image? top))
        (error "argument to stack not an image" top))
    (if (not (image? bottom))
        (error "argument to stack not an image" bottom))
    (if (not (= (image-width top) (image-width bottom)))
        (error "Attempt to stack images of different widths" (list top bottom))
        (let ((th (image-height top))
              (bh (image-height bottom)))
          (let* ((h (+ th bh))
                 (inexact-h (exact->inexact h)))
            (let ((tscale (/ th inexact-h))
                  (bscale (/ bh inexact-h)))
              (make-image
               (lambda (xt yt)
                 ((image-proc top) (lambda (x y)
                                     (xt x (+ (* tscale y) bscale)))
                                   (lambda (x y)
                                     (yt x (+ (* tscale y) bscale))))
                 ((image-proc bottom) (lambda (x y)
                                        (xt x (- (* bscale y) tscale)))
                                      (lambda (x y)
                                        (yt x (- (* bscale y) tscale)))))
               (image-width top)
               h))))))
  (let loop ((image top)
             (images rest))
    (if (null? images)
        image
        (loop (stack2 image (car images)) (cdr images)))))

(define (filled-triangle x0 y0 x1 y1 x2 y2 . wh)
  (if (not (real? x0))
      (error "x0 argument to filled-triangle not a real" x0))
  (if (not (<= -1 x0 1))
      (error "x0 argument to filled-triangle not in -1 to 1 range" x0))
  (if (not (real? x1))
      (error "x1 argument to filled-triangle not a real" x1))
  (if (not (<= -1 x1 1))
      (error "x1 argument to filled-triangle not in -1 to 1 range" x1))
  (if (not (real? x2))
      (error "x2 argument to filled-triangle not a real" x2))
  (if (not (<= -1 x2 1))
      (error "x2 argument to filled-triangle not in -1 to 1 range" x2))
  (if (not (real? y0))
      (error "y0 argument to filled-triangle not a real" y0))
  (if (not (<= -1 y0 1))
      (error "y0 argument to filled-triangle not in -1 to 1 range" y0))
  (if (not (real? y1))
      (error "y1 argument to filled-triangle not a real" y1))
  (if (not (<= -1 y1 1))
      (error "y1 argument to filled-triangle not in -1 to 1 range" y1))
  (if (not (real? y2))
      (error "y2 argument to filled-triangle not a real" y2))
  (if (not (<= -1 y2 1))
      (error "y2 argument to filled-triangle not in -1 to 1 range" y2))
  (let ((width default-image-size)
        (height default-image-size))
    (if (not (null? wh))
        (begin (set! width (car wh))
               (if (not (null? (cdr wh)))
                   (begin (set! height (cadr wh))
                          (if (not (null? (cddr wh)))
                              (error "too many argument to filled-triangle")))
                   (set! height width))))
    (if (not (and (integer? height)
                  (integer? width)
                  (exact? height)
                  (exact? width)
                  (> height 0)
                  (> width 0)))
        (error "illegal size specification in filled-triangle" wh))
    (ps-image (string-append (number->string (exact->inexact x0))
                           " " (number->string (exact->inexact y0))
                           " moveto "
                           (number->string (exact->inexact x1))
                           " " (number->string (exact->inexact y1))
                           " lineto "
                           (number->string (exact->inexact x2))
                           " " (number->string (exact->inexact y2))
                           " lineto closepath fill")
              width height)))
