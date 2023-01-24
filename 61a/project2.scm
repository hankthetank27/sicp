#lang sicp
(#%require sicp-pict)

;; Code for CS61A project 2 -- picture language

;;2.44
;(define (up-split painter n)      #redefined in ex 2.45
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))

;;2.45
(define (split direction split)
  (define (proc painter n)
    (if (= n 0)
        painter
        (let ((smaller (proc painter (- n 1))))
          (direction painter (split smaller smaller)))))
  proc)

(define up-split (split below beside))
(define right-split (split beside below))

;;2.46
(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (comb-vect op)
  (lambda (v1 v2)
    (make-vect (op (xcor-vect v1)
                   (xcor-vect v2))
               (op (ycor-vect v1)
                   (ycor-vect v2)))))

(define add-vect (comb-vect +))
(define sub-vect (comb-vect -))
(define (scale-vect scale vect)
  (make-vect (* scale (xcor-vect vect))
             (* scale (ycor-vect vect))))

;;2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (caddr frame))

;(define frame (make-frame (make-vect 0 0)
;                          (make-vect 1 2)
;                          (make-vect 3 4)))
;(origin-frame frame)
;(edge1-frame frame)
;(edge2-frame frame)

;2.48
(define (make-segment origin start-vect end-vect)
  (cons start-vect end-vect))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

;2.49
;; a)
(define outline
  ((segments->painter
    (list (make-segment (make-vect 0.0 0.0)(make-vect 0.0 1.0))
          (make-segment (make-vect 0.0 1.0)(make-vect 1.0 1.0))
          (make-segment (make-vect 1.0 1.0)(make-vect 1.0 0.0))
          (make-segment (make-vect 1.0 0.0)(make-vect 0.0 0.0))))
   (make-frame 0.0 1.0 1.0)))

;; b)
(define x-drawing
  ((segments->painter
    (list (make-segment (make-vect 0.0 1.0)(make-vect 1.0 0.0))
          (make-segment (make-vect 1.1 1.1)(make-vect 0.0 0.0))))
   (make-frame 0.0 1.0 1.0)))

;; c)
(define diamond
  ((segments->painter
    (list (make-segment (make-vect 0.0 0.5)(make-vect 0.5 1.0))
          (make-segment (make-vect 0.5 1.0)(make-vect 1.0 0.5))
          (make-segment (make-vect 1.0 0.5)(make-vect 0.5 0.0))
          (make-segment (make-vect 0.5 0.0)(make-vect 0.0 0.5))))
   (make-frame 0.0 1.0 1.0)))

;2.50
(define (flip-horiz painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 0.0 0.0)
		     (make-vect 1.0 1.0)))


;; procs from book ------

;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert painter))))
;    (below painter2 painter2)))

;(define (right-split painter n)       #redefined in ex 2.45
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;	(beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
	    (right (right-split painter (- n 1))))
	(let ((top-left (beside up up))
	      (bottom-right (below right right))
	      (corner (corner-split painter (- n 1))))
	  (beside (below painter top-left)
		  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
	  (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (identity x) x)

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
				  identity flip-vert)))
    (combine4 painter)))

;; or

; (define flipped-pairs
;   (square-of-four identity flip-vert identity flip-vert))

;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity
;				  rotate180 flip-vert)))
;    (combine4 (corner-split painter n))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
			   (edge1-frame frame))
	       (scale-vect (ycor-vect v)
			   (edge2-frame frame))))))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
	((frame-coord-map frame) (start-segment segment))
	((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define (draw-line v1 v2)
  (penup)
  (setxy (- (* (xcor-vect v1) 200) 100)
	 (- (* (ycor-vect v1) 200) 100))
  (pendown)
  (setxy (- (* (xcor-vect v2) 200) 100)
	 (- (* (ycor-vect v2) 200) 100)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
	(painter
	 (make-frame new-origin
		     (sub-vect (m corner1) new-origin)
		     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
		     (make-vect 0.0 1.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
		    (make-vect 0.5 0.5)
		    (make-vect 1.0 0.5)
		    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
		     (make-vect 1.0 0.0)
		     (make-vect 1.0 1.0)
		     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
		     (make-vect 0.0 0.0)
		     (make-vect 0.65 0.35)
		     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
	   (transform-painter painter1
			      (make-vect 0.0 0.0)
			      split-point
			      (make-vect 0.0 1.0)))
	  (paint-right
	   (transform-painter painter2
			      split-point
			      (make-vect 1.0 0.0)
			      (make-vect 0.5 1.0))))
      (lambda (frame)
	(paint-left frame)
	(paint-right frame)))))

(define full-frame (make-frame (make-vect -0.5 -0.5)
			       (make-vect 2 0)
			       (make-vect 0 2)))
