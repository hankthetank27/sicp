#lang sicp

;;random notes and helpers
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


;3.16
;;drawn in notebook

;3.17
;wrote and used before discovering memq
#;(define (includes? target list)
  (cond ((null? list) #f)
        ((eq? target list) #t)
        (else (includes? target (cdr list)))))

(define (count-pairs x)
  (let ((seen '()))
    (define (count x)
      (cond ((memq x seen) 0)
            ((not (pair? x)) 0)
            (else
             (begin (set! seen (cons x seen))
                    (+ (count (car x))
                       (count (cdr x))
                       1)))))
    (count x)))

;3.18
(define (has-cycle? x)
  (let ((seen '()))
    (define (detect x)
      (cond ((null? x) #f)
            ((memq x seen) #t)
            (else (begin (set! seen (cons x seen))
                         (detect (cdr x))))))
    (detect x)))

(has-cycle? (list 2 2 2 2 2))
(has-cycle? (make-cycle (list 2 2 2 2 2)))
(has-cycle? (make-cycle (list 1 2 3 4 5)))

;3.19
(define (has-cycle-const? x)
  (define (next x)
    (if (null? x) x (cdr x)))
  (define (detect s f)
    (let ((slow-ptr (next s))
          (fast-ptr (next (next f))))   
      (cond ((or (null? slow-ptr)
                 (null? fast-ptr)) #f)
            ((eq? slow-ptr fast-ptr) #t)
            (else (detect slow-ptr fast-ptr)))))
  (detect x x))
          
(has-cycle-const? (list 2 2 2 2 2))
(has-cycle-const? (make-cycle (list 2 2 2 2 2)))
(has-cycle-const? (make-cycle (list 1 2 3 4 5)))

;3.20
;drawn in notebook

;3.21




                            