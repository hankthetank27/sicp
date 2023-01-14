#lang simply-scheme


;2.29
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))


(define mob (make-mobile
             (make-branch 2 (make-mobile (make-branch 4 3)
                                         (make-branch 4 3)))
             (make-branch 6 10)))

(branch-structure (left-branch mob))
(branch-length (right-branch mob))

;b.

(define (total-weight mobile)
  (define (dfs branch)
    (if (pair? (branch-structure branch))
        (total-weight (branch-structure branch))
        (branch-structure branch)))
  (+ (dfs (left-branch mobile))
     (dfs (right-branch mobile))))

(define (total-length mobile)
  (define (dfs branch)
    (if (pair? (branch-structure branch))
        (+ (branch-length branch)
           (total-length (branch-structure branch)))
        (branch-length branch)))
  (+ (dfs (left-branch mobile))
     (dfs (right-branch mobile))))

;c.

(define (mobile-balanced? mobile)
  (define (toruqe branch)
    (if (pair? (branch-structure branch))
        (* (total-weight (branch-structure branch))
           (+ (total-length (branch-structure branch))
              (branch-length branch)))
        (* (branch-structure branch)(branch-length branch))))
  (= (toruqe (left-branch mobile))
     (toruqe (right-branch mobile))))


          