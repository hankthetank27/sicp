#lang simply-scheme


<<<<<<< HEAD
;2.24
   ;; '(1 (2 (3 4)))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
   ;; (1 2 3 4 5 6)
   ;; ((1 2 3) 4 5 6)
   ;; ((1 2 3)(4 5 6))

;2.29

=======
;2.29
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;a.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))


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
    (* (total-weight (branch-structure branch))(branch-length branch)))
  (and (= (toruqe (left-branch mobile))
          (toruqe (right-branch mobile)))
       (mobile-balanced? (branch-structure (left-branch mobile)))
       (mobile-balanced? (branch-structure (right-branch mobile)))))

;2.30
(define (square-tree-map tree)
  (map (lambda (el)
         (if (pair? el)
             (square-tree el)
             (* el el)))
       tree))

(define (square-tree tree)
  (cond ((null? tree) '())
        ((pair? tree)
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))
        (else (* tree tree))))
                
         
(square-tree-map
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;2.31
(define (tree-map proc tree)
  (map (lambda (el)
         (if (pair? el)
             (tree-map proc el)
             
             (proc el)))
       tree))

(tree-map (lambda (x) (* x x))
           (list 1
                 (list 2 (list 3 4) 5)
                 (list 6 7)))

;2.32

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))

(subsets (list 1 2 3))

;2.37
(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x)
                                       (car x)) seqs))
            (accumulate-n op init (map (lambda (x)
                                         (cdr x)) seqs)))))

(accumulate-n +
              0
              (list (list 1 2 3)
                    (list 4 5 6)
                    (list 7 8 9)
                    (list 10 11 12)))


  
          
>>>>>>> origin/main
