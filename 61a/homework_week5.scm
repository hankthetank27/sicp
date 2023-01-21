#lang simply-scheme


;2.24
   ;; '(1 (2 (3 4)))

;2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
   ;; (1 2 3 4 5 6)
   ;; ((1 2 3) 4 5 6)
   ;; ((1 2 3)(4 5 6))

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

;2.36
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

;2.38
; 1.5
; 1/6
; 3 2 1
; 1 2 3
; +


;2.54

(define (equal? x y)
  (cond ((and (null? x)(null? y)) #t)
        ((and (pair? (car x))(pair? (car y)))
         (if (equal? (car x)(car y))
             (equal? (cdr x)(cdr y))
             #f))
        ((eq? (car x)(car y))
         (equal? (cdr x)(cdr y)))
        (else #f)))

(equal? '(this is a list)'(this is a list))
(equal? '(this '(is a) list)'(this '(is a) list))
(equal? '(this is a list)'(this '(is a) list))


;2.67
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (left-branch-h tree) (car tree))

(define (right-branch-h tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch-h branch))
        ((= bit 1) (right-branch-h branch))
        (else (error "bad bit"))))


(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))


(define (encode-symbol symbol tree)
  (cond ((leaf? tree) '())
        ((member symbol (symbols (left-branch-h tree)))
         (cons 0
               (encode-symbol symbol (left-branch-h tree))))
        ((member symbol (symbols (right-branch-h tree)))
         (cons 1
               (encode-symbol symbol (right-branch-h tree))))))

(encode '(A D A B B C A) sample-tree)

;2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)   ;symbol
                               (cadr pair)) ;freq
                    (make-leaf-set (cdr pairs))))))

(define (successive-merge leaf-set)
  (define (merge set res)
    (if (null? set)
        res
        (merge (cdr set)
               (make-code-tree (car set) res))))
  (merge (cdr leaf-set) (car leaf-set)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define encoded-word (encode '(A D A B B C A)
        (generate-huffman-tree '((D 1)(A 4)(C 1)(B 2)))))

(decode encoded-word
        (generate-huffman-tree '((D 1)(A 4)(C 1)(B 2))))

;2.70

(define alphabet (generate-huffman-tree
                  '((A 2)(BOOM 1)(GET 2)(JOB 2)(NA 16)(SHA 3)(YIP 9)(WAH 1))))
(encode '(GET A JOB) alphabet)
(encode '(SHA NA NA NA NA NA NA NA NA NA) alphabet)
;4 x amount of chars in string

;2.71
;			(ABCDE) 31
;			/	 \
;		       /	  \
;		   (ABCD) 15     E 16
;		  /       \
;		 /	   \
;	     (ABC) 7       D 8
;	     /      \
;	    /	     \
;	(AB) 3	    C 4
;	/     \
;      /       \
;      A 1      B 2
; 1
; n-1
         
