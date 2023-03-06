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
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE called with empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(define (print-queue queue)
  (display (front-ptr queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)


;3.22
(define (make-q)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (dispatch m)
      (cond ((eq? m 'front-queue)
             (lambda ()
               (if (empty-queue?)
                   (error "FRONT called with empty queue"))))
            ((eq? m 'insert-queue!)
             (lambda (item)
               (let ((new-pair (cons item '())))
                 (cond ((empty-queue?)
                        (set-front-ptr! new-pair)
                        (set-rear-ptr! new-pair)
                        front-ptr)
                       (else
                        (set-cdr! rear-ptr new-pair)
                        (set-rear-ptr! new-pair)
                        front-ptr)))))
            ((eq? m 'delete-queue!)
             (lambda ()
               (cond ((empty-queue?)
                      (error "DELETE called with empty queue"))
                     (else
                      (set-front-ptr! (cdr front-ptr))
                      front-ptr))))
            (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

(define q2 (make-q))
((q2 'insert-queue!) 'a)
((q2 'insert-queue!) 'b)
((q2 'insert-queue!) 'c)
((q2 'delete-queue!))
((q2 'delete-queue!))
((q2 'delete-queue!))


;3.23
(define (empty-dequeue? dequeue)
  (and (null? (front-ptr dequeue))
       (null? (rear-ptr dequeue))))

(define (make-dequeue) (cons '() '()))

(define (make-dequeue-node item)
  (cons (cons item '()) '()))

(define (get-item dequeue end)
  (if (empty-dequeue? dequeue)
      (error "GET called with an empty queue" dequeue)
      (caar (end dequeue))))

(define (front-dequeue dequeue)
  (get-item dequeue front-ptr))

(define (rear-dequeue dequeue)
  (get-item dequeue rear-ptr))

(define (front-insert-dequeue! dequeue item)
  (let ((new-node (make-dequeue-node item)))
    (cond ((empty-dequeue? dequeue)
           (set-front-ptr! dequeue new-node)
           (set-rear-ptr! dequeue new-node))
          (else
           (set-cdr! new-node (front-ptr dequeue))
           (set-cdr! (car (front-ptr dequeue)) new-node)
           (set-front-ptr! dequeue new-node)))))

(define (rear-insert-dequeue! dequeue item)
  (let ((new-node (make-dequeue-node item)))
    (cond ((empty-dequeue? dequeue)
           (set-front-ptr! dequeue new-node)
           (set-rear-ptr! dequeue new-node))
          (else
           (set-cdr! (rear-ptr dequeue) new-node)
           (set-cdr! (car new-node) (rear-ptr dequeue))
           (set-rear-ptr! dequeue new-node)))))

(define (delete-front-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "DELETE called with empty queue" dequeue))
        (else
         (begin
           (set-front-ptr! dequeue (cdr (front-ptr dequeue)))
           (set-cdr! (car (front-ptr dequeue)) '())))))

(define (delete-rear-dequeue! dequeue)
  (cond ((empty-dequeue? dequeue)
         (error "DELETE called with empty queue" dequeue))
        (else
         (let ((prev (cdr (car (rear-ptr dequeue)))))
           (set-cdr! prev '())
           (set-rear-ptr! dequeue prev)))))
               

(define (print-dequeue dequeue)
  (map (lambda (x) (car x)) (front-ptr dequeue)))


(define dq (make-dequeue))
(rear-insert-dequeue! dq 'a)
(rear-insert-dequeue! dq 'b)
(rear-insert-dequeue! dq 'c)
(front-insert-dequeue! dq '1)
(front-insert-dequeue! dq '2)
(front-insert-dequeue! dq '3)
(delete-front-dequeue! dq)
(delete-front-dequeue! dq)
(delete-rear-dequeue! dq)
(delete-rear-dequeue! dq)
(front-insert-dequeue! dq '3)
(rear-insert-dequeue! dq 'b)

(print-dequeue dq)
  



