#lang simply-scheme

;1.

;;2.25
(define l1
  (list 1 3 (list 5 7) 9))
(car (cdr (car (cdr (cdr l1)))))

(define l2
  (list (list 7)))
(car (car l2))

(define l3
  (list 1(list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car
 (cdr
  (car
   (cdr
    (car
     (cdr
      (car
       (cdr
        (car
         (cdr
          (car
           (cdr l3)
           )))))))))))

;;2.53
(define false #f)
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))
  ;;; (a b c)
  ;;; ((george))
  ;;; '((y1 y2))
  ;;; '(y1 y2)
  ;;; false
  ;;; false
  ;;; '(red shoes blue socks)


;;2.27
(define (deep-reverse list)
  (define (rev init res)
    (cond ((null? init) res)
          ((pair? (car init))
             (rev (cdr init)
                  (cons (rev (car init) '())
                        res)))
          (else (rev (cdr init)
                     (cons (car init) res)))))
  (rev list '()))
