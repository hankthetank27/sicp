#lang sicp

;;random notes
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
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
             (begin (set! seen (cons seen x))
                    (+ (count (car x))
                       (count (cdr x))
                       1)))))
    (count x)))




                            