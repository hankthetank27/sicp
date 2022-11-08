#lang simply-scheme
;; homework week 1

;; .2
(define (sqr x)
  (* x x))

(define (squares sen)
  (if (empty? sen)
      '()
      (se (sqr (first sen))
          (squares (bf sen)))))

;; .3
(define (switch sen)
  ;; if 'i' or 'me', become 'you'
  ;; if 'you', become 'me',
  ;; unless first word, then become 'i'

  (define (process wd num)
    (cond ((or (equal? wd 'I)
               (equal? wd 'me))
           'you)
          ((equal? wd 'you)
           (if (= num 0)
               'I
               'me))
          (else wd)))


  (define (switcher sen num)
    (if (empty? sen)
        '()
         (se (process (first sen) num)
              (switcher (bf sen) (+ num 1)))))

  (switcher sen 0))

;; .4
(define (ordered? nums)
  (define (checker prev nums)
    (if (empty? nums)
        #t
        (if (asc? prev (first nums))
            (checker (first nums)
                     (bf nums))
            #f)))
  (define (asc? prev curr)
    (or (= prev curr)
        (< prev curr)))
  
  (checker (first nums) nums))

;;5.
(define (ends-e sen)
  (define (is-e? wd)
    (equal? (last wd) 'e))
  (define (checker sen)
    (if (empty? sen)
        '()
        (se (if (is-e? (first sen))
                (first sen)
                '())
            (checker (bf sen)))))
  (checker sen))
      
  
        