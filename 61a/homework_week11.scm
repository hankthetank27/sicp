#lang sicp

;Reading: 3.5.1-3, 3.5.5

(define (stream-car s)
  (car s))

(define (stream-cdr s)
  (force (cdr s)))

(define (stream-null? s)
  (null? s))

;3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc (map stream-cdr argstreams))))))

;3.51

    (define (stream-enumerate-interval low high)
      (if (> low high)
        '()
        (cons-stream
          low
          (stream-enumerate-interval (+ low 1) high))))

    (define (stream-ref s n)
      (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

    (define (display-line x)
      (newline)
      (display x))

    (define (show x)
      (display-line x)
      x)

    (define x (stream-map show (stream-enumerate-interval 0 10)))    

    (stream-ref x 5)
    (stream-ref x 7)

    ; (stream-ref x 5) will print streams starting with cars of 1 2 3 4 5
    ; (stream-ref x 7) will print streams starting with cars of 6 7
    ; this is because the implmentation of delay is memoizing values

;3.52 
    (define (stream-for-each proc s)
      (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

    (define (stream-filter pred s)
        (cond ((stream-null? s) the-empty-stream)
              ((pred (stream-car s)) 
               (cons-stream (stream-car s)
                            (stream-filter pred 
                                           (stream-cdr s))))
              (else (stream-filter pred (stream-cdr s)))))

    (define (display-stream s)
      (stream-for-each display-line s))

    (define (display-stream-ref s n)
      (stream-ref (stream-map display-line s) n))

    (define sum 0) ; sum = 0

    (define (accum x)
       (set! sum (+ x sum))
       sum) ;sum = 0

    (define seq (stream-map accum (stream-enumerate-interval 1 20))) ;sum = 1
    (define y (stream-filter even? seq)) ;(seq memo'd 1), 3, x6. sum = 6
    (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                             seq)) ;(seq memo'd 1, 3, 6), x10. sum = 10

    (stream-ref y 7) ;(seq memo'd x6, 10), 15, 21, x28, x36, 45 ,55 ,x66 ,x78 ,91 ,105 ,x120 ,x136. sum = 136
    (display-stream z) ;(seq memo'd x10, x15, 21, 28, 36) ... x210. sum = 210
    sum

    ; if delay had not been implemented using memoization, each call to filter or
    ; map would recompute the list item based on the global variable sum, which is 
    ; being mutated inside of filter/map, thus changing the returned list from each method.

;3.53 
    ; 1, 2, 4, 8, 16... 
    ; Each element is the sum of the prior element and itself.

;3.54 
    (define (add-streams s1 s2)
      (stream-map + s1 s2))

    (define integers (cons-stream 1 
                                  (stream-map (lambda (x) (+ 1 x)) 
                                              integers)))

    (define (mul-streams s1 s2)
        (stream-map * s1 s2))

    (define factorials (cons-stream 1 (mul-streams integers factorials)))   

;3.55 
    (define (partial-sums s)
      (define res (cons-stream (stream-car s)
                               (add-streams res (stream-cdr s))))
      res)

;3.56 
    (define (scale-stream s factor)
      (stream-map (lambda (x)(* x factor)) s))

    (define (merge s1 s2)
      (cond ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
              (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (cond ((< s1car s2car)
                       (cons-stream s1car (merge (stream-cdr s1) s2)))
                      ((> s1car s2car)
                       (cons-stream s2car (merge s1 (stream-cdr s2))))
                      (else
                        (cons-stream s1car
                                     (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

    (define S (cons-stream 1 (merge (scale-stream S 2)
                                    (merge (scale-stream S 3)
                                           (scale-stream S 5)))))

    (display-stream-ref S 20)

;3.64 
(define (stream-limit s tolerance)
  (if (< (abs (- (stream-car s)
                 (stream-car (stream-cdr s))))
         tolerance)
    (stream-car (stream-cdr s))
    (stream-limit (stream-cdr s) tolerance)))

;3.66 
    (define (interleave s1 s2)
      (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

    (define (stream-append s1 s2)
      (if (stream-null? s1)
        s2
        (cons-stream (stream-car s1)
                     (stream-append (stream-cdr s1) s2))))

    (define (pairs s t)
      (cons-stream
        (list (stream-car s)(stream-car t))
        (interleave
          (stream-map (lambda (x) (list (stream-car s) x))
                      (stream-cdr t))
          (pairs (stream-cdr s) (stream-cdr t)))))

;3.67
    (define (pairs-full s t)
      (cons-stream
        (list (stream-car s)(stream-car t))
        (interleave
          (interleave
            (stream-map (lambda (x) (list (stream-car s) x))
                        (stream-cdr t))
            (stream-map (lambda (x) (list x (stream-car t)))
                        (stream-cdr s)))
          (pairs (stream-cdr s)(stream-cdr t)))))

;3.68
    ; interleave is a normal procedure, and will evaluate both arguments at runtime
    ; thus causing an infinite recursion.

;3.70
    (define (merge-weighted s1 s2 weight)
      (cond ((stream-null? s1) s2)
            ((stream-null? s2) s1)
            (else
              (let ((s1car (stream-car s1))
                    (s2car (stream-car s2)))
                (let ((s1weight (weight s1car))
                      (s2weight (weight s2car)))
                  (if (< s1weight s2weight)
                    (cons-stream s1car 
                                 (merge-weighted (stream-cdr s1) s2 weight))
                    (cons-stream s2car 
                                 (merge-weighted s1 (stream-cdr s2) weight))))))))

    (define (sum-weight p)
      (apply + p))

    (define (weighted-pairs s t weight)
      (cons-stream
        (list (stream-car s)(stream-car t))
        (merge-weighted
          (stream-map (lambda (x) (list (stream-car s) x))
                      (stream-cdr t))
          (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
          weight)))

    (display-stream-ref (weighted-pairs integers integers sum-weight) 50)

;2. Write and test two functions to manipulate nonnegative proper fractions. The first
;function, fract-stream, will take as its argument a list of two nonnegative integers, the
;numerator and the denominator, in which the numerator is less than the denominator. It
;will return an infinite stream of decimal digits representing the decimal expansion of the
;fraction. The second function, approximation, will take two arguments: a fraction stream
;and a nonnegative integer numdigits. It will return a list (not a stream) containing the
;first numdigits digits of the decimal expansion.
;(fract-stream ’(1 7)) should return the stream representing the decimal expansion of
;1/7 , which is 0.142857142857142857...
;(stream-car (fract-stream ’(1 7))) should return 1.
;(stream-car (stream-cdr (stream-cdr (fract-stream ’(1 7))))) should return 2.
;(approximation (fract-stream ’(1 7)) 4) should return (1 4 2 8).
;(approximation (fract-stream ’(1 2)) 4) should return (5 0 0 0).

;~~~~ Extra for experts ~~~~~

;3.59

;3.60

;3.61

;3.62

;----
;2. Consider this procedure:
;(define (hanoi-stream n)
;  (if (= n 0)
;    the-empty-stream
;    (stream-append (hanoi-stream (- n 1))
;                   (cons-stream n (hanoi-stream (- n 1))))))
;It generates finite streams; here are the first few values:
;(hanoi-stream 1) (1)
;(hanoi-stream 2) (1 2 1)
;(hanoi-stream 3) (1 2 1 3 1 2 1)
;(hanoi-stream 4) (1 2 1 3 1 2 1 4 1 2 1 3 1 2 1)
;Notice that each of these starts with the same values as the one above it, followed by some
;more values. There is no reason why this pattern can’t be continued to generate an infinite
;stream whose first 2n − 1 elements are (hanoi-stream n). Generate this stream.
