#lang simply-scheme

(define (twenty-one strategy)
  (define (play-dealer customer-hand dealer-hand-so-far rest-of-deck)
    (cond ((> (best-total dealer-hand-so-far) 21) 1)
	  ((< (best-total dealer-hand-so-far) 17)
	   (play-dealer customer-hand
			(se dealer-hand-so-far (first rest-of-deck))
			(bf rest-of-deck)))
	  ((< (best-total customer-hand) (best-total dealer-hand-so-far)) -1)
	  ((= (best-total customer-hand) (best-total dealer-hand-so-far)) 0)
	  (else 1)))

  (define (play-customer customer-hand-so-far dealer-up-card rest-of-deck)
    (cond ((> (best-total customer-hand-so-far) 21) -1)
	  ((strategy customer-hand-so-far dealer-up-card)
	   (play-customer (se customer-hand-so-far (first rest-of-deck))
			  dealer-up-card
			  (bf rest-of-deck)))
	  (else
	   (play-dealer customer-hand-so-far
			(se dealer-up-card (first rest-of-deck))
			(bf rest-of-deck)))))

  (let ((deck (make-deck)))
    (play-customer (se (first deck) (first (bf deck)))
		   (first (bf (bf deck)))
		   (bf (bf (bf deck))))) )

(define (make-ordered-deck)
  (define (make-suit s)
    (every (lambda (rank) (word rank s)) '(A 2 3 4 5 6 7 8 9 10 J Q K)) )
  (se (make-suit 'H) (make-suit 'S) (make-suit 'D) (make-suit 'C)) )

(define (make-deck)
  (define (shuffle deck size)
    (define (move-card in out which)
      (if (= which 0)
	  (se (first in) (shuffle (se (bf in) out) (- size 1)))
	  (move-card (bf in) (se (first in) out) (- which 1)) ))
    (if (= size 0)
	deck
    	(move-card deck '() (random size)) ))
  (shuffle (make-ordered-deck) 52) )

(define (best-total hand)
  (define (count-hand hand sum a)
    (let ((card (if (empty? hand)
                   '()
                   (bl (first hand)))))
      (cond ((empty? hand) (make-total sum a))
            ((equal? card 'A) (count-hand (bf hand) sum (+ 1 a)))
            ((or (equal? card 'J)
                 (equal? card 'Q)
                 (equal? card 'K))
             (count-hand (bf hand) (+ 10 sum) a))
            (else (count-hand (bf hand) (+ card sum) a))) ))
  (define (make-total sum a)
    (cond ((= a 0) sum)
          ((> (+ sum 11) 21)
           (make-total (+ sum 1) (- a 1)))
          (else
           (make-total (+ sum 11) (- a 1)))))
  (count-hand hand 0 0))

(define (stop-at-17 hand dealer)
  (if (< (best-total (se hand dealer)) 17)
      #t #f))

(define (play-n strategy n)
  (if (= n 0)
      0
      (+ (play-n strategy (- n 1))
         (twenty-one strategy))))

(define (dealer-sensitive hand dealer)
  (define (check-dealer needed dealer)
    (cond ((empty? dealer) #f)
          ((member? (first dealer) needed) #t)
          (else (check-dealer needed (bf dealer)))))
        
  (cond ((and (< (best-total hand) 17)
              (check-dealer '(7 8 9 10 J Q K A) dealer))
         #t)
        ((and (< (best-total hand) 12)
              (check-dealer '(2 3 4 5 6) dealer))
         #t)
        (else #f)))

(define (stop-at n)
  (lambda (hand dealer)
    (if (< (best-total (se hand dealer)) n)
        #t #f)))

(define (valentine hand dealer)
  (define (has-heart? hand)
    (cond ((empty? hand) #f)
          ((equal? (last (first hand)) 'H) #t)
          (else (has-heart? (bf hand)))))
  (if (has-heart? hand)
      ((stop-at 19) hand dealer)
      ((stop-at 17) hand dealer)))

(define (suit-strategy suit strat-if-has strat-if-not)
  (lambda (hand dealer)
    (define (has-suit? hand)
      (cond ((empty? hand) #f)
            ((equal? (last (first hand)) suit) #t)
            (else (has-suit? (bf hand)))))
    (if (has-suit? hand)
        (strat-if-has hand dealer)
        (strat-if-not hand dealer))))


(define (valentine-2 hand dealer)
  ((suit-strategy 'H (stop-at 19) (stop-at 17)) hand dealer))



      
  