#lang sicp
(load "parallel-execute-and-serializer")

; 3.38
    ; A. 35, 40, 45, 50
    ; B. It could be any single transaction, ie. 110, 80, 50, or any combination 
    ; of two transactions, ie. 90, 40, 

; 3.39 
    ; 121, 100, 101, 11

; 3.40
    ; p1[x1 = 10, x2 = 10]
    ; p1[x1 = 10, x2 = 1000]
    ; p1[x1 = 1000, x2 = 1000]
    ; p2[x1 = 10, x2 = 10, x3 = 10] 
    ; p2[x1 = 10, x2 = 10, x3 = 100]
    ; p2[x1 = 10, x2 = 100, x3 = 100]
    ; p2[x1 = 100, x2 = 100, x3 = 100]
    
    ;100, 1000, 10,000, 100,000, 1,000,000 
    ;if serialized, only 1,000,000

; 3.41
    ; I cannot see a use for it in this case. I could see it being useful for 
    ; proccedures where there are serveral opperations involved where the value
    ; would mutate.

; 3.42
    ; I belive this would work. Each function call will be wrapped in the same serializer.

; 3.33
    ; Without serializtion, a1 can be acessed at the same inital value in both
    ; proccedures, allowing it to be swapped concurrently resulting in the same
    ; value appearing twice (diagram in notepad).

; 3.44
    ; With a serializer on each proccedure it should be fine. Since the opperations
    ; do not depend on eachothers inital value, the calculations can be done
    ; independently. Eg. in the exchange problem the state of one account depends
    ; on the state of another account (they are coupled).

; 3.45
    ; This will result in deadlock as the same serializer used on to wrap the exchange
    ; proccedure is being used within the exchange proccedure on the withdraw and 
    ; deposit methods.

; 3.46
    ; Diagram in notebook...
    ; Suppose we have a mutex with cell (list false), and both Peter and Paul attempt to acquire it. The following may happen if test-and-set! has *not* been made atomic using without-interrupts:
    ; 1. Peter calls test-and-set!, (car cell) is false.
    ; 2. Paul *also* calls test-and-set!, *also* finds (car cell) to be false, since Peter's test-and-set! has not completed yet.
    ; 3. Peter sets (car cell) to true.
    ; 4. Paul *also* sets (car cell) to true.
    ;Here, both Peter and Paul have acquired the mutex, which is bad. 

; 3.47
    (define (test-and-set! cell)
      (if (car cell)
        true
        (begin (set-car! cell true)
               false)))

    (define (clear cell)
      (set-car! cell false))

    (define (make-mutex)
      (let (cell (list false))
        (define (the-mutex m)
          (cond ((eq? m 'acquire)
                 (if (test-and-set! cell)
                   (the-mutex 'acquire)))
                ((eq? m 'release)
                 (clear cell))))
        the-mutex))
        
    (define (make-semaphore size)
      (let ((procs 0)
            (mutex (make-mutex)))
        (define (semaphore m)
          (cond ((eq? m 'acquire)
                 (mutex 'acquire)
                 (if (< size procs)
                   (begin (set! procs (+ 1 procs))
                          (mutex 'release))
                   (begin (mutex 'release))(semaphore 'acquire))))
          (cond ((eq? m 'release)
                 (mutex 'acquire)
                 (set! procs (- procs 1))
                 (mutex 'release))))
        semaphore))
                        
; 3.48
    (define (serialized-exchange acct1 acct2)
      (let ((serializer1 (acct1 'serializer))
            (serializer2 (acct2 'serializer)))
          (define (exchangeer s1 s2)
            (s1 (s2 (exchange acct1 acct2))))
          (if (< (acct1 'id)(acct2 'id))
            (exchangeer (serializer1 serializer2))
            (exchangeer (serializer2 serializer1)))))


; -------------------------------
; EXTRA FOR EXPERTS
; read 3.3.5

; 3.33

; 3.34

; 3.35

; 3.36

; 3.37
