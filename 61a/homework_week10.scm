#lang sicp
(load "parallel-execute-and-serializer")

; 3.38
    ; A. 35, 40, 45, 50
    ; B. It could be any single transaction, ie. 110, 80, 50, or any combination of two transactions, ie. 90, 40, 

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

; 3.44

; 3.46

; 3.48


; -------------------------------
; EXTRA FOR EXPERTS
; read 3.3.5

; 3.33

; 3.34

; 3.35

; 3.36

; 3.37
