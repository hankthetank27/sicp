#lang simply-scheme

;2.74

;each div personal records are a set of records keyd on employee name 
;;strucutre of set varies between div
;;each record is a set (unique struct for each div) that containers info keyed under ids "address", "salary"

;;A.
;we are assuming each div file has a type tag with name
;exp of div record:
'('name-of-div (('bill (records)) ('jane (records)) ('jamie (records))))

;dept install:
(define (install-research-div)
   (define (get-records emp-name file)(file))
   (put 'get-record 'research get-record))

;HQ get:
(define (get-record employee file)
  (attach-tag (type-tag file)
              ((get 'get-record (type-tage file)) employee (contents file))))

;;B.
;dept install:
(define (install-research-div-2)
  (define (get-salary record)....)
  (put 'get-salaray 'reseach get-salary))

;HQ get:
;(define (get-salary employee-record)
;  ((get 'get-salaray (type-tage employee-record)) (contents employee-record)))

;;C.
(define (find-employee-record emp-name div-files)
  (if (null? div-files)
      #f
      (let ((emp-records (get-records emp-name (car div-files)))
        (if (contents emp-records)
             emp-records
            (find-employee-records emp-name (cdr div-files)))))))


;;D.
;create install file according above spec

;;2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          (else (error "unknown OP" op))))
  dispatch)

;2.76

;;Explicit dispatch -- requires modifiying the procedure definitions themseleves to add new cases depending on the tag name.
;;Data-directed -- requires putting new procedues into table to later get based on type and functionality.
;;Message-passing -- requires creating new data objects as each new data type is constructor made.

;;message-passing is better for adding new types because you can define the type with its associated functionality discretely.
;;Data-directed is better for adding new operations because you simply add the operations for each type to the table.

;2.77
;without the put operations listed in the book for this exersize, there is no reference to what type of complex number
;we are looking for (or reference to any magnitude procedure). Once we add said put procs, when magnitude is applied to the
;object, we can check the "sub" type tag, see that it is rectange, and call both real-part and imag-part to calculate the
;magnitude, as per the installed procedures are defined for a rectangle type. When apply-generic is called on type complex number,
;with 'magnitude, it will lookup, and apply magnitude to the 'contents' of the object, which is the rectangle obj.
;
;(define (magnitude z) (apply-generic 'magnitude z))
;
; (magnitude obj: complex) -> (magnitude sub-obj: rectange) -> res
; 2 calls to apply generic total.

;2.79
(define (equ? x y) (apply-generic 'equ? x y))

;install to ordinary
(define (equ-ord? x y) (= x y))

(put 'equ? '(scheme-number scheme-numer)
     (lambda (x y) (tag (equ-ord? x y))))

;install to rat
(define (equ-rat? x y)
  (and (= (* (denom x)(numer y)))
       (= (* (numer x)(denom y)))))
  
(put 'equ? '(rational rational)
     (lambda (x y) (tag (equ-rat? x y))))

;install to complex
(define (equ-complex? x y)
  (and (= (real-part x)(real-part y))
       (= (imag-part x)(image-part y))))

(put 'equ? '(complex complex)
     (lambda (x y) (tag (equ-complex x y)))) 

;2.80
(define (=zero? x) (apply-generic '=zero x))

(put '=zero '(scheme-number)
     (lambda (x)(= 0 x)))

(put '=zero '(rational)
     (lambda (x)(=zero? (numer x)))) ;type-recurisve

(put '=zero '(complex)
     (lambda (x)                     ;type-recursive
       (and (=zero? (real-part x))
            (=zero? (imag-part x)))))

;2.81
  ;1. apply-generic will infinitely recurse, as it passes the same two types back into apply-generic upon coersion.
  ;2. it will throw an error after failing to lookup both coersions, then checking them in both conditions where apply-generic is recursed.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error "No method for these types")
                    (let ((t1->t2 
                           (get-coercion type1
                                         type2))
                          (t2->t1 
                           (get-coercion type2 
                                         type1)))
                      (cond (t1->t2
                             (apply-generic 
                              op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic 
                              op a1 (t2->t1 a2)))
                            (else
                             (error 
                              "No method for 
                           these types"
                              (list 
                               op 
                               type-tags))))))
              (error 
               "No method for these types"
               (list op type-tags))))))))

;2.83
(define (raise x)
  (apply-generic 'raise x))

(put 'raise '(scheme-number)
     (lambda (x) (make-rational x 1)))
(put 'raise '(rational)
     (lambda (x) (make-real (/ (numer x)
                               (denom x)))))
(put 'raise '(real)
     (lambda (x) (make-from-real-img x 0)))

;2.84
(define (get-coercion t1 t2)
  (define (find-tree-level start target level)
    (let ((type-s (type start))
          (type-t (type target)))
      (cond ((eq? type-s type-t) level)
            ((eq? type-s 'complex) #f)
            (else
             (find-tree-level (raise start)
                              target
                              (+ 1 level))))))
  (let ((depth (find-tree-level t1 t2 0)))
    (define (raise-depth n level)
      (if (= 0 level)
          n
          (raise-depth (raise n)
                       (- level 1))))
    (lambda (n) (rasie-depth n depth))))
