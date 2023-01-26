#lang simply-scheme

;2.74

;each div personal records are a set of records keyd on employee name 
;;strucutre of set varies between div
;;each record is a set (unique struct for each div) that containers info keyed under ids "address", "salary"

;;A.
;we are assuming each div file has a type tag with name
;exp of div record: ('name-of-div (('bill (records)) ('jane (records)) ('jamie (records))))

;dept install:
;(define (install-research-div)
;   (define (get-records emp-name file).......)
;   (put 'get-record 'research get-record)

;HQ get:
;(define (get-record employee file)
;  (attach-tag (type-tag file)
;              ((get 'get-record (type-tage file)) employee (contents file)))

;;B.
;dept install:
;(define (install-research-div)
;  (define (get-salary record)....)
;  (put 'get-salaray 'reseach get-salary)

;HQ get:
;(define (get-salary employee-record)
;  ((get 'get-salaray (type-tage employee-record)) (contents employee-record)))

;;C.
;(define (find-employee-record emp-name div-files)
;  (if (null? div-files)
;      #f
;      (let ((emp-records (get-records emp-name (car div-files)))
;        (if (contents emp-records)
;             emp-records
;            (find-employee-records emp-name (cdr div-files)))))


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
