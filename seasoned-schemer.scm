
;; atom?: Determines whether the argument is an atom
;; Preliminary
(define atom?
  (lambda (x)
    (and (not (pair? x))
	 (not (null? x)))))

;; two-in-a-row?: Determines whether two atoms appear sequentially in a list
;; Page 6
(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (or (is-first-b? (car lat) (cdr lat))))))

;; is-first-b?: Helper function to two-in-a-row?
;; Page 6
(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (two-in-a-row? lat))))))

;; two-in-a-row-b?: Alternate version of two-in-a-row?
;; Page 7
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) preceding)
	       (two-in-a-row-b? (car lat) (cdr lat)))))))

;; two-in-a-row-c: Final version of two-in-a-row?
;; Page 7
(define two-in-a-row-c
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else two-in-a-row-b? (car lat) (cdr lat)))))

