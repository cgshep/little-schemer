;; The Little Schemer commandments:
;; 1. The first condition when recursing through a list should be (null? lat)
;; 2. Use 'cons' to build lists
;; 3. When building a list, describe the first typical element and cons it onto the recursion
;; 4. Always change at least one argument when recurring.  It must be changed to be closer to termination.
;;    The changing argument must be tested in the termination condition;
;;    for example, when using cdr, test termination with null?

(define atom?
  (lambda (x)
    (and
     (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

;; Rember: removes first occurrence of a member from a list
(define rember
  (lambda (a lat)
    (cond
     ((null? lat)          (quote ()))
     ((eq? (car lat) a)    (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

;; Firsts: constructs a list containing the first elements of each internal list
(define firsts
  (lambda (l)
    (cond
     ((null? l)           (quote ()))
     (else (cons (car (car l))
		 (firsts (cdr l)))))))

;; insertR: Inserts the element 'new' to the right of 'old' in list 'lat'
;; new: topping, old: fudge, lat: (ice cream with fudge for desert)
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old)
	(cons old
	      (cons new (cdr lat))))
       (else (cons (car lat)
		   (insertR new old
			    (cdr lat)))))))))

;; insertL: Inserts the element 'new' to the left of 'old' in list 'lat'
;; Page 51
(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat)
		   (insertR new old
			    (cdr lat)))))))))

;; multirember: Removes all occurrences of an element from list 'lat'
;; Page 53
(define multirember
  (lambda (a lat)
    (cond
     ((eq? (car lat) a) (multirember a (cdr lat)))
     (else (cons (car lat)
		 (multirember a (cdr lat)))))))

;; multiinsertR: Inserts element 'new' to the right of every occurrence of 'old' in list 'lat'
;; Page 56
(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old)
	(cons (car lat)
	      (cons new
		    (multiinsertR new old (cdr lat)))))
       (else (cons (car lat)
		   (multiinsertR new old (cdr lat)))))))))

;; multiinsertL: Inserts element 'new' to the left of every occurrence of 'old' in list 'lat'
;; Page 57
(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old)
	(cons new
	      ;; 'old' is equivalent to (car lat) in this case
	      (cons old 
			(multiinsertL new old (cdr lat)))))
       (else (cons (car lat)
		   (multiinsertL new old (cdr lat)))))))))

;; multisubst: Replaces all occurrences of element 'old' in list 'lat' with 'new'
;; Page 57
(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) old)
	     (cons new
		   (multisubst new old (cdr lat))))
	    (else (cons (car lat)
			(multisubst new old (cdr lat)))))))))
