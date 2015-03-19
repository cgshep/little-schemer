;; Little schemer commandments:
;; 1. The first condition when recursing through a list should be (null? lat)
;; 2. Use 'cons' to build lists
;; 3. When building a list, describe the first typical element and cons it onto the recursion

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

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

;; insertR: Inserts the element 'new' to the right of element 'old' in list 'lat'
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) old) (cdr lat))
       (else (cons (car lat)
		   (insertR new old
			    (cdr lat)))))))))
