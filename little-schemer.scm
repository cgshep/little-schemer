
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
     ((null? lat) (quote ()))
     (else
      (cond
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat)
		 (multirember a (cdr lat)))))))))

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

;; add1: Returns the value n+1 for some n
;; Page 59
(define add1
  (lambda (n)
    (+ n 1)))

;; sub1: Returns the value n-1 for some n
;; Page 59
(define sub1
  (lambda (n)
    (- n 1)))

;; ++: Rewrites addition for non-negative integers
;; Page 60
(define ++
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (++ n (sub1 m)))))))

;; --: Rewrites substraction for non-negative integers
;; Page 61
(define --
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (-- n (sub1 m)))))))

;; addtup: Totals the numbers in tuple 'tup'
;; Introduced on page 62; formalised on page 64
(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     ;; Let's use ++ for fun
     (else (++ (car tup) (addtup (cdr tup)))))))

;; **: Rewrites multiplication for non-negative integers
;; Page 65
(define **
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (++ n (** n (sub1 m)))))))

;; tup+: Adds the elements of tup1 to tup2 at each position
;; Page 69
(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (+ (car tup1) (car tup2))
	    (tup+ (cdr tup1) (cdr tup2)))))))

;; >>: Rewrites the greater-than function for non-negative integers
;; Page 72
(define >>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (>> (sub1 n) (sub1 m))))))
  
;; <<: Rewrites the less-than function for non-negatie integers
;; Page 73
(define <<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (<< (sub1 n) (sub1 m))))))

;; ==: Rewrites equality using << and >>
;; Page 74
(define ==
  (lambda (n m)
    (cond
     ((>> n m) #f)
     ((<< n m) #f)
     (else #t))))

