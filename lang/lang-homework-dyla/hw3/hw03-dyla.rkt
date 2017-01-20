#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================

(define (foldl-335 op zero-el lst)
  (if (null? lst) zero-el
       (foldl-335 op (op (car lst) zero-el) (cdr lst)))
  )


;---
(define (rcdr lst)
  (reverse (cdr (reverse lst))))

(define (rcar lst)
  (car (reverse lst)))

(define (foldr-335 op zero-el lst)
  (if (null? lst) zero-el
      (foldr-335 op (op (rcar lst) zero-el) (rcdr lst)))
   
)

;======================================02=======================================

(define (andmap-335 test-op lst)
  (define (t e n)
    (if (test-op e) n (add1 n)))
  (= 0 (foldl t 0 lst))
)

;======================================03=======================================
(define (filter-335 test-op lst)
  (cond
    ((null? lst) '())
    ((test-op (car lst))
     (cons (car lst) (filter-335 test-op (cdr lst))))
    (else
     (filter-335 test-op (cdr lst))))
)

;======================================04=======================================
(define (map-reduce m-op r-op zero-el lst)
  (foldl r-op zero-el (map m-op lst))
)

;======================================05=======================================
(define (elem n)
  (/ (if (odd? n) -1 1) (fact (add1 n))))

(define (fact n)
  (if (>= 1 n) 1
      (* n (fact (sub1 n)))))

(define (series n)
  (foldl + 0 (map elem (range (add1 n))))
)

;======================================06=======================================
(define (zip lst1 lst2)
  (cond
    ((null? lst1) '())
    (else
     (cons (list (car lst1) (car lst2)) (zip (cdr lst1) (cdr lst2)))))
)

;======================================07=======================================

(define (reshape mat)
  (if (null? (car mat)) '()
      (cons (map car mat) (reshape (map cdr mat)))))

(define (matrix-to-vector op mat)
  (define (myop lst)
    (apply op lst))
  (map myop (reshape mat)))

