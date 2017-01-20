#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (cond
    ((null? lst) #t)
    ((not (pair? lst)) #f)
    ((not (number? (car lst))) #f)
    ((not (even? (car lst))) #f)
    (else
     (list-of-even-numbers? (cdr lst))))
)

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (cond
    ((= n 1) 1)
    (else
     (+ (/ 1 (* n n)) (series-a (- n 1)))))
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...
(define (neg1-pow n)
  (if (even? n) 1 -1))

(define (fact n)
  (if (= n 0) 1
      (* n (fact (sub1 n)))))

(define (series-b n)
  (if
    (= n 0) 1
    (+ (/ (neg1-pow n) (fact (add1 n))) (series-b (sub1 n))))
)

;======================================03=======================================
; width and height are both 2n + 1 => (add1 (* 2 n))
; even n => %, else => +
(define (cap n)
  (let
      ((c (if (even? n) '% '+))
       (n (add1 (* 2 n))))
    (make-list n c)))

(define (surround lst a)
  (cons a (append lst (list a))))


(define (next-carpet-line ln)
  (letrec
      (
       (n (/ (sub1 (length ln)) 2))
       (c (if (even? n) '+ '%))
       )
    (surround ln c)))


(define (carpet n)
  (cond
    ((zero? n) '((%)))
    (else
     (surround
      (map next-carpet-line (carpet (sub1 n)))
      (cap n)))))

;======================================04=======================================
(define (pair-off lst)
  (cond
    ((null? lst) '())
    ((< (length lst) 2) '())
    (else
     (cons (list (car lst) (cadr lst)) (pair-off (cdr lst))))))

(define (sub-sum lst)
  (cond
    ((null? lst) '())
    (else
     (cons (foldl + 0 (car lst)) (sub-sum (cdr lst))))))

(define (pnext lst)
  (cond
    ((null? lst) '())
    (else
     (sub-sum (pair-off (surround (car lst) 0))))))

(define (pascal i)
  (define (rpascal n)
    (cond
      ((zero? n) '())
      ((= 1 n) '((1)))
      (else
       (cons (pnext (rpascal (sub1 n))) (rpascal (sub1 n))))))
  (reverse (rpascal i)))


;======================================05=======================================
(define (balanced? in)
  (define (bcount lst i)
    (cond
      ((null? lst)
       (if (zero? i) #t #f))
      ((> 0 i) #f)
      ((equal? (car lst) #\()
       (bcount (cdr lst) (add1 i)))
      ((equal? (car lst) #\))
       (bcount (cdr lst) (sub1 i)))
      (else
       (bcount (cdr lst) i))))
  (bcount (string->list in) 0)
)

;======================================06=======================================
(define (list-of-all? predicate lst)
  (cond
    ((null? lst) #t)
    (else
     (and (predicate (car lst)) (list-of-all? predicate (cdr lst)))))
)

;======================================07=======================================
(define (create-mapping keys vals)
  (define (allsym? lst)
    (cond
      ((null? lst) #t)
      (else
       (and (symbol? (car lst)) (allsym? (cdr lst))))))
  (define (get-val k ks vs)
    (cond
      ((or (null? ks) (null? vs))
       (string-append
        "Could not find mapping for symbol '"
        (symbol->string k)))
      ((equal? (car ks) k) (car vs))
      (else
       (get-val k (cdr ks) (cdr vs)))))
  (cond
    ((not (allsym? keys))
     "The keys are not all symbols.")
    ((not (= (length keys) (length vals)))
     "The lists are not of equal length.")
    (else
     (lambda (k)
       (get-val k keys vals))))
)