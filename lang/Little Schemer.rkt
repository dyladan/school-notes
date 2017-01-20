#lang racket
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define firsts
  (lambda (l)
    (map car l)))

(define f2
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (caar l) (f2 (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat)
       '())
      ((eq? (car lat) old)
       (cons (car lat) (cons new (cdr lat))))
      (else
       (cons (car lat) (insertR new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat)
       '())
      ((eq? old (car lat))
       (cons new (cdr lat)))
      (else
       (cons (car lat) (subst new old (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else
       (cons (car lat) (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else
       (cons (car lat) (multiinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (multisubst new old (cdr lat))))
      (else
       (cons (car lat) (multisubst new old (cdr lat)))))))

(define plus
  (lambda (a b)
    (cond
      ((zero? a) b)
      (else (plus (sub1 a) (add1 b))))))

(define minus
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else
       (minus (sub1 a) (sub1 b))))))

(define addtup
  (lambda (tup)
    (if
      (null? tup) 0
      (+ (car tup) (addtup (cdr tup))))))

(define times
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else
       (+ a (times a (sub1 b)))))))

(define tup+
  (lambda (t1 t2)
    (cond
      ((null? t1) t2)
      ((null? t2) t1)
      (else
       (cons (+ (car t1) (car t2)) (tup+ (cdr t1) (cdr t2)))))))

(define gt
  (lambda (a b)
    (cond
      ((zero? a) #f)
      ((zero? b) #t)
      (else
       (gt (sub1 a) (sub1 b))))))

(define lt
  (lambda (a b)
    (cond
      ((zero? b) #f)
      ((zero? a) #t)
      (else
       (lt (sub1 a) (sub1 b))))))

(define pow
  (lambda (a b)
    (cond
      ((zero? b) 1)
      (else (* a (pow (sub1 b)))))))

(define div
  (lambda (a b)
    (cond
      ((< a b) 0)
      (else
       (add1 (div (- a b) b))))))

(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
       (add1 (len (cdr l)))))))

(define pick
  (lambda (n lat)
    (cond
      ((= n 1) (car lat))
      (else
       (pick (sub1 n) (cdr lat))))))

(define rempick
  (lambda (n lat)
    (cond
      ((= n 1) (cdr lat))
      (else
       (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else
       (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
      (else
       (all-nums (cdr lat))))))

(define eqan?
  (lambda (a b)
    (if (and (number? a) (number? b))
        (= a b)
        (eq? a b))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a)
        (add1 (occur a (cdr lat))))
       (else
        (occur a (cdr lat))))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (if (eq? (car l) a)
           (rember* a (cdr l))
           (cons (car l) (rember* a (cdr l)))))
      (else
       (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons (car l) (cons new (insertR* new old (cdr l)))))
         (else
          (cons (car l) (insertR* new old (cdr l))))))
       (else
        (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

;(firsts '((apple) (plum pear) (grape juice)))
;(f2 '((apple) (plum pear) (grape juice)))
;(insertR 'fresh 'want (insertR 'juice 'apple '(I want apple)))
;(subst 'pear 'apple '(I want apple juice))
;(multirember 'um '(I um want um some apple juice))
;(multiinsertR 'fried 'fish '(chips and fish or fish and fried))
;(multiinsertL 'fried 'fish '(chips and fish or fish and fried))
;(multisubst 'ah 'um '(I um want um some apple juice))
;(plus 87 3);
;(minus 87 3)
;(addtup '(3 5 2 8))
;(addtup '(15 6 7 12 3))
;(times 3 4)
;(tup+ '(3 6 9 11 4) '(8 5 2 0 7))
;(tup+ '(3 6 9 11 4 11) '(8 5 2 0 7))
;(gt 1 0)
;(lt 0 1)
;(div 15 4)
;(len '())
;(len '(1))
;(len '(1 3 4))
;(pick 4 '(lasagna spaghetti ravioli macaroni meatball))
;(rempick 3 '(hotdogs with hot mustard))
;(no-nums '(5 pears 6 prunes 9 dates))
;(all-nums '(5 pears 6 prunes 9 dates))
;(eqan? 'a 3)
;(occur 'a '(a b c d e a))
;(rember* 'sauce '(((tomato sauce)) ((bean) sauce) (and ((flying)) sauce)))
;(insertR* 'roast 'chuck '((how much (wood)) could ((a (wood) chuck )) (((chuck))) (if (a) ((wood chuck))) could chuck wood))
