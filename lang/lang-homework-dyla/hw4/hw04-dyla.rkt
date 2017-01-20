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
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename the file to hw04-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#
;======================================01=======================================
(define-syntax-rule (for {var <- value-range} yield result)
   (map (lambda (x) (let ([var x]) result)) value-range)
)
;======================================02=======================================
(define-syntax-rule (seq expr1 expr2)
  ((lambda () expr1 expr2))
)

;====
(define-syntax-rule (while condition body)
  ((lambda (f) (f f))
   (lambda (while-iter)
      (cond
        (condition body (while-iter while-iter))
(else 0)))))

;======================================03=======================================

#|
<step> ::=  <step>  <step>       "seq-step"
          | "up" number          "up-step"
          | "down" number        "down-step"
          | "left" number        "left-step"
          | "right" number       "right-step"
|#
;example of how to create the error message for the "up-step" constructor
;> (invalid-args-msg "up-step" "number?" '(1 2 3 4))
;where '(1 2 3 4) should be replaced by the actual violating value.

(define (invalid-args-msg fn expected actual)
  (string-append
   "Invalid arguments in: "
   fn
   " --- expected: "
   expected
   " --- received: "
   actual))

;;you can reorder the functions below if it better suits your needs
(define (up-step n)
  (if (not (number? n)) (invalid-args-msg "up-step" "number?" "not-a-number")
  (list 'up n))
)

(define (down-step n)
  (if (not (number? n)) (invalid-args-msg "down-step" "number?" "not-a-number")
  (list 'down n))
)

(define (left-step n)
  (if (not (number? n)) (invalid-args-msg "left-step" "number?" "not-a-number")
  (list 'left n))
)

(define (right-step n)
  (if (not (number? n)) (invalid-args-msg "right-step" "number?" "not-a-number")
  (list 'right n))
)

(define (seq-step st-1 st-2)
  (if (and (step? st-1) (step? st-2))
      (list 'seq st-1 st-2)
      (invalid-args-msg "seq-step" "step?" "not-a-step"))
)

;;====
(define (step-like-pair? st)
  (and
    (pair? st)
    (= (length st) 2)
    (number? (cadr st))))

(define (up-step? st)
  (and
    (step-like-pair? st)
    (eqv? (car st) 'up))
)

(define (down-step? st)
  (and
    (step-like-pair? st)
    (eqv? (car st) 'down))
)

(define (left-step? st)
  (and
    (step-like-pair? st)
    (eqv? (car st) 'left)) 
)

(define (right-step? st)
  (and
    (step-like-pair? st)
    (eqv? (car st) 'right))
)

(define (single-step? st)
  (or
   (right-step? st)
   (left-step? st)
   (up-step? st)
   (down-step? st)))

(define (seq-step? st)
  (and
   (pair? st)
   (= (length st) 3)
   (eqv? (car st) 'seq)
   (step? (cadr st))
   (step? (caddr st))))

;This is a predicate that tells you whether or not something is a step,
;it should return true when given either up, down, left, right or seq steps.
(define (step? st)
  (or
   (up-step? st)
   (down-step? st)
   (left-step? st)
   (right-step? st)
   (seq-step? st)))


;; to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. 
;; So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (single-step? st) (cadr st)
      (invalid-args-msg "single-step->n" "single-step?" "not-a-single-step")))

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (seq-step? st) (cadr st)
      (invalid-args-msg "seq-step->st-1" "seq-step?" "not-a-seq-step"))
)


(define (seq-step->st-2 st)
  (if (seq-step? st) (caddr st)
      (invalid-args-msg "seq-step->st-2" "seq-step?" "not-a-seq-step"))
)
;;===================================
(define (move start-p step)
  (cond
    ((seq-step? step)
     (move (move start-p (seq-step->st-1 step)) (seq-step->st-2 step)))
    ((up-step? step)
     (list (car start-p) (+ (cadr start-p) (single-step->n step))))
    ((down-step? step)
     (list (car start-p) (- (cadr start-p) (single-step->n step))))
    ((right-step? step)
     (list (+ (car start-p) (single-step->n step)) (cadr start-p)))
    ((left-step? step)
     (list (- (car start-p) (single-step->n step)) (cadr start-p))))
)

;======================================04=======================================
;singleton-set should return a function that takes a number as an argument and
;tells whether or not that number is in the set
(define (singleton-set x)
  (lambda (e)
    (eq? e x))
  )


;the set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (lambda (e)
    (or (s1 e) (s2 e))))

;the set of all elements that are in both  in 's1' and 's2'
(define (intersection s1 s2)
  (lambda (e)
    (and (s1 e) (s2 e))))

;the set of all elements that are in 's1', but that are not in 's2'
(define (diff s1 s2)
  (lambda (e) (and (s1 e) (not (s2 e)))))

;returns the subset of s, for which the predicate 'predicate' is true.
(define (filter predicate s)
  (lambda (e) (and (predicate e) (s e))))

;we assume that the sets can contain only numbers between 0 and bound
(define bound 100)

;returns whether or not the set contains at least an element for which
;the predicate is true. s below is the parameter standing for a given set
(define (exists? predicate s)
  (ormap
    (lambda (e)
      (and (predicate e)
           (s e)))
    (range bound)))

;returns whether or not the predicate is true for all the elements
;of the given set s
(define (all? predicate s)
  (andmap
    (lambda (e)
      (or
        (not (s e))
        (and
          (predicate e)
          (s e))))
    (range bound)))

;returns a new set where "op" has been applied to all elements
; NOTE: just because a procedure/function has the word "map" in it, it 
;       doesn't mean you have to use map higher order function to implement it. 
;       Map is a functional operation with well defined behavior that 
;       is not tied to any implementation.
(define (map-set op s)
  (lambda (e)
    (ormap
      (lambda (f)
        (and (s f) (eq? (op f) e)))
      (range bound))))

;just a sample predicate
; built in one was broken?
(define (broken-prime? n)
  (define (non-divisible? n)
    (lambda (i)
      (not (= (modulo n i) 0))))
  (define range-of-prime-divisors (cddr (range (+ (integer-sqrt n) 1))))
  (if (equal? n 1)
      #f
      (andmap (non-divisible? n) range-of-prime-divisors)
      )
  )

;fixed it
(define (prime? n)
  (define (non-divisible? n)
    (lambda (i)
      (not (= (modulo n i) 0))))
  (if (or (equal? n 1) (equal? n 0))
    #f
    (let ([range-of-prime-divisors (cddr (range (+ (integer-sqrt n) 1)))])
      (andmap (non-divisible? n) range-of-prime-divisors))))

;=====================================05====================================
; FYI:
;  to emphasize the procedural-based approach to implement "step" data type and to
;  contrast it with the data structure-based approach for "step" implementation 
;  used in p3, here we add "-proc" suffix to each corresponding function name.

;====p5-a================
(define (up-step-proc n)
  (if (not (number? n))
      (invalid-args-msg "up-step-proc"
                        "number?"
                        "not-a-number")
      (lambda (e)
        (cond
          ((eq? e 'step-req) n)
          ((eq? e 'step-chk) 'up)))))

(define (down-step-proc n)
  (if (not (number? n))
      (invalid-args-msg "down-step-proc"
                        "number?"
                        "not-a-number")
      (lambda (e)
        (cond
          ((eq? e 'step-req) n)
          ((eq? e 'step-chk) 'down)))))

(define (left-step-proc n)
  (if (not (number? n))
      (invalid-args-msg "left-step-proc"
                        "number?"
                        "not-a-number")
      (lambda (e)
        (cond
          ((eq? e 'step-req) n)
          ((eq? e 'step-chk) 'left)))))

(define (right-step-proc n)
  (if (not (number? n))
      (invalid-args-msg "right-step-proc"
                        "number?"
                        "not-a-number")
      (lambda (e)
        (cond
          ((eq? e 'step-req) n)
          ((eq? e 'step-chk) 'right)))))

(define (seq-step-proc st-1 st-2)
  (cond
    ((not (step-proc? st-1)) (invalid-args-msg
                         "seq-step-proc"
                         "step-proc?"
                         "not-a-step-proc"))
    ((not (step-proc? st-2)) (invalid-args-msg
                         "seq-step-proc"
                         "step-proc?"
                         "not-a-step-proc"))
    (else
      (lambda (e)
        (cond
          ((eq? e 'step-req)
           (list (st-1 'step-req) (st-2 'step-req)))
          ((eq? e 'seq-req1) st-1)
          ((eq? e 'seq-req2) st-2)
          ((eq? e 'step-chk) 'seq))))))

;;====
(define (up-step-proc? st)
  (eq? (st 'step-chk) 'up)
)

(define (down-step-proc? st)
  (eq? (st 'step-chk) 'down)
)

(define (left-step-proc? st)
  (eq? (st 'step-chk) 'left)
)

(define (right-step-proc? st)
  (eq? (st 'step-chk) 'right)
)

(define (seq-step-proc? st)
  (eq? (st 'step-chk) 'seq) 
)

;This is a predicate that tells you whether or not st is a step,
; it should return true when given either up, down, left, right or seq steps.
(define (step-proc? st)
  (if (not (procedure? st))
      #f
      (ormap
       (lambda (e)
         (eq? (st 'step-chk) e))
       '(up down left right seq))))

(define (single-step-proc? st)
  (if (not (procedure? st))
      #f
      (ormap
       (lambda (e)
         (eq? (st 'step-chk) e))
       '(up down left right))))


;;to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. So this should take: up, down, left and right 
;; steps. 
(define (single-step-proc->n st)
  (if (single-step-proc? st)
      (st 'step-req)
      (invalid-args-msg "single-step-proc->n"
                        "single-step-proc?"
                        "not-a-single-step-proc"))
)

;;two extractors
(define (seq-step-proc->st-1 st)
  (if (seq-step-proc? st)
      (st 'seq-req1)
      (invalid-args-msg "seq-step-proc->n"
                        "seq-step-proc?"
                        "not-a-seq-step-proc"))
)


(define (seq-step-proc->st-2 st)
  (if (seq-step-proc? st)
      (st 'seq-req2)
      (invalid-args-msg "seq-step-proc->n"
                        "seq-step-proc?"
                        "not-a-seq-step-proc"))
)
;;========p5-b
(define (move-proc start-p step-proc)
  (cond
    ((seq-step-proc? step-proc)
     (move-proc (move-proc start-p (seq-step-proc->st-1 step-proc)) (seq-step-proc->st-2 step-proc)))
    ((up-step-proc? step-proc)
     (list (car start-p) (+ (cadr start-p) (single-step-proc->n step-proc))))
    ((down-step-proc? step-proc)
     (list (car start-p) (- (cadr start-p) (single-step-proc->n step-proc))))
    ((right-step-proc? step-proc)
     (list (+ (car start-p) (single-step-proc->n step-proc)) (cadr start-p)))
    ((left-step-proc? step-proc)
     (list (- (car start-p) (single-step-proc->n step-proc)) (cadr start-p))))
)