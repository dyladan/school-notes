#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))

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
   - please rename this file to hw05-yourlastname.rkt prior to submission
   - also rename hw05-tests.rkt to hw05-yourlastname-tests.rkt
   - upload both hw05-yourlastname.rkt and hw05-tests.rkt
|#
;=======================================01======================================
(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (~a received)
                 )
)

;You can compare the contents of this answer sheet with the answer sheet of the
;previous homework to infer what is generated automatically by define-datatype.

(define-datatype step step?
  (up-step
   (n number?))
  (down-step
   (n number?))
  (left-step
   (n number?))
  (right-step
   (n number?))
  (seq-step
   (s1 step?)
   (s2 step?))
  )

(define (up-step? st)
  (if (step? st)
      (cases step st
        (up-step (n) #t)
        (down-step (n) #f)
        (left-step (n) #f)
        (right-step (n) #f)
        (seq-step (s1 s2) #f))
      #f)
)

(define (down-step? st)
    (if (step? st)
        (cases step st
          (up-step (n) #f)
          (down-step (n) #t)
          (left-step (n) #f)
          (right-step (n) #f)
          (seq-step (s1 s2) #f))
        #f)
)

(define (left-step? st)
    (if (step? st)
        (cases step st
          (up-step (n) #f)
          (down-step (n) #f)
          (left-step (n) #t)
          (right-step (n) #f)
          (seq-step (s1 s2) #f))
        #f)
)

(define (right-step? st)
    (if (step? st)
        (cases step st
          (up-step (n) #f)
          (down-step (n) #f)
          (left-step (n) #f)
          (right-step (n) #t)
          (seq-step (s1 s2) #f))
        #f)
)

(define (seq-step? st)
    (if (step? st)
        (cases step st
          (up-step (n) #f)
          (down-step (n) #f)
          (left-step (n) #f)
          (right-step (n) #f)
          (seq-step (s1 s2) #t))
        #f)
)

;;to avoid needless duplication we will only implement one extractor to handle all the
;;simple steps, rather than 4. So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (and (step? st) (not (seq-step? st)))
      (cases step st
        (up-step (n) n)
        (down-step (n) n)
        (left-step (n) n)
        (right-step (n) n)
        (seq-step (s1 s2) #f))
      (string-append
       "Invalid arguments in: single-step->n"
       " --- "
       "expected: single-step?"
       " --- "
       "received: not-a-single-step"))
)

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (and (step? st) (seq-step? st))
      (cases step st
        (up-step (n) #f)
        (down-step (n) #f)
        (left-step (n) #f)
        (right-step (n) #f)
        (seq-step (s1 s2) s1))
      (string-append
       "Invalid arguments in: seq-step->st-1"
       " --- "
       "expected: seq-step?"
       " --- "
       "received: not-a-seq-step"))
)


(define (seq-step->st-2 st)
  (if (and (step? st) (seq-step? st))
      (cases step st
        (up-step (n) #f)
        (down-step (n) #f)
        (left-step (n) #f)
        (right-step (n) #f)
        (seq-step (s1 s2) s2))
      (string-append
       "Invalid arguments in: seq-step->st-2"
       " --- "
       "expected: seq-step?"
       " --- "
       "received: not-a-seq-step"))
)
;;===================================
(define (move start-p st)
    (cases step st
      (up-step (n)
               (list (car start-p) (+ (cadr start-p) (single-step->n st))))
      (down-step (n)
                 (list (car start-p) (- (cadr start-p) (single-step->n st))))
      (left-step (n)
                 (list (- (car start-p) (single-step->n st)) (cadr start-p)))
      (right-step (n)
                  (list (+ (car start-p) (single-step->n st)) (cadr start-p)))
      (seq-step (s1 s2)
              (move (move start-p s1) s2)))
)
;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

;
(define-datatype environment environment?
  (empty-env)
  (extend-env
   (var symbol?)
   (val number?)
   (env environment?))
  (extend-env-final
   (var symbol?)
   (val number?)
   (env environment?)))

(define (apply-env env search-sym)
  (cases environment env
    (empty-env ()
               (exception-no-binding-msg search-sym))
    (extend-env (var val e)
                (if (eq? var search-sym) val
                    (apply-env e search-sym)))
    (extend-env-final (var val e)
                (if (eq? var search-sym) val
                    (apply-env e search-sym))))
  )

;==========
;2.b
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
  )


(define (is-final? env sym)
  (cases environment env
    (empty-env () #f)
    (extend-env (stored-sym val prev-env)
                (if (equal? sym stored-sym)
                    #f
                    (is-final? prev-env sym)))
    (extend-env-final (stored-sym val prev-env)
                      (if (equal? sym stored-sym)
                          #t
                          (is-final? prev-env sym)))
    (else #f)))

;It is prefered to give meaningfull names to marker values.
;In the tests we will be using these two values to invoke
;the extend-env-wrapper function
(define FINAL #t)
(define NON-FINAL #f)

(define (extend-env-wrapper sym val old-env final?)
  (cond
    ((is-final? old-env sym)
     (exception-sym-final-msg sym))
    (final?
     (extend-env-final sym val old-env))
    (else
     (extend-env sym val old-env))))

