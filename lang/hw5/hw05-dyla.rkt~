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
  (unimplemented)
  )

(define (up-step? st)
  'UNIMPLEMENTED  
)

(define (down-step? st)
  'UNIMPLEMENTED  
)

(define (left-step? st)
  'UNIMPLEMENTED  
)

(define (right-step? st)
  'UNIMPLEMENTED  
)

(define (seq-step? st)
  'UNIMPLEMENTED  
)

;;to avoid needless duplication we will only implement one extractor to handle all the
;;simple steps, rather than 4. So this should take: up, down, left and right steps.
(define (single-step->n st)
  'UNIMPLMENTED  
)

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  'UNIMPLEMENTED  
)


(define (seq-step->st-2 st)
  'UNIMPLEMENTED  
)
;;===================================
(define (move start-p step)
  'UNIMPLEMENTED  
)
;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

;
(define-datatype environment environment?
  ;delete this unimplemented variant from your solution
  (unimplemented-env)
  )

(define (apply-env env search-sym)
  'UNIMPLEMENTED
  )

;==========
;2.b
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
  )

;It is prefered to give meaningfull names to marker values.
;In the tests we will be using these two values to invoke
;the extend-env-wrapper function
(define FINAL #t)
(define NON-FINAL #f)

(define (extend-env-wrapper sym val old-env final?)
  'UNIMPLEMENTED
  )

