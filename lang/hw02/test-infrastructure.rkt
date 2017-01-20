#lang racket
(require rackunit)
(#%provide (all-defined))

;;all the following syntax definitions wrap a standard
;;rackunit assertion in a check-not-exn one. This is
;;necessary because if functions result in unexpected
;;errors, e.g. "arity mismatch" the test runner does
;;not execute the remaining tests.

;;To run the tests simply execute:
;;>(test a-test-suite)
;;where a-test-suite is a custom (test-suite); see
;;example-tests, defined below.

;;More info on rackunit can be found here: 
;; http://docs.racket-lang.org/rackunit/api.html?q=rackunit

(define-syntax 335-check-equal?
 (syntax-rules ()
  [ (335-check-equal? actual expected)
      (check-not-exn 
        (lambda () (check-equal? actual expected))  
       ) 
  ]
   
  [ (335-check-equal? actual expected message)
      (check-not-exn 
        (lambda () (check-equal? actual expected message))  
       ) 
  ]
 )
)

(define-syntax 335-check-true
 (syntax-rules ()
  [ (335-check-true val)
      (check-not-exn 
        (lambda () (check-true val))  
       ) 
  ]
   
  [ (335-check-true val message)
      (check-not-exn 
        (lambda () (check-true val message))  
       ) 
  ]
 )
)

(define-syntax 335-check-false
 (syntax-rules ()
  [ (335-check-false val)
      (check-not-exn 
        (lambda () (check-false val))  
       ) 
  ]
   
  [ (335-check-false val message)
      (check-not-exn 
        (lambda () (check-false val message))  
       ) 
  ]
 )
)

(define-syntax 335-check-exn
 (syntax-rules ()
  [ (335-check-exn expression exn-msg)
      (335-check-equal? 
       (with-handlers ([string? (lambda (err-msg) err-msg)]) 
                    expression)
                    exn-msg)
  ]
 )
)

(require rackunit/text-ui)
(define (test suite)
  (run-tests suite 'verbose)  
)
;;examples:

(define example-tests
  (test-suite
   "example test-suite"
  
   (335-check-equal? 1 (car '()))
   (335-check-equal? 1 1)
   (335-check-equal? 1 2)
     
   (335-check-equal? 1 (car '()) "raises exception")
   (335-check-equal? 1 1 "equal")
   (335-check-equal? 1 2 "legitimate failure")
     
   (335-check-true #t)
   (335-check-true #f)
   (335-check-true (/ 1 0))
     

   (335-check-true #t "true")
   (335-check-true #f "legitimate failure")
   (335-check-true (/ 1 0) "raises exception")
     
   (335-check-false #f)
   (335-check-false #t)
   (335-check-false (/ 1 0))
     
   (335-check-false #f "false")
   (335-check-false #t "legitimate failure")
   (335-check-false (/ 1 0) "raises exception")
     
   (335-check-exn (raise "42") "42")
     
   (335-check-exn (/ 1 0) "42")
  )
)
 