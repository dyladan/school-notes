#lang racket
(#%provide (all-defined))
(#%require "test-infrastructure.rkt")
(#%require rackunit)

;if you rename the answer sheet to conform with the submission requirements
;you will have to change the following line to correspond 
(#%require "hw01-answer-sheet.rkt")


;to run the tests, copy the following line without the leading ";" and paste it into your interaction window and hit enter
;(test all)

(define all
  (test-suite
    "These are all the tests for homework 1"
    
    (335-check-equal? (p1-1) 54 "Problem 1, ((3 + 3) * 9)")
    (335-check-equal? (p1-2) 3 "Problem 1, ((6 * 9) / ((4 + 2) + (4 * 3)))")
    (335-check-equal? (p1-3) 42 "Problem 1, (2* ((20 - (91 / 7)) * (45 - 42)))")
    
    (335-check-equal? (p4) 7 "p2, sum of the two largest x,y,z")
    (335-check-equal? (p5) 5 "p2, sum of the two smallest x,y,z")
    (335-check-equal? (p6) #f "p2, x, y are not equal")
    
    (335-check-equal? (p12-1 example) '(d a b c) "12-1")
    (335-check-equal? (p12-2 example) '(a b d a b) "12-2, ")
    (335-check-equal? (p12-3 example) '(b c d a) "12-3")
    
    ;;problem 14
    (335-check-equal? (create-error-msg 'forty-two 42)
                      "This is a custom error message we will be using next. Symbol 'forty-two was not paired with value 42")
    
    ;;problem 15
    (335-check-equal? (check-correctness '(answer-to-everything 42)) #t)
    (335-check-equal? (check-correctness '(symbol-other-than-the-previous-one 42)) #f)
    (335-check-equal? (check-correctness '(test 30)) #f)
    
    ;this will check to see if an exception with the below written error-message
    ;is raised.
    (335-check-exn 
       (check-correctness '(answer-to-everything 10))
       "This is a custom error message we will be using next. Symbol 'answer-to-everything was not paired with value 42")
  )
)