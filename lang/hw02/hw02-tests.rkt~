#lang racket
(#%provide (all-defined))
(#%require "test-infrastructure.rkt")
(#%require rackunit)

(#%require "hw02-dyla.rkt")

;======================================01=======================================
(define p1
  (test-suite
    "list-of-even-numbers?"

    (335-check-true
       (list-of-even-numbers? '(4 20 16 2))
       "test 1. normal input")

    (335-check-false
       (list-of-even-numbers? '(4 1 16 2))
       "test 2. list containing an odd number")

    (335-check-false
       (list-of-even-numbers? '(4 2 16 "two"))
       "test 3. list containing not a number, string")

    (335-check-false
       (list-of-even-numbers? '(4 2 16 'two))
       "test 4. list containing not a number, symbol")

    (335-check-false
       (list-of-even-numbers? '(4 1 16 '(2 4)))
       "test 5. list containing not a number, list")

    (335-check-false
       (list-of-even-numbers? 2)
       "test 6. input is not a list")
  )
)

;======================================02=======================================
(define p2-a
  (test-suite
    "Sn = 1/1 + 1/4 + 1/9 + 1/16 + ..."

    ;;you do not have to account for the case n=0
    (335-check-equal?
       (series-a 1)
       1
       "test 1"
     )

    (335-check-equal?
       (series-a 2)
       (+ 1/4 1)
      "test 2"
     )

    (335-check-equal?
       (series-a 3)
       (+ 1 13/36)
     "test 3"
     )

    (335-check-equal?
       (series-a 4)
       (+ 1 61/144)
    "test 4"
    )
  )
)
;===
(define p2-b
  (test-suite
    "Sn = 1 - 1/2 + 1/6 - 1/24 + ..."

    (335-check-equal?
       (series-b 0)
       1
     "test 0"
     )

    (335-check-equal?
       (series-b 1)
       1/2
     "test 1"
     )

    (335-check-equal?
       (series-b 2)
       2/3
     "test 2"
     )

    (335-check-equal?
       (series-b 3)
       5/8
     "test 3"
     )

    (335-check-equal?
       (series-b 4)
       19/30
     "test 4"
     )

    (335-check-equal?
       (series-b 5)
       91/144
     "test 5"
     )
  )
)

;======================================03=======================================
(define p3
  (test-suite
    "carpet problem"
    (335-check-equal?
       (carpet 0)
       '((%))
     )

    ;it doesn't matter that the expected values are written on different lines
    ;the quote operator will ignore them when creating the lists.
    (335-check-equal?
       (carpet 1)
       '((+ + +)
         (+ % +)
         (+ + +))
     )

    (335-check-equal?
       (carpet 2)
       '((% % % % %)
         (% + + + %)
         (% + % + %)
         (% + + + %)
         (% % % % %))
     )

    (335-check-equal?
       (carpet 3)
       '((+ + + + + + +)
         (+ % % % % % +)
         (+ % + + + % +)
         (+ % + % + % +)
         (+ % + + + % +)
         (+ % % % % % +)
         (+ + + + + + +))
     )
  )
)

;======================================04=======================================
(define p4
  (test-suite
    "pascal's triangle"

    (335-check-equal?
       (pascal 1)
       '((1))
     )

    (335-check-equal?
       (pascal 2)
       '((1) (1 1))
     )

    (335-check-equal?
       (pascal 3)
       '((1) (1 1) (1 2 1))
     )

    (335-check-equal?
       (pascal 4)
       '((1) (1 1) (1 2 1) (1 3 3 1))
     )

    (335-check-equal?
       (pascal 5)
       '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1))
     )

    (335-check-equal?
       (pascal 6)
       '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1) (1 5 10 10 5 1))
     )

    (335-check-equal?
       (pascal 7)
       '((1) (1 1) (1 2 1) (1 3 3 1) (1 4 6 4 1) (1 5 10 10 5 1) (1 6 15 20 15 6 1))
     )
  )
)

;======================================05=======================================
(define p5
  (test-suite
    "parenthesis balancing"

    (335-check-true
       (balanced? "()")
     )

    (335-check-false
       (balanced? "(")
     )

    (335-check-false
       (balanced? ")")
     )

    (335-check-true
       (balanced? "(())")
       "nested parentheses, balanced"
     )

    (335-check-true
       (balanced? "((()))")
       "even more nested parentheses, balanced"
     )

    (335-check-false
       (balanced? "())(")
       "counting is not enough, unbalanced"
     )

    (335-check-true
       (balanced? "(balanced? (stuff))")
       "contains irrelevant characters, balanced"
     )

    (335-check-false
       (balanced? "(balanced? (stuff) probably not")
       "contains irrelevant characters, unbalanced"
     )
   )
)

;======================================06=======================================
(define p6
  (test-suite
    "list-of-all?"

    (335-check-true
       (list-of-all? string? '("a" "b" "42"))
       "test 1. all strings"
     )

    (335-check-false
       (list-of-all? string? '('a "b" "42"))
       "test 2. all strings but the first element"
     )

    (335-check-true
       (list-of-all? number? '(1 2 42 3))
       "test 3. all numbers"
     )

    (335-check-false
       (list-of-all? number? '(1 2 42 "not-a-number"))
       "test 4. all numbers but one"
     )

    (335-check-true
       (list-of-all?
       ;this lambda tells us if the argument is an odd number
       (lambda (n) (and (integer? n) (odd? n)))
       '(1 3 5))
       "test 5. list containing only odd numbers"
     )

    (335-check-false
       (list-of-all?
       (lambda (n) (and (integer? n) (odd? n)))
       '(1 3 6))
       "test 6. list containing almost only odd numbers"
     )
  )
)

;======================================07=======================================
;these are the test values used
(define roman '(I II III IV))
(define arabic '(1 2 3 4))
(define invalid-keys '(a b "c" d))

(define p7
  (test-suite
    "create-mapping"

    (335-check-equal?
       ((create-mapping roman arabic) 'I)
       1
       "test 1"
       )

    (335-check-equal?
       ((create-mapping roman arabic) 'II)
       2
       "test 2"
    )

    (335-check-equal?
       ((create-mapping roman arabic) 'III)
       3
       "test 3"
     )

    (335-check-equal?
       ((create-mapping roman arabic) 'IV)
       4
       "test 4"
     )

    (335-check-exn
      (create-mapping invalid-keys arabic)
      "The keys are not all symbols."
    )

    (335-check-exn
      (create-mapping '(a b c) '(1 2 3 4))
      "The lists are not of equal length."
    )

    (335-check-exn
       ((create-mapping roman arabic) 'not-in-the-map)
       "Could not find mapping for symbol 'not-in-the-map"
     )
  )
)
