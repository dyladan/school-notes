#lang racket
(#%provide (all-defined))
(#%require "test-infrastructure.rkt")
(#%require rackunit)

(#%require "hw04-dyla.rkt")



;this function will run all the tests.
(define (test-all)
  (test p1)
  (test p2-a)
  (test p2-b)
  (test p3-a)
  (test p3-b)
  (test p4)
  (test p5-a)
  (test p5-b)
)

;======================================01=======================================
(define p1
  (test-suite
    "for"

    (335-check-equal?
       (for {i <- '()} yield (+ i 42))
       '()
       "empty list"
     )

    (335-check-equal?
       (for {a <- '(0 1 2 3)} yield (+ a 42))
       '(42 43 44 45)
       "add 42 to the list"

     )

    (335-check-equal?
       (for {to-ignore <- '(0 1 2 3)} yield 42)
       '(42 42 42 42)
       "ignore looping variable"
     )
  )
)
;======================================02=======================================
(define test-var-1 42)
(define p2-a
  (test-suite
    "seq"

    (335-check-true
       (and
          (equal? (seq (set! test-var-1 'not-42) 342) 342)
          (equal? test-var-1 'not-42))
       "measure side effect and return value"
     )

    (335-check-equal?
       (seq 'to-be-ignored (* 5 6))
       30
       "ignoring first expression"
     )
  )
)
;;======================
(define test-counter-1 0)

(define p2-b
  (test-suite
    "while"

    (335-check-true
       (and
         (equal? 0 (while (< test-counter-1 100)
                          (set! test-counter-1 (+ test-counter-1 1))
                   )
         )
         (equal? test-counter-1 100)
       )
       "loop and increment counter, while always returns 0"
     )

    (test-case
       "nested whiles"
       (let ([i 0] [j 0] [v #()])
         (while (< i 3)
           (begin
              ;;this condition is here to exemplify that it should be arbitrarily complex
              (while (and (< j 5) (= 1 1))
                  (begin
                    (set! j (+ j 1))
                    (set! v (vector-append v #(*)))
                  )
              )
              (set! v (vector-append v #(/n)))
              (set! j 0)
              (set! i (+ i 1))
           )
         )
         (335-check-equal? v #(* * * * * /n * * * * * /n * * * * * /n))
       )
     )
  )
)

;======================================03=======================================
(define p3-a
  (test-suite
    "steps-3a"

    (335-check-true
       (up-step? (up-step 3))
       "test 1"
       )

    (335-check-exn
       (up-step "not-a-number")
       "Invalid arguments in: up-step --- expected: number? --- received: not-a-number")

    (335-check-true
       (down-step? (down-step 3))
       "test 2")

    (335-check-exn
       (down-step "not-a-number")
       "Invalid arguments in: down-step --- expected: number? --- received: not-a-number")

    (335-check-true
       (left-step? (left-step 3))
       "test 3")

    (335-check-exn
       (left-step "not-a-number")
       "Invalid arguments in: left-step --- expected: number? --- received: not-a-number")

    (335-check-true
       (right-step? (right-step 3))
       "test 4")

    (335-check-true
       (seq-step? (seq-step (right-step 3) (up-step 4)))
       "test 5")

    (335-check-exn
       (seq-step "not-a-step" (up-step 4))
       "Invalid arguments in: seq-step --- expected: step? --- received: not-a-step")

    (335-check-true
       (step? (up-step 3))
       "test 6")

    (335-check-true
       (step? (down-step 3))
       "test 7")

    (335-check-true
       (step? (left-step 3))
       "test 8")

    (335-check-true
       (step? (right-step 3))
       "test 9")

    (335-check-true
       (step? (seq-step (right-step 3) (up-step 4)))
       "test 10")

    (335-check-equal?
        (single-step->n (up-step 3))
        3
        "test 11"
     )

    (335-check-equal?
        (single-step->n (down-step 3))
        3
        "test 12"
     )

    (335-check-equal?
        (single-step->n (left-step 3))
        3
        "test 13"
     )

    (335-check-equal?
        (single-step->n (right-step 3))
        3
        "test 14"
     )

    (335-check-exn
       (single-step->n "not-a-single-step")
       "Invalid arguments in: single-step->n --- expected: single-step? --- received: not-a-single-step")

    (335-check-equal?
        (seq-step->st-1 (seq-step (left-step 3) (right-step 4)))
        (left-step 3)
        "test 15"
     )

    (335-check-equal?
        (seq-step->st-2 (seq-step (left-step 3) (right-step 4)))
        (right-step 4)
        "test 16"
     )
  )
)

;;======
(define p3-b
  (test-suite
    "steps-3b"
    (335-check-equal?
        (move '(0 0) (up-step 3))
        '(0 3)
        "move up"
     )

    (335-check-equal?
        (move '(0 0) (down-step 3))
        '(0 -3)
        "move down"
     )

    (335-check-equal?
        (move '(0 0) (left-step 3))
        '(-3 0)
        "move left"
     )

    (335-check-equal?
        (move '(0 0) (right-step 3))
        '(3 0)
        "move right"
     )

    (335-check-equal?
        (move '(0 0) (seq-step (up-step 3)(right-step 3)))
        '(3 3)
        "move in sequence: up, right"
     )

    (335-check-equal?
        (move '(0 0) (seq-step (up-step 3)(down-step 3)))
        '(0 0)
        "move in sequence: up, down; they should cancel each other"
     )

    (335-check-equal?
        (move '(0 0) (seq-step (up-step 10) (seq-step (left-step 7) (right-step 4))))
        '(-3 10)
        "move in sequence of sequence: up, left, right; they should cancel each other"
     )

  )
)

;======================================04=======================================
(define p4
  (test-suite
   "functional sets"
   (test-case
    "singleton-set test"

    (335-check-true ((singleton-set 1) 1) "set containing 1, given 1")
    (335-check-false ((singleton-set 1) 2) "set containing 1, given 2")
    )

   (test-case
    "union test"

    ;we will actually give names to our test data
    (define s1 (singleton-set 1))
    (define s2 (singleton-set 2))
    (define s1-2 (union s1 s2))

    (335-check-true (s1-2 1) "set containing 1 2, given 1")
    (335-check-true (s1-2 2) "set containing 1 2, given 2")
    (335-check-false (s1-2 3) "set containing 1 2, given 3")
    )

   (test-case
    "intersection test"

    ;we will actually give names to our test data
    (define s1 (singleton-set 1))
    (define s2 (singleton-set 2))
    (define s3 (singleton-set 3))
    (define s4 (singleton-set 4))

    (define s1-3 (union (union s1 s2) s3))
    (define s2-4 (union (union s2 s3) s4))

    ;this set will effectively contain elements 2,3
    (define s2-3 (intersection s1-3 s2-4))

    (335-check-true (s2-3 2) "set containing 2 3, given 2")
    (335-check-true (s2-3 3) "set containing 2 3, given 3")
    (335-check-false (s2-3 1) "set containing 2 3, given 1")
    (335-check-false (s2-3 4) "set containing 2 3, given 4")

    (335-check-false (s2-3 42) "set containing 2 3, given 42")
    )

   (test-case
    "diff test"

    ;we will actually give names to our test data
    (define s1 (singleton-set 1))
    (define s2 (singleton-set 2))
    (define s3 (singleton-set 3))
    (define s4 (singleton-set 4))

    (define s1-3 (union (union s1 s2) s3))
    (define s2-4 (union (union s2 s3) s4))

    ;this set will effectively contain only: 1
    (define sd-1 (diff s1-3 s2-4))

    (335-check-true (sd-1 1) "set containing 1, given 1")
    (335-check-false (sd-1 2) "set containing 1, given 2")
    (335-check-false (sd-1 3) "set containing 1, given 3")
    (335-check-false (sd-1 4) "set containing 1, given 4")

    (335-check-false (sd-1 42) "set containing 1, given 42")
    )

   (test-case
    "filter test"

    ;we will actually give names to our test data
    (define s1 (singleton-set 1))
    (define s2 (singleton-set 2))
    (define s3 (singleton-set 3))
    (define s4 (singleton-set 4))
    (define s7 (singleton-set 7))

    (define s1-7 (union (union (union (union s1 s2) s3) s4) s7))
    (define primes-set (filter prime? s1-7))

    (335-check-true (primes-set 2) "set containing primes, given 2")
    (335-check-true (primes-set 3) "set containing primes, given 3")
    (335-check-true (primes-set 7) "set containing primes, given 7")
    (335-check-false (primes-set 1) "set containing primes, given 1")
    (335-check-false (primes-set 4) "set containing primes, given 1")
    (335-check-false (primes-set 8) "set containing primes, given 8")
    )

   (test-case
    "exists? test"

    ;we will actually give names to our test data
    (define s4 (singleton-set 4))
    (define s6 (singleton-set 6))
    (define s7 (singleton-set 7))

    (define test-set (union (union s4 s6) s7))

    (335-check-true (exists? prime? test-set) "exists? a prime")

    (335-check-false (exists? string? test-set) "exists? a string")
    )

   (test-case
    "all? test"

    ;we will actually give names to our test data
    (define s3 (singleton-set 3))
    (define s7 (singleton-set 7))
    (define s13 (singleton-set 13))
    (define s4 (singleton-set 4))

    (define primes (union (union s3 s7) s13))
    (define mixed (union primes s4))

    (335-check-true (all? prime? primes) "all? primes in a primes only set")
    (335-check-false (all? prime? mixed) "all? primes in a mixed set")
    )

   (test-case
    "map-set test"

    ;we will actually give names to our test data
    (define s3 (singleton-set 3))
    (define s7 (singleton-set 7))
    (define s13 (singleton-set 13))

    (define primes (union (union s3 s7) s13))
    (define (add-forty-two x) (+ x 42))

    (define mapped-set (map-set add-forty-two primes))

    (335-check-true (mapped-set 45) "42+3 should be in the set")
    (335-check-true (mapped-set 49) "42+7 should be in the set")
    (335-check-true (mapped-set 55) "42+13 should be in the set")

    (335-check-false (mapped-set 3) "3, original value should not be in the new set")
    (335-check-false (mapped-set 7) "7, original value should not be in the new set")
    (335-check-false (mapped-set 13) "13, original value should not be in the new set")

    (335-check-false (mapped-set 42) "42, some random value should not be in the set")

    )

   )
  )

;======================================05=======================================
(define p5-a
  (test-suite
    "steps-5a"

    (335-check-true
       (up-step-proc? (up-step-proc 3))
       "test 17"
       )

    (335-check-exn
       (up-step-proc "not-a-number")
       "Invalid arguments in: up-step-proc --- expected: number? --- received: not-a-number")

    (335-check-true
       (down-step-proc? (down-step-proc 3))
       "test 18")

    (335-check-exn
       (down-step-proc "not-a-number")
       "Invalid arguments in: down-step-proc --- expected: number? --- received: not-a-number")

    (335-check-true
       (left-step-proc? (left-step-proc 3))
       "test 19")

    (335-check-exn
       (left-step-proc "not-a-number")
       "Invalid arguments in: left-step-proc --- expected: number? --- received: not-a-number")

    (335-check-true
       (right-step-proc? (right-step-proc 3))
       "test 20")

    (335-check-true
       (seq-step-proc? (seq-step-proc (right-step-proc 3) (up-step-proc 4)))
       "test 21")

    (335-check-exn
       (seq-step-proc "not-a-step-proc" (up-step-proc 4))
       "Invalid arguments in: seq-step-proc --- expected: step-proc? --- received: not-a-step-proc")

    (335-check-true
       (step-proc? (up-step-proc 3))
       "test 22")

    (335-check-true
       (step-proc? (down-step-proc 3))
       "test 23")

    (335-check-true
       (step-proc? (left-step-proc 3))
       "test 24")

    (335-check-true
       (step-proc? (right-step-proc 3))
       "test 25")

    (335-check-true
       (step-proc? (seq-step-proc (right-step-proc 3) (up-step-proc 4)))
       "test 26")

    (335-check-equal?
        (single-step-proc->n (up-step-proc 3))
        3
        "test 27"
     )

    (335-check-equal?
        (single-step-proc->n (down-step-proc 3))
        3
        "test 28"
     )

    (335-check-equal?
        (single-step-proc->n (left-step-proc 3))
        3
        "test 29"
     )

    (335-check-equal?
        (single-step-proc->n (right-step-proc 3))
        3
        "test 30"
     )

    (335-check-exn
       (single-step-proc->n "not-a-single-step-proc")
       "Invalid arguments in: single-step-proc->n --- expected: single-step-proc? --- received: not-a-single-step-proc")

    (335-check-equal?
        (single-step-proc->n (seq-step-proc->st-1 (seq-step-proc (left-step-proc 3) (right-step-proc 4))))
        3
        "test 31"
     )

    (335-check-equal?
        (single-step-proc->n (seq-step-proc->st-2 (seq-step-proc (left-step-proc 3) (right-step-proc 4))))
        4
        "test 32"
     )
  )
)

;;======
(define p5-b
  (test-suite
    "steps-5b"
    (335-check-equal?
        (move-proc '(0 0) (up-step-proc 3))
        '(0 3)
        "move up"
     )

    (335-check-equal?
        (move-proc '(0 0) (down-step-proc 3))
        '(0 -3)
        "move down"
     )

    (335-check-equal?
        (move-proc '(0 0) (left-step-proc 3))
        '(-3 0)
        "move left"
     )

    (335-check-equal?
        (move-proc '(0 0) (right-step-proc 3))
        '(3 0)
        "move right"
     )

    (335-check-equal?
        (move-proc '(0 0) (seq-step-proc (up-step-proc 3)(right-step-proc 3)))
        '(3 3)
        "move in sequence: up, right"
     )

    (335-check-equal?
        (move-proc '(0 0) (seq-step-proc (up-step-proc 3)(down-step-proc 3)))
        '(0 0)
        "move in sequence: up, down; they should cancel each other"
     )

    (335-check-equal?
        (move-proc '(0 0) (seq-step-proc (up-step-proc 10) (seq-step-proc (left-step-proc 7) (right-step-proc 4))))
        '(-3 10)
        "move in sequence of sequence: up, left, right; they should cancel each other"
     )

  )
)
