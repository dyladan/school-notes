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
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* (+ 3 3) 9)
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (* (- 20 (/ 91 7)) (- 45 42)))
)
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "If the entire expression is not wrapped in parenthases, make it so.
Working from the outside in, recursively transform the infix notation into prefix
notation, paying attention to order of operations.
Example ((a) + (b) * (c)) becomes (* (+ (a) (b)) (c)), then apply the same transformation
on a, b, and c."
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (if (= x y z) 0
      (- (+ x y z) (min x y z)))  
)

;======================================05=======================================
(define (p5)
  (if (= x y z) 0
      (- (+ x y z) (max x y z)))  
)

;======================================06=======================================
(define (p6)
  (= x y)  
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "The first expression defines a variable.
The second expression defines a function that returns a constant value."
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "Tells the scheme interpreter not to evaluate the quoted form, thereby treating
code as data rather than as something to be evaluated."
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "The ' tells the interpreter not to evaluate anything in the form Including its
subforms.
The (list ...) function is evaluated like any other form and its subforms
are also evaluated."
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "Strings can be distinguished from each other even when they have the same value.
2 symbols that are the same can never be distinguished between each other. they also
have some performance benefits over strings with regard to memory and comparison."
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list 4 2 6 9)
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))

(define (p11-2)
  '(spaceship
    (name(serenity))
    (class(firefly))
    )
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  '(2 *  ((20 - (91 / 7)) * (45 - 42)))
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (cons 'd lst)
)

;(a b d a b)
(define (p12-2 lst)
  (list (first lst) (second lst) 'd (first lst) (second lst))
)

;(b c d a)
(define (p12-3 lst)
  (list (cadr lst) (caddr lst) 'd (car lst))
)


;======================================13=======================================
(define p13
  "equal? is a function that returns true if the arguments have the same value.
eq? is a function that returns true only if the arguments are the same object in memory."
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
;(define (create-error-msg sym val)
;  (~a "This is a custom error message we will be using next. Symbol '" sym " was not paired with value " val)
;)
(define (create-error-msg sym val)
  (string-append
   "This is a custom error message we will be using next. Symbol '"
   (symbol->string sym)
   " was not paired with value "
   (number->string 42)
   )
)

;======================================15=======================================
(define (check-correctness pair)
  (if (eq? (car pair) 'answer-to-everything)
      (if (not (= (cadr pair) 42))
          (raise (create-error-msg (car pair) (cadr pair)))
          #t)
      #f)
)

;======================================16=======================================
;No answer necessary

