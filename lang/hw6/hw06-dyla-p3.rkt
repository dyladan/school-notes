#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

;write your answer in string form:
(define problem-1-answer
  "The lexical spec has 4 different types of tokens.
  Whitespace is ignored, anything after a # (including the #)
  is a comment and is ignored. A sequence of one or more digits
  optionally prefixed by a - is a number. An identifier starts
  with a letter and is followed by zero or more letters, digits,
  dashes, underscores, or question marks. Numbers are parsed as
  numbers and identifiers are parsed as symbols"
  )

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program) ; (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up" "(" expr ")") up-expr)
    (expr ("down" "(" expr ")") down-expr)
    (expr ("left" "(" expr ")") left-expr)
    (expr ("right" "(" expr ")") right-expr)
    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("origin?" "(" expr ")") origin-expr)
    (expr ("if" "(" expr ")" "then" expr "else" expr) if-expr)
    (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
    )
  )

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;create-ast function.
(define (run program-string)
  (value-of-program (create-ast program-string))
  )

(define (value-of-program ast)
  (cases program ast
    (a-program (exp1 exps)
               (andmap value-of-expr (flat-list exp1 exps)))))
;(value-of-expr exp1))))

(define (value-of-expr ast)
  (cases expr ast
    (num-expr (num) (num-val num))
    (up-expr (num) (step-val (up-step (num-val->n (value-of-expr num)))))
    (down-expr (num) (step-val (down-step (num-val->n (value-of-expr num)))))
    (left-expr (num) (step-val (left-step (num-val->n (value-of-expr num)))))
    (right-expr (num) (step-val (right-step (num-val->n (value-of-expr num)))))
    (point-expr (num1 num2) (point-val (point (num-val->n (value-of-expr num1)) (num-val->n (value-of-expr num2)))))
    (add-expr (st1 st2) (add-steps (step-val->st (value-of-expr st1)) (step-val->st (value-of-expr st2))))
    (origin-expr (point) (origin? (point-val->p (value-of-expr point))))
    (if-expr (pred tr fl) (if (bool-val->b (value-of-expr pred)) (value-of-expr tr) (value-of-expr fl)))
    (move-expr (pt mv mvs) (point-val (move (point-val->p (value-of-expr pt)) (map value-of-expr (flat-list mv mvs)))))
    ))

(define (apply-step pt st)
  (cases step st
    (up-step (num)
             (point (point->x pt) (+ (point->y pt) (single-step->n st))))
    (down-step (num)
               (point (point->x pt) (- (point->y pt) (single-step->n st))))
    (left-step (num)
               (point (- (point->x pt) (single-step->n st)) (point->y pt)))
    (right-step (num)
                (point (+ (point->x pt) (single-step->n st)) (point->y pt)))))

(define (move pt mvs)
  (cond
    ((null? mvs) pt)
    (else
     (move (apply-step pt (step-val->st (car mvs))) (cdr mvs)))))

(define (origin? point)
  (bool-val (and (zero? (point->x point)) (zero? (point->y point)))))

(define (add-steps st1 st2)
  (convert (cases step st1
    (up-step (num)
             (cases step st2
               (up-step (num2) (up-step (+ num num2)))
               (down-step (num2) (up-step (- num num2)))
               (else (raise (~a "cannot add left or right step to up-step")))))
    (down-step (num)
             (cases step st2
               (down-step (num2) (down-step (+ num num2)))
               (up-step (num2) (down-step (- num num2)))
               (else (raise (~a "cannot add left or right step to down-step")))))
    (left-step (num)
             (cases step st2
               (left-step (num2) (left-step (+ num num2)))
               (right-step (num2) (left-step (- num num2)))
               (else (raise (~a "cannot add up or down step to left-step")))))
    (right-step (num)
             (cases step st2
               (right-step (num2) (right-step (+ num num2)))
               (left-step (num2) (right-step (- num num2)))
               (else (raise (~a "cannot add up or down step to right-step")))))
    )))

(define (convert st)
  (step-val (cases step st
    (up-step (num) (if (negative? num) (down-step (- num)) st))
    (down-step (num) (if (negative? num) (up-step (- num)) st))
    (left-step (num) (if (negative? num) (right-step (- num)) st))
    (right-step (num) (if (negative? num) (left-step (- num)) st))
    )))

;for each different ast node type, e.g. <program>, <expr>, <var-expr> you might
;consider implementing a function with the outline:
#|
(define (value-of-ast-node-type ast)
  (cases ast-node-type ast
    (ast-node-type-variant
     (f1 f2)
     'UNIMPLEMENTED
     )
    (else (raise (~a "value-of-ast-node-type error: unimplemented expression: " ast)))
    )
  )
|#
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;create-ast is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree.
(define create-ast
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the create-ast
;function. This is a one-argument function that takes a
;string.
(define just-scan
  (sllgen:make-string-scanner lexical-spec grammar-spec))
