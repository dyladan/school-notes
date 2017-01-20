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

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program)
    
    (expr (number) num-expr)
    (expr ("up" "(" expr ")") up-expr)
    (expr ("down" "(" expr ")") down-expr)
    (expr ("left" "(" expr ")") left-expr)
    (expr ("right" "(" expr ")") right-expr)
    (expr ("(" expr expr ")") point-expr)
    
    (expr ("+" expr expr) add-expr)
    (expr ("origin?" "(" expr ")") origin-expr)
    (expr ("if" "(" expr ")" "then" expr "else" expr ) if-expr)
    (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
    
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (expr (identifier) iden-expr)
    (var-expr ("val" identifier "=" expr) val)
    (var-expr ("final val" identifier "=" expr) final-val)

    (expr ("cond"  "[" (arbno "(" expr ")" "(" expr ")") "]") cond-expr)
    )
  )

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
   (flatten (list el1 rest)  )
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;create-ast function.
(define (run program-string)
  (if (string? program-string)
      (value-of (create-ast program-string) (empty-env))
      (raise (string-append "expected a program as string, got: " (~a program-string)))
      )  
  )


(define (value-of ast env)
  (cond 
    [(program? ast) (value-of-program ast env)]
    [(expr? ast) (value-of-expr ast env)]
    [(var-expr? ast) (value-of-var ast env)]
    [else (raise (~a "Unimplemented ast node: " ~a ast))]
    )
  )
;================================= program =====================================
(define (value-of-program prog env)
  (cases program prog
    (a-program
     (expr rest-of-expressions)
     ;given a non-predicate function, andmap will apply the function
     ;to every element in the list and then return the value of
     ;applying the function on the last element.
     (andmap (lambda (ex) (value-of ex env))
             (flat-list expr rest-of-expressions))
     )
    )
  )

;=================================== expr =======================================
(define (value-of-expr ex env)
  (or (expr? ex) (raise (string-append "value-of-expr error: expected an expression, got " (~a ex))))
  (cases expr ex
    (num-expr (n) (num-val n))
    
    (up-expr
     (num)
     (step-val (up-step (num-val->n (value-of num env)))))
    
    (down-expr
     (num)
     (step-val (down-step (num-val->n (value-of num env)))))
    
    (left-expr
     (num)
     (step-val (left-step (num-val->n (value-of num env)))))
    
    (right-expr
     (num)
     (step-val (right-step (num-val->n (value-of num env)))))
    
    (iden-expr
     (var-name)
     (apply-env env var-name))
    
    ;(expr ("[" expr expr "]") point-expr)
    (point-expr
     (x y)
     (point-val (point (num-val->n (value-of x env)) (num-val->n (value-of y env))))
     )
    
    ;(expr ("move" "(" expr (arbno expr)")") move-expr)
    (move-expr
     (point-expr first-move rest-of-moves)
     (letrec
         ([start-p (point-val->p (value-of point-expr env))]
          [all-moves-as-expr (map (lambda (ex) (value-of ex env)) (flat-list first-move rest-of-moves))]
          [all-moves-step (map step-val->st all-moves-as-expr)]
          [final-p (foldl move start-p all-moves-step)])
       (point-val final-p)
       )
     )
    
    (add-expr
     (lhs rhs)
     (letrec
         ([l-step-val (value-of lhs env)]
          [r-step-val (value-of rhs env)]
          [l-step (step-val->st l-step-val)]
          [r-step (step-val->st r-step-val)]
          [res (+ (get-axis-value l-step) (get-axis-value r-step))])
       (cond
         [(and (valid-steps-for-add? l-step r-step) 
               (or (left-step? l-step) (right-step? l-step))) 
          (get-horizontal-step res)
          ]
         [(and (valid-steps-for-add? l-step r-step) (or (up-step? l-step) (down-step? l-step)))
          (get-vertical-step res)
          ]
         [else (raise "invalid args in add")]
         )
       )
     )
    
    (origin-expr 
     (p-expr)
     (bool-val (equal? (point-val->p (value-of p-expr env)) (point 0 0)))
     )
    
    (if-expr 
     (cond then-exp else-exp)
     (let
         ([c-val (bool-val->b (value-of cond env))])
       (if c-val
           (value-of then-exp env)
           (value-of else-exp env))
       )
     )
    
    (block-expr
     (lst-of-var-expr lst-of-expr)
     (andmap (lambda (x) (value-of x (build-new-env lst-of-var-expr env)))  lst-of-expr))

    (cond-expr
     (preds values)
     (print preds)
     )
    
    (else (raise (~a "value-of-expr error: unimplemented expression: " ex)))
    )
  )

(define (move st start-p)
  (cases step st
    (up-step (st)
             (point (point->x start-p) (+ (point->y start-p) st)))
    
    (down-step (st)
               (point (point->x start-p) (- (point->y start-p) st)))
    
    (left-step (st)
               (point ( - (point->x start-p) st) (point->y start-p)))
    
    (right-step (st)
                (point ( + (point->x start-p) st) (point->y start-p)))
    
    )
  )


;========================= helpers for add ================================
(define (valid-steps-for-add? st1 st2)
  (or 
   (and (up-step? st1) (up-step? st2))
   (and (down-step? st1) (down-step? st2))
   (and (up-step? st1) (down-step? st2))
   (and (down-step? st1) (up-step? st2))
   
   (and (left-step? st1) (left-step? st2))
   (and (right-step? st1) (right-step? st2))
   (and (left-step? st1) (right-step? st2))
   (and (right-step? st1) (left-step? st2))
   )
  )

(define (get-axis-value st)
  (cases step st
    (up-step (st) st)
    (down-step (st) (* -1 st))
    (left-step (st) (* -1 st))
    (right-step (st) st)
    )
  )

(define (get-vertical-step num)
  (if (positive? num)
      (step-val (up-step num)) 
      (step-val (down-step (* -1 num)))
      )         
  )

(define (get-horizontal-step num)
  (if (positive? num)
      (step-val (right-step num)) 
      (step-val (left-step (* -1 num)))
      )         
  )

;==========================helpers for block-expr==============
(define (build-new-env lst-var-expr old-env)
 (if (null? lst-var-expr)
     old-env
     (build-new-env (cdr lst-var-expr) (one-at-a-time-add (car lst-var-expr) old-env))))

(define (one-at-a-time-add var old-env)
  (cases var-expr var
    (val (iden val-of-iden)
         (extend-env-wrapper iden (value-of val-of-iden old-env) old-env NON-FINAL))
    (final-val (iden val-of-iden)
              (extend-env-wrapper iden (value-of val-of-iden old-env) old-env FINAL))))

;=================================== var =======================================
(define (value-of-var v-ex env)
  (or (var-expr? v-ex) (invalid-args-exception "value-of-var" "var-expr?" v-ex))
  (cases var-expr v-ex
    (val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env NON-FINAL))
    
    (final-val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env FINAL))
    
    (else (raise (~a "value-of-var error: unimplemented expression: " v-ex)))
    )
  )
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST.
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