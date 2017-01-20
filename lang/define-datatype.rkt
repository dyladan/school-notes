(define-datatype Env Env?
                 (empty-env)
                 (extend-env (var symbol?) (val number?) (Env Env?)))
