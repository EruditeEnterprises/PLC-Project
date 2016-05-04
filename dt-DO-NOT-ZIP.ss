dt
(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [closure
   (params (list-of symbol?))
   (body (list-of expression?))
   (env environment?)]
  [closure-for-lambda-symbol
    (params (list-of symbol?))
    (body (list-of expression?))
    (env environment?)]
  [closure-for-ref
    (params (list-of expression?))
    (body (list-of expression?))
    (env environment?)
  ]
  [closure-for-lambda-pair
   (params pair?)
   (body (list-of expression?))
   (env environment?)]
)

[var-exp (id)
        ;(apply-env init-env id; look up its value.
    ;      (lambda (x) x) ; procedure to call if id is in the environment 
    ;       (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
      ;        "variable not found in environment: ~s"
       ;  id)))
       id
      ] 