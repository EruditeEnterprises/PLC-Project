;; Parsed expression datatypes

(define-datatype expression expression?
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum literal?)]
  [var-exp
    (id symbol?)]
  [lambda-exp
    (id (list-of symbol?))
    (body (list-of expression?))]
  [lambda-spec-exp
    (indiv-syms (list-of symbol?))
    (rest-sym symbol?)
    (body list?)]
  [app-exp
    (rator expression?)
    (rand list?)]
  [set!-exp
    (id symbol?)
    (body expression?)]
  [if-then-exp
    (condition expression?)
    (true expression?)
    (false expression?)]
  [no-else-exp
    (condition expression?)
    (true expression?)]
  [let-exp
    (type symbol?)
    (bound list?)
    (body (list-of expression?))]
  [begin-exp
    (body (list-of expression?))
  ]
  [cond-exp
    (conditions
      list? 
      ;(list? 
        ;(lambda (x) 
        ;  (and 
        ;    (expression? (car x))
        ;    (list? (cadr x))
        ;  )
        ;)
    ;)
    )
    (else (list-of expression?))
  ]
  [and-exp
    (body (list-of expression?))
  ]
  [or-exp
    (body (list-of expression?))
  ]
  [case-exp
    (key expression?)
    (body list?)
    (else list?)
  ]
  [while-exp
    (test-exp expression?)
    (body list?)
  ]  
)

(define (literal? id)
  (or (number? id) 
    (boolean? id) 
    (null? id) 
    (string? id) 
    (symbol? id)
    (pair? id)
    (vector? id))
)

(define (sym-or-ls? arg) ;This helps for dealing with the 2 different lambdas
  (or (symbol? arg) (pair? arg) (list? arg))
)

; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
  [lambda-proc
    (proc procedure?)
  ]
)
	 
	
; environment type definitions
(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  [empty-env-record]
  [extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)]
   [recursively-extended-env-record
    (proc-names (list-of symbol?))
    (idss (list-of (list-of symbol?)))
    (bodies (list-of expression?))
    (env environment?)]
 )