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
  [let-name
    (name symbol?)
    (bound list?)
    (body (list-of expression?))
  ]
  [begin-exp
    (body (list-of expression?))
  ]
  [cond-exp
    (conditions list?)
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
  [define-exp
    (name symbol?)
    (body (list-of expression?))
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
    (bodies list?)
    (env environment?)]
)

(define cont-or-proc?
  (lambda (x) 
    (or (continuation? x) (procedure? x))
  )
)

(define-datatype continuation continuation?
  [init-k]
  [append-k (the-car symbol?) (k cont-or-proc?)]
  [if-else-k
    (then-exp expression?)
    (else-exp expression?)
    (env environment?)
    (k cont-or-proc?) ; Changed to continuation later
  ]
  [if-spec-k
    (else-exp expression?)
    (env environment?)
    (k cont-or-proc?)
  ]
  [no-else-k
    (then-exp expression?)
    (env environment?)
    (k cont-or-proc?)
  ]
  [rator-k 
    (rands (list-of expression?))
    (env environment?)
    (k cont-or-proc?)
  ]
  [rands-k
    (proc-value scheme-value?)
    (k cont-or-proc?)
  ]
  [last-body-k
    (env environment?)
    (k cont-or-proc?)
  ]
  [new-env-k 
    (rands (list-of expression?))
    (k cont-or-proc?)
  ]
  [split-k
    (indiv (list-of symbol?))
    (rest symbol?)
    (body (list-of expression?))
    (env environment?)
    (k cont-or-proc?)
  ]
)
