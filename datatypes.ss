(load "parse.ss")
;; Parsed expression datatypes

(define-datatype expression expression?
  [lit-exp        ; "Normal" data.  Did I leave out any types?
   (datum literal?)]
  [var-exp
    (id symbol?)
  ]
  [lambda-exp
    (id sym-or-ls?)
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
)

	
; datatype for procedures.  At first there is only one
; kind of procedure, but more kinds will be added later.

(define-datatype proc-val proc-val?
  [prim-proc
   (name symbol?)]
)
	 
	 
	 
	
;; environment type definitions

(define scheme-value?
  (lambda (x) #t))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals (list-of scheme-value?))
   (env environment?)))