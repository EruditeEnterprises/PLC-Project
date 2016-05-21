(define *prim-proc-names* '(+ - * / add1 sub1 zero? cons = not < > <= >= cons 
  car cdr list null? assq eq? eqv? equal? atom? length list->vector 
  list? pair? procedure? vector->list vector make-vector vector-ref 
  vector? number? symbol? set-car! set-cdr! vector-set! display 
  newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr 
  cddar cddr map apply void member quotient list-tail append))

(define make-init-env         ; for now, our initial global environment only contains 
  (lambda ()
    (extend-env-recursively            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)
     (init-k)    
    )
  )
)

(define global-env
  (make-init-env)
)

(define reset-global-env
  (lambda () 
    (set! global-env (make-init-env))
  )
)

; top-level-eval evaluates a form in the global environment
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form global-env (init-k))))

; eval-exp is the main component of the interpreter
(define eval-exp
  (lambda (exp env k)
    (cases expression exp
      [lit-exp (datum) (apply-k k datum)]
      [var-exp (id)
        (apply-env 
          env 
          id; look up its value.
          k ;continuation to call if id is in the environment 
          (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s" id)
          )
        )
      ]
      [app-exp (rator rands)
        ;(let ([proc-value (eval-exp rator env)]
        ;      [args (eval-all rands env)])
        ;  (apply-proc proc-value args))
        (eval-exp rator env (rator-k rands env k))
      ]
      [if-then-exp (condition true false)
          ;(let ([first (eval-exp condition env)])
          ;  (if first
          ;    first
          ;    (eval-exp false env)
          ;  )
          ;)
          ;(if (eval-exp condition env)
          ;        (eval-exp true env)
          ;        (eval-exp false env)
          ;)
        (if (eq? condition true)
          (eval-exp condition env (if-spec-k false env k))
          (eval-exp condition env (if-else-k true false env k))
        )
      ]
      [let-exp (type bound body)
        ;(let ([new-env 
        ;        (extend-env-recursively 
        ;          (map car bound) 
        ;          (map cadr bound) 
        ;          env
        ;        )])
        ;  (let ([bodies (eval-all body new-env)])
        ;    (list-ref bodies (- (length bodies) 1))
        ;  )
        ;)
        (extend-env-recursively 
          (map car bound) 
          (map cadr bound) 
          env
          (new-env-k body k)
        )
      ]
      [lambda-exp (id body)
        (apply-k k
          (lambda-proc 
            (lambda (args k) 
              ;(let ([new-env (extend-env id args env)])
              ;  (let ([bodies (eval-all body new-env)])
              ;    (list-ref bodies (- (length bodies) 1))
              ;  )
              ;)
              (extend-env id args env (new-env-k body k))
            )
          )
        )
      ]
      [lambda-spec-exp (indiv rest body)
        (apply-k k
          (lambda-proc 
            (lambda (args k)
              (split-vals args (length indiv) 
                (split-k indiv rest body env k)
              )
            )
          )
        )
      ]
      [no-else-exp (condition true)
        ;(if (eval-exp condition env)
        ;  (eval-exp true env)
        ;)
        (eval-exp condition env (no-else-k true env k))
      ]
      [while-exp (test-exp body)
        (letrec  
          [(while-loop 
            ;(lambda ()
              ;(if (eval-exp test-exp env)
              ;  (begin (eval-all body env) (while-loop))
              ;)
            ;)
            (lambda (k)
              (eval-exp test-exp env (while-k while-loop body env k))
            )
          )
          ] (while-loop k)
        )
      ]
      [set!-exp (id body)
        ;(set-in-env! env id (eval-exp body env)
        ;  void 
        ;  (lambda () (eopl:error 'set-in-env! ; procedure to call if id not in env
        ;      "variable not found in environment: ~s" id)
        ;  )
        ;)
        (eval-exp body env 
          (lambda (evaluated)
            (set-in-env! env id evaluated 
              k
              (lambda () (eopl:error 'set-in-env! ; procedure to call if id not in env
              "variable not found in environment: ~s" id))              
            )
          )
        )
      ]
      [define-exp (id bodies)
        (add-to-global 
          id
          (app-exp (lambda-exp '() bodies) '())
          k
        )
      ]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])
  )
)

(define (clone element k) 
  (map-cps 
    (lambda (x cont) (apply-k cont x)) 
    element
    k 
  )
)

(define (add-to-global id body k)
  (cases environment global-env
    [recursively-extended-env-record  (proc-names bodies env)
      ;(let ([name-clone (clone proc-names)] [bodies-clone (clone bodies)])
      ;  (set-cdr! bodies bodies-clone)
      ;  (set-car! bodies body)
      ;  (set-cdr! proc-names name-clone)
      ;  (set-car! proc-names id)
      ;)
      (clone proc-names (clone-k id body bodies proc-names k))
    ]
    [else 
      (eopl:error 'add-to-global "Global environment corrupted")
    ]
  )
)

(define (split-vals args index k)
  (if (= index 0)
    (apply-k k (list args))
    ;(cons (car args) (split-vals (cdr args) (- index 1)))
    (split-vals (cdr args) (- index 1) (append-k (car args) k))
  )
)

; evaluate the list of operands, putting results into a list
(define eval-all
  (lambda (rands env k)
    (if (null? rands)
      (apply-k k '())
      ;(let ([x (eval-exp (car rands) env)]) ;Gonna have to add a continuation here later
      ;  (cons x (eval-all (cdr rands) env))
      ;)
      (eval-exp (car rands) env (eval-all-k rands env k))
    )
  )
)
    

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
  (lambda (proc-value args k)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args k)]
      [lambda-proc (proc) (proc args k)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.
(define apply-prim-proc
  (lambda (prim-proc args k)
    (case prim-proc
      [(map) (map-cps (lambda (x k) (apply-proc (1st args) (list x) k)) (cadr args) k)]
      [(apply) (apply-proc (1st args) (cadr args) k)]
      [else 
        (apply-k k 
          (case prim-proc
            [(+) (apply + args)]
            [(-) (apply - args)]
            [(*) (apply * args)]
            [(/) (apply / args)]
            [(add1) (+ (1st args) 1)]
            [(sub1) (- (1st args) 1)]
            [(zero?) (zero? (1st args))]
            [(=) (apply = args)]
            [(not) (not (1st args))]
            [(<) (apply < args)]
            [(>) (apply > args)]
            [(<=) (apply <= args)]
            [(>=) (apply >= args)]
            [(cons) (cons (1st args) (2nd args))]
            [(car) (car (1st args))]
            [(cdr) (cdr (1st args))]
            [(caar) (caar (1st args))]
            [(cadr) (cadr (1st args))]
            [(cdar) (cdar (1st args))]
            [(cddr) (cddr (1st args))]
            [(caaar) (caaar (1st args))]
            [(cadar) (cadar (1st args))]
            [(cdaar) (cdaar (1st args))]
            [(cddar) (cddar (1st args))]
            [(caadr) (caadr (1st args))]
            [(caddr) (caddr (1st args))]
            [(cdadr) (cdadr (1st args))]
            [(cdddr) (cdddr (1st args))]
            [(list) (apply list args)]
            [(null?) (null? (1st args))]
            [(assq) (assq (1st args) (2nd args))]
            [(eq?) (eq? (1st args) (2nd args))]
            [(eqv?) (eqv? (1st args) (2nd args))]
            [(equal?) (equal? (1st args) (2nd args))]
            [(atom?) (atom? (1st args))]
            [(length) (length (1st args))]
            [(list->vector) (list->vector (1st args))]
            [(list?) (list? (1st args))]
            [(pair?) (pair? (1st args))]
            [(procedure?) (proc-val? (1st args))]
            [(vector->list) (vector->list (1st args))]
            [(vector) (apply vector args)] ;multiple args case
            [(make-vector) (make-vector (1st args) (2nd args))]
            [(vector-ref) (vector-ref (1st args) (2nd args))]
            [(vector?) (vector? (1st args))]
            [(number?) (number? (1st args))]
            [(symbol?) (symbol? (1st args))]
            [(set-car!) (set-car! (1st args) (2nd args))]
            [(set-cdr!) (set-cdr! (1st args) (2nd args))]
            [(vector-set!) (apply vector-set! args)]
            [(display) (display (1st args))]
            [(newline) (newline)]
            [(void) (void)]
            ;[(map) (map (lambda (x) (apply-proc (1st args) (list x))) (cadr args))]
            ;[(apply) (apply-proc (1st args) (cadr args))]
            [(member) (member (1st args) (2nd args))]
            [(quotient) (apply quotient args)]
            [(list-tail) (list-tail (1st args) (2nd args))]
            [(append) (apply append args)]
            [else (error 'apply-prim-proc 
                  "Bad primitive procedure name: ~s" 
                  prim-op)]
          )
        )
      ]
    )
  )
)

(define get-proc
  (lambda (proc-value)
    (cases proc-val proc-value
      [prim-proc (name) 
                (lambda args (apply-prim-proc name args (lambda (x) x)))
      ]
      [lambda-proc (proc)
                proc
      ]
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)
      ]
    )
  )
)

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (syntax-expand (parse-exp (read))))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.


(define eval-one-exp
  (lambda (x) 
    (top-level-eval (syntax-expand (parse-exp x)))
  )
)










