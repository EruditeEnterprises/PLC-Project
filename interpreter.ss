; top-level-eval evaluates a form in the global environment

(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
      [lit-exp (datum) datum]
      [var-exp (id)
        (apply-env env id; look up its value.
          (lambda (x) x) ; procedure to call if id is in the environment 
           (lambda () (eopl:error 'apply-env ; procedure to call if id not in env
              "variable not found in environment: ~s"
         id)))
      ]
      [app-exp (rator rands)
        (let ([proc-value (eval-exp rator env)]
              [args (eval-all rands env)])
          (apply-proc proc-value args))]
      [if-then-exp (condition true false)
        (if (eval-exp condition env)
                  (eval-exp true env)
                  (eval-exp false env)
        )
      ]
      [let-exp (type bound body)
        (let ([new-env 
                (extend-env 
                  (map car bound) 
                  (eval-all (map cadr bound) env)
                  env
                )
              ])
              (let ([bodies (eval-all body new-env)])
                (list-ref bodies (- (length bodies) 1))
              )
        )
      ]
      [lambda-exp (id body)
        (lambda-proc (lambda (args) 
          (let ([new-env (extend-env id args env)])
            (let ([bodies (eval-all body new-env)])
              (list-ref bodies (- (length bodies) 1))
            )
          ))
        )
      ]
      [lambda-spec-exp (indiv rest body)
        (lambda-proc (lambda (args)
          (let 
            ([new-env 
              (extend-env 
                (append indiv (list rest))
                (split-vals args (length indiv))
                env
              )
            ])
              (let ([bodies (eval-all body new-env)])
                (list-ref bodies (- (length bodies) 1))
              )
          ))
        )
      ]
      [no-else-exp (condition true)
        (if (eval-exp condition env)
          (eval-exp true env)
        )
      ]
      [else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])
  )
)

(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-exp (type bound body)
        (cond
          ((equal? type 'let) 
            (app-exp (lambda-exp (map car bound) body) (map cadr bound)))
          ((equal? type 'let*)
            (let*-recursor bound body)
          )
        )
      ]
      [lambda-exp (id body)
        (lambda-exp id (map syntax-expand body))
      ]
      [lambda-spec-exp (indiv-syms rest-sym body)
        (lambda-spec-exp indiv-syms rest-sym (map syntax-expand body))
      ]
      [app-exp (rator rand)
        (app-exp (syntax-expand rator) (map syntax-expand rand))
      ]
      [if-then-exp (condition true false)
        (if-then-exp (syntax-expand condition) (syntax-expand true) (syntax-expand false))
      ]
      [no-else-exp (condition true)
        (no-else-exp (syntax-expand condition) (syntax-expand true))
      ]
      [begin-exp (body)
        (app-exp (lambda-exp '() body) '()) 
      ]
      [cond-exp (conditions else)
        (cond-recursor conditions else)
      ]
      [and-exp (body)
        (and-recursor body)
      ]
      [or-exp (body)
        (or-recursor body)
      ]
      [case-exp (key body else)
        (case-recursor (syntax-expand key) body else)
      ]
      [else exp]
    )
  )
)

(define case-recursor
  (lambda (key exp elsa)
    (if (null? exp)
      (map syntax-expand elsa)
      (if-then-exp (app-exp (var-exp member) (list key (lit-exp (car exp)))) 
        (map syntax-expand (cadr exp)) 
        (case-recursor (cdr exp) elsa)
      )
    )
  )
)

(define let*-recursor
  (lambda (bound body)
    (if (null? bound)
      (syntax-expand body)
      (app-exp 
        (lambda-exp (caar bound) (let*-recursor (cdr bound) body))
        (syntax-expand (cadar bound))
      )
    )
  )
)

(define and-recursor
  (lambda (exp)
    (if (null? exp)
      (lit-exp #t)
      (if-then-exp (syntax-expand (car exp)) 
        (and-recursor (cdr exp)) (lit-exp #f))
    )
  )
)

(define or-recursor
  (lambda (exp)
    (if (null? exp)
      (lit-exp #f)
      (if-then-exp
        (syntax-expand (car exp)) 
        (lit-exp #t)  
        (or-recursor (cdr exp))
      )
    )
  )
)

(define cond-recursor
  (lambda (exp elsa)
    (if (null? exp)
      (map syntax-expand elsa)
      (if-then-exp (syntax-expand (car exp)) (map syntax-expand (cadr exp)) (cond-recursor (cdr exp) elsa))
    )
  )
)

(define (split-vals args index)
  (if (= index 0)
    (list args)
    (cons (car args) (split-vals (cdr args) (- index 1)))
  )
)

; evaluate the list of operands, putting results into a list

(define eval-all
  (lambda (rands env)
    (if (null? rands)
      '()
      (let ([x (eval-exp (car rands) env)])
        (cons x (eval-all (cdr rands) env))
      )
    )
  )
)
    

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.

(define apply-proc
  (lambda (proc-value args)
    (cases proc-val proc-value
      [prim-proc (op) (apply-prim-proc op args)]
      [lambda-proc (proc) (proc args)]
			; You will add other cases
      [else (error 'apply-proc
                   "Attempt to apply bad procedure: ~s" 
                    proc-value)])))

(define *prim-proc-names* '(+ - * / add1 sub1 zero? cons = not < > <= >= cons 
  car cdr list null? assq eq? equal? atom? length list->vector 
  list? pair? procedure? vector->list vector make-vector vector-ref 
  vector? number? symbol? set-car! set-cdr! vector-set! display 
  newline caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr 
  cddar cddr map apply void))

(define init-env         ; for now, our initial global environment only contains 
  (extend-env            ; procedure names.  Recall that an environment associates
     *prim-proc-names*   ;  a value (not an expression) with an identifier.
     (map prim-proc      
          *prim-proc-names*)
     (empty-env)))

; Usually an interpreter must define each 
; built-in procedure individually.  We are "cheating" a little bit.

(define apply-prim-proc
  (lambda (prim-proc args)
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
      [(equal?) (equal? (1st args) (2nd args))]
      [(atom?) (atom? (1st args))]
      [(length) (length (1st args))]
      [(list->vector) (list->vector (1st args))]
      [(list?) (list? (1st args))]
      [(pair?) (pair? (1st args))]
      [(procedure?) (or (procedure? (1st args)) (proc-val? (1st args)))]
      [(vector->list) (vector->list (1st args))]
      [(vector) (apply vector args)] ;multiple args case
      [(make-vector) (make-vector (1st args) (2nd args))]
      [(vector-ref) (vector-ref (1st args) (2nd args))]
      [(vector?) (vector? (1st args))]
      [(number?) (number? (1st args))]
      [(symbol?) (symbol? (1st args))]
      [(set-car!) (set-car! (1st args) (2nd args))]
      [(set-cdr!) (set-cdr! (1st args) (2nd args))]
      ;[(vector-set!) (vector-set! (1st args) (2nd args) (3rd args))]
      [(vector-set!) (apply vector-set! args)]
      [(display) (display (1st args))]
      [(newline) (newline)]
      [(void) (void)]
      [(map) (apply map (get-proc (1st args)) (cdr args))]
      [(apply) (apply (get-proc (1st args)) (cadr args))]
      [else (error 'apply-prim-proc 
            "Bad primitive procedure name: ~s" 
            prim-op)])))

(define get-proc
  (lambda (proc-value)
    (cases proc-val proc-value
      [prim-proc (name) 
                (lambda args (apply-prim-proc name args))
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
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.


(define eval-one-exp
  (lambda (x) 
    ;(top-level-eval (parse-exp x))
    (eval-exp (syntax-expand (parse-exp x)) init-env)
  )
)










