(define syntax-expand
  (lambda (exp)
    (cases expression exp
      [let-exp (type bound body)
        (cond
          ((equal? type 'let)
            (app-exp 
              (lambda-exp 
                (map car bound) 
                (map syntax-expand body)
              ) 
              (map syntax-expand (map cadr bound))
            )
          )

          ((equal? type 'let*)
            (car (let*-recursor bound body))
          )
          ((equal? type 'letrec)
            (let-exp 
              type 
              (map (lambda (x) (list (car x) (syntax-expand (cadr x)))) bound)
              (map syntax-expand body)
            )
          )
        )
      ]
      [let-name (name bound body)
        (let-exp
          'letrec
          (list 
            (list name
              (lambda-exp (map car bound) (map syntax-expand body))
            )
          )
          (list (app-exp (var-exp name) (map syntax-expand (map cadr bound))))
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
        (app-exp (lambda-exp '() (map syntax-expand body)) '()) 
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
      [set!-exp (id body)
        (set!-exp id (syntax-expand body))
      ]
      [define-exp (id bodies)
        (define-exp id (map syntax-expand bodies))
      ]
      [else exp]
    )
  )
)

(define case-recursor
  (lambda (key exp elsa)
    (if (null? exp)
      (syntax-expand (begin-exp elsa))
      (if-then-exp 
        (app-exp (var-exp 'member) (list key (lit-exp (caar exp)))) 
        (syntax-expand (begin-exp (cadar exp))) 
        (case-recursor key (cdr exp) elsa)
      )
    )
  )
)

(define let*-recursor
  (lambda (bound body)
    (if (null? bound)
      (map syntax-expand body)
      (list (app-exp 
        (lambda-exp (list (caar bound)) (let*-recursor (cdr bound) body))
        (list (syntax-expand (cadar bound)))
      ))
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
      (let ([current (syntax-expand (car exp))])
        (if-then-exp
          current
          current 
          (or-recursor (cdr exp))
        )
      )
    )
  )
)

(define cond-recursor
  (lambda (exp elsa)
    (if (null? exp)
      (syntax-expand (begin-exp elsa))
      (if-then-exp 
        (syntax-expand (caar exp)) 
        (syntax-expand (begin-exp (cadar exp))) 
        (cond-recursor (cdr exp) elsa)
      )
    )
  )
)