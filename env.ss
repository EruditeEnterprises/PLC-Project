; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define extend-env-recursively
  (lambda (ids bodies old-env)
    (recursively-extended-env-record ids bodies old-env)
  )
)

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (xsym) (eqv? sym xsym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
     ((null? ls) #f)
     ((pred (car ls)) 0)
     (else (let ((list-index-r (list-index pred (cdr ls))))
	     (if (number? list-index-r)
		 (+ 1 list-index-r)
		 #f))))))

(load "interpreter.ss")
(define (closure body env) 
  (eval-exp body env)
)

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
  	    (let ((pos (list-find-position sym syms)))
        	(if (number? pos)
  	        (succeed (list-ref vals pos))
  	        (apply-env env sym succeed fail)))]
      [recursively-extended-env-record
        (ids bodies old-env)
        (let ([pos (list-find-position sym ids)])
          (if (number? pos)
            (if (proc-val? (list-ref bodies pos))
              (list-ref bodies pos)
              (closure (list-ref bodies pos) env)
            )
            (apply-env old-env sym succeed fail)
          )
        )
      ]
    )
  )
)

(define replace-index
  (lambda (ls pos new)
    (cond 
      [(null? ls) (void)]
      [(= pos 0) (set-car! ls new)]
      [else (replace-index (cdr ls) (- pos 1) new)]
    )
  )
)
(define set-in-env!
  (lambda (env sym exp succeed fail)
    (cases environment env
      [empty-env-record ()
        (fail)
      ]
      [extended-env-record (syms vals old-env)
        (let ([pos (list-find-position sym syms)])
          (if (number? pos)
            (begin
              (succeed)
              (replace-index vals pos exp)
            )
            (set-in-env! old-env sym exp succeed fail)
          )
        )
      ]
      [recursively-extended-env-record
        (ids bodies old-env)
        (let ([pos (list-find-position sym ids)])
          (if (number? pos)
            (begin
              (succeed)
              (replace-index bodies pos (parse-exp exp))
            )
            (set-in-env! old-env sym exp succeed fail)
          )
        )
      ]
    )
  )
)

