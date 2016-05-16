; Environment definitions for CSSE 304 Scheme interpreter.  Based on EoPL section 2.3
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env k)
    (apply-k k (extended-env-record syms vals env))))

(define extend-env-recursively
  (lambda (ids bodies old-env k)
    (apply-k k (recursively-extended-env-record ids bodies old-env))
  )
)

(define list-find-position
  (lambda (sym los k)
    (list-index 
      (lambda (xsym k) (apply-k k (eqv? sym xsym))) 
      los
      k
    )
  )
)

(define list-index
  (lambda (pred-cps ls k)
    (cond
      [(null? ls) (apply-k k #f)]
   ;  [(pred (car ls)) 0]
   ;  [else (let ((list-index-r (list-index pred (cdr ls))))
	  ;   (if (number? list-index-r)
		 ;(+ 1 list-index-r)
		 ;#f))]
      [else 
        (pred-cps (car ls) 
          (lambda (first) 
            (if first
              (apply-k k 0)
              (list-index pred-cps (cdr ls)
                (lambda (list-index-r)
                  (if (number? list-index-r)
                    (apply-k k (+ 1 list-index-r))
                    (apply-k k #f)
                  )
                )
              )
            )
          )
        )
      ]
    )
  )
)

(load "interpreter.ss")
(define (closure body env k) 
  (eval-exp body env k)
)

(define apply-env
  (lambda (env sym succeed fail) ; succeed and fail are procedures applied if the var is or isn't found, respectively.
    (cases environment env
      [empty-env-record ()
        (fail)]
      [extended-env-record (syms vals env)
  	    ;(let ((pos (list-find-position sym syms)))
       ; 	(if (number? pos)
  	    ;    (succeed (list-ref vals pos))
  	    ;    (apply-env env sym succeed fail)))]
        (list-find-position sym syms 
          (lambda (pos)
            (if (number? pos)
              (apply-k succeed (list-ref vals pos))
              (apply-env env sym succeed fail)
            )
          )
        )
      ]
      [recursively-extended-env-record (ids bodies old-env)
        ;(let ([pos (list-find-position sym ids)])
        ;  (if (number? pos)
        ;    (if (not (expression? (list-ref bodies pos)))
        ;      (list-ref bodies pos)
        ;      (closure (list-ref bodies pos) env)
        ;    )
        ;    (apply-env old-env sym succeed fail)
        ;  )
        ;)
        (list-find-position sym ids
          (lambda (pos)
            (if (number? pos)
              (if (not (expression? (list-ref bodies pos)))
                (apply-k k (list-ref bodies pos))
                (closure (list-ref bodies pos) env succeed)
              )
              (apply-env old-env sym succeed fail)
            )
          )
        )
      ]
    )
  )
)

(define replace-index
  (lambda (ls pos new k)
    (cond 
      [(null? ls) (apply-k k (void))]
      [(= pos 0) (apply-k k (set-car! ls new))]
      [else (replace-index (cdr ls) (- pos 1) new k)]
    )
  )
)
(define set-in-env!
  (lambda (env sym exp k fail)
    (cases environment env
      [empty-env-record ()
        (fail)
      ]
      [extended-env-record (syms vals old-env)
        ;(let ([pos (list-find-position sym syms)])
        ;  (if (number? pos)
        ;    (begin
        ;      (k)
        ;      (replace-index vals pos exp)
        ;    )
        ;    (set-in-env! old-env sym exp k fail)
        ;  )
        ;)
        (list-find-position sym sym 
          (lambda (pos)
            (if (number? pos)
              (replace-index vals pos exp k)
              (set-in-env! old-env sym exp k fail)
            )
          )
        )
      ]
      [recursively-extended-env-record (ids bodies old-env)
        ;(let ([pos (list-find-position sym ids)])
        ;  (if (number? pos)
        ;    (begin
        ;      (k)
        ;      (replace-index bodies pos exp)
        ;    )
        ;    (set-in-env! old-env sym exp k fail)
        ;  )
        ;)
        (list-find-position sym ids 
          (lambda (pos)
            (if (number? pos)
              (replace-index vals pos exp k)
              (set-in-env! old-env sym exp k fail)
            )
          )
        )
      ]
    )
  )
)

