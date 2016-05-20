;For CPS

(define apply-k
  (lambda (k v)
    (if (procedure? k)
      (k v)
      (cases continuation k
        [init-k ()
          v
        ]
        [append-k (the-car k)
          (apply-k k (cons the-car v))
        ]
        [if-else-k (then-exp else-exp env k)
          (if v
            (eval-exp then-exp env k)
            (eval-exp else-exp env k)
          )
        ]
        [if-spec-k (else-exp env k)
          (if v
              (apply-k k v)
              (eval-exp else-exp env k)
          )
        ]
        [no-else-k (then-exp env k)
          (if v
            (eval-exp then-exp env k)
            (apply-k k (void))
          )
        ]
        [rator-k (rands env k)
          (eval-all rands env (rands-k v k))
        ]
        [rands-k (proc-value k)
          (apply-proc proc-value v k)
        ]
        [last-body-k (env k)
          (apply-k k (list-ref v (- (length v) 1)))
        ]
        [new-env-k (rands k)
          (eval-all rands v (last-body-k v k))
        ]
        [split-k (indiv rest rands env k)
          (extend-env 
            (append indiv (list rest)) 
            v 
            env
            (new-env-k rands k)
          )
        ]
        [while-k (while-proc rands env k)
          (if v
            (eval-all rands env (proc-k while-proc k))
            (apply-k k (void))
          )
        ]
        [proc-k (proc k)
          (proc k)
        ]
      )
    )
  )
)

(define (map-cps proc-cps ls k)
  (if (null? ls)
    (apply-k k '())
    (map-cps proc-cps (cdr ls)
      (lambda (map-result)
        (proc-cps (car ls)
          (lambda (v)
            (apply-k k
              (cons v map-result))))))))