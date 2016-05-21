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
        [list-ind-k (k)
          (if (number? v)
            (apply-k k (+ 1 v))
            (apply-k k #f)
          )
        ]
        [list-ind-pred-k (pred-cps ls k)
          (if v
            (apply-k k 0)
            (list-index pred-cps (cdr ls) (list-ind-k k))
          )
        ]
        [list-find-pos-k (vals sym env k fail)
          (if (number? v)
            (apply-k k (list-ref vals v))
            (apply-env env sym k fail)
          )
        ]
        [list-find-rec-k (bodies sym env old-env k fail)
          (if (number? v)
            (if (not (expression? (list-ref bodies v)))
              (apply-k k (list-ref bodies v))
              (closure (list-ref bodies v) env k)
            )
            (apply-env old-env sym k fail)
          )
        ]
        [set-env-k (vals exp old-env sym k fail)
          (if (number? v)
            (replace-index vals v exp k)
            (set-in-env! old-env sym exp k fail)
          )
        ]
        [global-add-k (id body bodies proc-names name-clone)
          (apply-k 
            k
            (begin
              (set-cdr! bodies v)
              (set-car! bodies body)
              (set-cdr! proc-names name-clone)
              (set-car! proc-names id)
            )
          )
        ]
        [clone-k (id body bodies proc-names)
          (clone bodies (global-add-k id body bodies proc-names v))
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