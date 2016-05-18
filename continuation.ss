;For CPS
;(define apply-k
;  (lambda (k v)
;    (k v)
;  )
;)

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