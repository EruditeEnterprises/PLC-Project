;Assignment 11
;Nithin Perumal

(load "chez-init.ss")

;Problem 1
;a
(define-syntax my-let
	(syntax-rules ()
		[(_ ((x v) ...) e1 e2 ...)
			((lambda (x ...) e1 e2 ...)
				v ...)
		]
		[(_ name ((x v) ...) e1 e2 ...) 
			(letrec ([name (lambda (x ...) e1 e2 ...)]) 
				(name v ...))
			;((letrec ([name (lambda (x ...) e1 e2 ...)]) 
			;	name) v ...)
		]
	)
)

;testing:
;(my-let fact ([n 5]) (if (zero? n) 1 (* n (fact (- n 1)))))

(define-syntax my-or
	(syntax-rules ()
		[(_) #f]
		[(_ e) e]
		[(_ e1 e2 ...) (let ((rep e1))
		(if rep rep (my-or e2 ...)))]
	)
)

(define-syntax +=
	(syntax-rules ()
		[(_ a num) 
			(begin (set! a (+ a num)) a)
		]
	)
)

(define-syntax return-first
	(syntax-rules ()
		[(_ e) e]
		[(_ e1 e2 ...) (let ((rep e1))
			e2 ... rep)
		]
	)
)

(define-datatype bintree bintree?
 (leaf-node
 (num integer?))
 (interior-node
 (key symbol?)
 (left-tree bintree?)
 (right-tree bintree?)))

;Problem 2
(define (bintree-to-list tree)
	(cases bintree tree
		[leaf-node (num) (list 'leaf-node num)]
		[interior-node (key left-sub-tree right-sub-tree) 
			(list 'interior-node key
			(bintree-to-list left-sub-tree)
			(bintree-to-list right-sub-tree))
		]
	)
)

;Problem 3
(define (max-interior tree)
	(car (max-interior-cases tree))
)

(define (max-interior-cases tree)
	(cases bintree tree
		[leaf-node (num) (list '() num num)]
		[interior-node (key left-sub-tree right-sub-tree)
			(let* ([leftVal (max-interior-cases left-sub-tree)]
						[rightVal (max-interior-cases right-sub-tree)]
						[lrsum (+ (caddr leftVal) (caddr rightVal))])
						(cond
							((and (or (null? (car leftVal)) (<= (cadr leftVal) lrsum))
								 (or (null? (car rightVal)) (<= (cadr rightVal) lrsum)))
								(list (cadr tree) lrsum lrsum)
							)
							((and (>= (cadr leftVal) (cadr rightVal)) 
									(not (null? (car leftVal))))
								(list (car leftVal) (cadr leftVal) lrsum)
							)
							(else 
								(list (car rightVal) (cadr rightVal) lrsum)
							)
						)
			)
		]
	)
)

;Problem 4

(load "chez-init.ss")

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
	(lambda (datum)
		(cond
 			;[(symbol? datum) (var-exp datum)]
 			;[(number? datum) (num-exp datum)]
 			;[(vector? datum) (vec-exp datum)]
 			[(literal? datum) (lit-exp datum)]
 			[(not (list? datum)) (eopl:error 'parse-exp
              "expression ~s is not a good list" datum)]
 			[(pair? datum)
	  			(cond
	  				[(eqv? (car datum) 'quote) (parse-exp (cdr datum))]
		    		[(eqv? (car datum) 'lambda)
		    			(lambda-parse datum)
		    		]
		    		[(eqv? (car datum) 'set!) 
						(if (equal? (length datum) 3)
							;Something
							(set!-exp (2nd datum) (parse-exp (caddr datum)))
							(eopl:error 'parse-exp "set!-parse: length!=3 ~s" datum)
						)
		    		]
		    		[(eqv? (car datum) 'if) 
						(cond 
							((equal? (length datum) 4)
								(if-then-exp (parse-exp (cadr datum)) 
									(parse-exp (caddr datum)) (parse-exp (cadddr datum)))
							)
							((equal? (length datum) 3) ;if-then
								(no-else-exp (parse-exp (cadr datum)) (parse-exp (caddr datum))))
				        (else (eopl:error 'parse-exp "incorrect #args to ifExp ~s" datum))
						)
		    		]
		    		[(eqv? (car datum) 'let) (let-parse datum)]
		    		[(eqv? (car datum) 'let*) (let-parse datum)]
		    		[(eqv? (car datum) 'letrec) (let-parse datum)]
	   				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
	 			)
	  		]
 			[else (eopl:error 'parse-exp "bad expression: ~s" datum)]
 		)
	)
)

(define lambda-parse
	(lambda (datum)
		(if (< (length datum) 3) ;Error checking
			;Something
      		(eopl:error 'parse-exp "lambda-parse: length < 3 ~s" datum)
			(if (list? (2nd datum)) 
      			(if (andmap symbol? (2nd datum)) 
					(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
		        	(eopl:error 'parse-exp
              			"error in passing args to lambda ~s they need 
              				to be symbols" datum)
				)
          		(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
			)		
		)
	)
)
(define let-parse
	(lambda (datum)
		(if (< (length datum) 3)
	      	(eopl:error 'parse-exp
	            "let/let*/letrec length != 3 ~s" datum)
			(if (list? (cadr datum))
		      	(if (andmap list? (cadr datum))
		          	(if (andmap (lambda (ls) (equal? (length ls) 2)) (cadr datum))
		             	(let ([cad (map cadr (cadr datum))])
			             	(if (map parse-exp cad) 
			                	(if (andmap symbol? (map car (cadr datum))) ;first thing is symbol
			                    	(let-exp (car datum) (cadr datum) (map parse-exp (cddr datum)))
			                      	(eopl:error 'parse-exp "variables putting into the andmap should be symbols ~s" datum))
			              	)
		              	)	
		              	(eopl:error 'parse-exp 
		              		"What your putting in this pair-check is not a pair of 2 ~s" (car datum) datum)
		            )
		          	(eopl:error 'parse-exp
		            	"Andmap input is not a list: error: ~s" datum)
		        )
		        (eopl:error 'parse-exp "(cadr datum) is not a list, : ~s" datum)
		    )
		)
	)
)

(define (literal? id)
	(or (number? id) 
	  (boolean? id) 
	  (null? id) 
	  (string? id) 
	  (symbol? id)
	  (vector? id))
)

(define-datatype expression expression?
	[lit-exp
		(id literal?)
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

(define (sym-or-ls? arg) ;This helps for dealing with the 2 different lambdas
  (if (or (symbol? arg) (list? arg))
      #t)
)


(define unparse-exp
	(lambda (parsed-exp)
		(cases expression parsed-exp
			;(var-exp (id) id)
			;(num-exp (id) id)
			;(vec-exp (id) id)
			(lit-exp (id) id)
			(lambda-exp (id body)
				(append (list 'lambda id) (map unparse-exp body))
			)
			(app-exp (rator rand)
				(append (list (unparse-exp rator)) (map unparse-exp rand))
			)
			(set!-exp (id body)
				(list 'set id (unparse-exp body))
			)
			(if-then-exp (condition true false)
				(list 'if (unparse-exp condition) 
					(unparse-exp true) (unparse-exp false))
			)
			(no-else-exp (condition true)
				(list 'if (unparse-exp condition) (unparse-exp true))
			)
			(let-exp (type bound body) ;To handle all lets
				(append (list type bound) (map unparse-exp body))
			)
		)
	)
)