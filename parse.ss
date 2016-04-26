(load "chez-init.ss")
(load "datatypes.ss")

(define 1st car)
(define 2nd cadr)
(define 3rd caddr)

(define parse-exp         
	(lambda (datum)
		(cond
			[(symbol? datum) (var-exp datum)]
 			[(and (literal? datum) (not (pair? datum))) (lit-exp datum)]
 			[(pair? datum)
	  			(cond
	  				[(eqv? (car datum) 'quote) (lit-exp (cadr datum))]
		    		[(eqv? (car datum) 'lambda)
		    			(lambda-parse datum)
		    		]
		    		[(eqv? (car datum) 'set!) 
						(if (equal? (length datum) 3)
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
		    		[(eqv? (car datum) 'begin)
						(begin-exp (cdr datum))
		    		]
		    		[(eqv? (car datum) 'cond)
		    			(cond-parse datum)
		    		]
	   				[else (app-exp (parse-exp (1st datum)) (map parse-exp (cdr datum)))]
	 			)
	  		]
 			[else (eopl:error 'parse-exp "bad expression: ~s" datum)]
 		)
	)
)

(define cond-parse
	(lambda (datum)
		(if (equal? (car (list-ref datum (- (length datum) 1))) 'else)
			(cond-exp 
				[map (lambda (x) 
					(parse-exp (car x)) (parse-exp (cadr x))) 
						(list-head datum (- 2 (length datum)))
				]
				[parse-exp (cadr (list-ref datum (- (length datum) 1)))]
			)
			(cond-exp
				[map (lambda (x) 
					(parse-exp (car x)) (parse-exp (cadr x))) 
						(list-head datum (- 1 (length datum)))
				]
				[app-exp (var-exp void) '()]
			)			
		)
	)
)

(define lambda-parse
	(lambda (datum)
		(if (< (length datum) 3) ;Error checking
			;Something
      		(eopl:error 'parse-exp "lambda-parse: length < 3 ~s" datum)
			(cond 
				[(list? (2nd datum)) 
	      			(if (andmap symbol? (2nd datum)) 
						(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
			        	(eopl:error 'parse-exp
	              			"error in passing args to lambda ~s they need 
	              				to be symbols" datum)
					)
				]
				[(symbol? (2nd datum))
					(lambda-spec-exp '() (2nd datum) (map parse-exp (cddr datum)))
				]
				[(pair? (2nd datum))
					(let ([split (split-list (2nd datum))])
						(lambda-spec-exp (car split) (cadr split) (map parse-exp (cddr datum)))
					)
				]
          		(lambda-exp (2nd datum) (map parse-exp (cddr datum)))
			)		
		)
	)
)
(define (split-list lst)
	(if (symbol? (cdr lst))
		(list (list (car lst)) (cdr lst))
		(let ([rest (split-list (cdr lst))])
			(list (cons (car lst) (car rest)) (cadr rest))
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
		             	(let* ([cad (map cadr (cadr datum))]
			             		[parsed-bound (map parse-exp cad)])
			                	(if (andmap symbol? (map car (cadr datum))) ;first thing is symbol
			                    	(let-exp (car datum) (map list (map car (cadr datum)) parsed-bound) (map parse-exp (cddr datum)))
			                      	(eopl:error 'parse-exp "variables putting into the andmap should be symbols ~s" datum)
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

(define unparse-exp
	(lambda (parsed-exp)
		(cases expression parsed-exp
			(var-exp (id) id)
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