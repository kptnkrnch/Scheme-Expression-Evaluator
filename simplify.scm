;; Example usage:
;; (simplify '((1 * (a + 0)) + 0))                    = a
;; (simplify '(((a + b) - (a + b)) * (1 * (1 + 0))))  = 0
;; (simplify '((1 * a) + (b * 1)))                    = (a + b)
;; (simplify '((1 * a) + (b * 0)))                    = a
;; (simplify '(z ** (b * (dec 1))))                   = 1
(define simplify
	(lambda (expr)
		(cond 
			((list? (car expr)) ;; if the first part of the expression is a list
				(cond
					((null? (cdr expr)) ;; if there are no other elements in the expressions
						(simplify (car expr)) ;; simplify the first part of the expression list
					)
					((list? (car (cdr (cdr expr)))) ;; if the following part of the expression is a list
						(simplify-simple-expr (car (cdr expr)) (list (simplify (car expr)) (simplify (car (cdr (cdr expr)))))) ;; simplify the first part of the expression, the following part of the expression, and then simplify the expression they are a part of
					)
					((symbol? (car (cdr (cdr expr)))) ;; if the following part of the expression is a symbol/variable
						(simplify-simple-expr (car (cdr expr)) (list (simplify (car expr)) (car (cdr (cdr expr))))) ;; simplify the first part of the expression and then simplify the parent expression with the symbol/variable as the second parameter
					)
					(else ;; otherwise, simplify the initial list expression and then simplify the parent expression with the following number as the second parameter
						(simplify-simple-expr (car (cdr expr)) (list (simplify (car expr)) (car (cdr (cdr expr)))))
					)
				)
			)
			((number? (car expr)) ;; if the first part of the expression is a number
				(cond
					((null? (cdr expr)) ;; if there are no other elements in the expression
						(car expr) ;; return the number
					)
					((list? (car (cdr (cdr expr)))) ;; if the following part of the expression is a list
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (simplify (car (cdr (cdr expr)))))) ;; simplify that list and then simplify the parent expression
					)
					((symbol? (car (cdr (cdr expr)))) ;; if the following part of the expression is a symbol/variable
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (car (cdr (cdr expr))))) ;; simplify the parent expression
					)
					(else ;; otherwise, the following part of the expression is a number, therefore the parent expression can then be simplified
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (car (cdr (cdr expr)))))
					)
				)
			)
			((and (symbol? (car expr)) (or (equal? (car expr) 'dec) (equal? (car expr) 'inc))) ;; if the first part of the expression is either inc or dec
				(cond
					((list? (car (cdr expr))) ;; if the next part of the expression is a list
						(simplify-simple-expr (car expr) (list (simplify (car (cdr expr))))) ;; simplify the next part of the expression and then simplify the dec/inc expression
					)
					((symbol? (car (cdr expr))) ;; if the next part of the expression is a symbol/variable
						(simplify-simple-expr (car expr) (list car (cdr expr))) ;; simplify the dec/inc expression
					)
					(else ;; otherwise, the next part of the expression is a number and the dec/inc expression can be simplified
						(simplify-simple-expr (car expr) (list (car (cdr expr)))) 
					)
				)
			)
			((symbol? (car expr)) ;; if the first part of the expression is a symbol/variable
				(cond
					((null? (cdr expr)) ;; if there are no other elements in the expression
						(car expr) ;; return the symbol/variable
					)
					((list? (car (cdr (cdr expr)))) ;; if the next part of the expression is a list
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (simplify (car (cdr (cdr expr)))))) ;; simplify the next part of the expression and then simplify the main/parent expression
					)
					((symbol? (car (cdr (cdr expr)))) ;; if the next part of the experssion is a symbol/variable
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (car (cdr (cdr expr))))) ;; simplify the main/parent expression
					)
					(else ;; otherwise, the next part of the expression is a number and the main/parent expression can be simplified
						(simplify-simple-expr (car (cdr expr)) (list (car expr) (car (cdr (cdr expr)))))
					)
				)
			)
			(else ;; anything else will produce an error
				(error "simplify: invalid format")
			)
		)
	)
)

;; Takes in an operator and a list of values (either numbers, variables, or expressions)
;; and attempts to simplify them based on a set of rules
;; e.g. (simplify-simple-expr '- '(a a)) returns 0
;; e.g. (simplify-simple-expr '* '(a 1)) returns a
(define simplify-simple-expr
	(lambda (operator valuelst)
		(cond
			((equal? operator '+)
				(cond
					((and (number? (car valuelst)) (number? (car (cdr valuelst))))
						(+ (car valuelst) (car (cdr valuelst)))
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 0))
						(car valuelst)
					)
					((and (number? (car valuelst)) (equal? (car valuelst) 0) (symbol? (car (cdr valuelst))))
						(car (cdr valuelst))
					)
					(else
						(list (car valuelst) operator (car (cdr valuelst)))
					)
				)
			)
			((equal? operator '-)
				(cond
					((and (number? (car valuelst)) (number? (car (cdr valuelst))))
						(- (car valuelst) (car (cdr valuelst)))
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 0))
						(car valuelst)
					)
					((or (and (symbol? (car valuelst)) (symbol? (car (cdr valuelst))) (equal? (car valuelst) (car (cdr valuelst))))
						 (and (list? (car valuelst)) (list? (car (cdr valuelst))) (equal? (car valuelst) (car (cdr valuelst)))))
						0
					)
					(else
						(list (car valuelst) operator (car (cdr valuelst)))
					)
				)
			)
			((equal? operator '/)
				(cond
					((and (number? (car valuelst)) (number? (car (cdr valuelst))))
						(/ (car valuelst) (car (cdr valuelst)))
					)
					(else
						(list (car valuelst) operator (car (cdr valuelst)))
					)
				)
			)
			((equal? operator '*)
				(cond
					((and (number? (car valuelst)) (number? (car (cdr valuelst))))
						(* (car valuelst) (car (cdr valuelst)))
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 1))
						(car valuelst)
					)
					((and (number? (car valuelst)) (equal? (car valuelst) 1) (symbol? (car (cdr valuelst))))
						(car (cdr valuelst))
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 0))
						0
					)
					((and (number? (car valuelst)) (equal? (car valuelst) 0) (symbol? (car (cdr valuelst))))
						0
					)
					(else
						(list (car valuelst) operator (car (cdr valuelst)))
					)
				)
			)
			((equal? operator '**)
				(cond
					((and (number? (car valuelst)) (number? (car (cdr valuelst))))
						(expt (car valuelst) (car (cdr valuelst)))
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 0))
						1
					)
					((and (number? (car valuelst)) (equal? (car valuelst) 1) (symbol? (car (cdr valuelst))))
						1
					)
					((and (symbol? (car valuelst)) (number? (car (cdr valuelst))) (equal? (car (cdr valuelst)) 1))
						(car valuelst)
					)
					(else
						(list (car valuelst) operator (car (cdr valuelst)))
					)
				)
			)
			((equal? operator 'inc)
				(cond
					((number? (car valuelst))
						(+ (car valuelst) 1)
					)
					(else
						(list operator (car valuelst))
					)
				)
			)
			((equal? operator 'dec)
				(cond
					((number? (car valuelst))
						(- (car valuelst) 1)
					)
					(else
						(list operator (car valuelst))
					)
				)
			)
			(else
				(error "simplify-simple-expr: invalid operator")
			)
		)
	)
)