;; Example usage:
;; (define env1
;;    (extend-env 'x -1
;;        (extend-env 'y 4
;;            (extend-env 'x 1
;;                (make-empty-env))))
;; )
;;
;; (define env2
;;     (extend-env 'm -1
;;         (extend-env 'a 4
;;             (make-empty-env)))
;; )
;;
;; (define env3
;;     (extend-env 'q -1
;;         (extend-env 'r 4
;;             (make-empty-env)))
;; )
;; 
;; (myeval '(2 + (3 * x)) env1)         = -1
;; (myeval '(2 + (3 * 1)) env1)         = 5
;; (myeval '((m * a) - 0.1) env2)       = -4.1
;; (myeval '(4 * (s * s)) env3)         = error!, unknown variable s
(define myeval
	(lambda (expr env)
		(cond 
			((list? (car expr)) ;; if the first element of the expression is a list
				(myeval (cons (myeval (car expr) env) (cdr expr)) env) ;; evaluate that sub expression first and return its value, then evaluate the expr again
			)
			((number? (car expr)) ;; if the first element of the expr is a number
				(cond
					((null? (cdr expr)) ;; if there are no other expressions following that number
						(car expr) ;; return that number
					)
					((list? (car (cdr (cdr expr)))) ;; if the following expression is sub expression
						(eval-simple-expr (car (cdr expr)) (list (car expr) (myeval (car (cdr (cdr expr))) env))) ;; simplify and evaluate that sub expression and then evaluate the value of the expression
					)
					((symbol? (car (cdr (cdr expr)))) ;; if the following expression is a symbol
						(eval-simple-expr (car (cdr expr)) (list (car expr) (apply-env env (car (cdr (cdr expr)))))) ;; obtain the value for that symbol/variable from the env and evaluate the expression
					)
					(else ;; otherwise, the following value is a number and can just be evaluated
						(eval-simple-expr (car (cdr expr)) (list (car expr) (car (cdr (cdr expr)))))
					)
				)
			)
			((and (symbol? (car expr)) (or (equal? (car expr) 'dec) (equal? (car expr) 'inc))) ;; if the first element of the expression is either an inc or dec expression
				(cond
					((list? (car (cdr expr))) ;; if the following expression is a subexpression
						(eval-simple-expr (car expr) (list (myeval (car (cdr expr)) env))) ;; simplify and evaluate that subexpression, then evaluate the current expression
					)
					((symbol? (car (cdr expr))) ;; if the following expression is a symbol/variable
						(eval-simple-expr (car expr) (list (apply-env env (car (cdr expr))))) ;; obtain the value for that symbol/variable from the env and evaluate the expression
					)
					(else ;; otherwise, the following expression is a number and can be evaluated immediately
						(eval-simple-expr (car expr) (list (car (cdr expr))))
					)
				)
			)
			((symbol? (car expr)) ;; if the first element of the expression is a symbol/variable 
				(cond
					((null? (cdr expr)) ;; if there are no following expressions
						(apply-env env (car expr)) ;; return the value associated with that variable from the env
					)
					((list? (car (cdr (cdr expr)))) ;; if the following expression is a list
						(eval-simple-expr (car (cdr expr)) (list (apply-env env (car expr)) (myeval (car (cdr (cdr expr))) env))) ;; simplify and evaluate that subexpression, then evaluate the current expression
					)
					((symbol? (car (cdr (cdr expr)))) ;; if the following expression is a symbol/variable
						(eval-simple-expr (car (cdr expr)) (list (apply-env env (car expr)) (apply-env env (car (cdr (cdr expr)))))) ;; obtain the value for that symbol/variable from the env and evaluate the expression
					)
					(else ;; otherwise, the following expression is number and can be evaluated immediately
						(eval-simple-expr (car (cdr expr)) (list (apply-env env (car expr)) (car (cdr (cdr expr)))))
					)
				)
			)
			(else ;; anything else and it produces an error
				(error "myeval: invalid format")
			)
		)
	)
)
;; takes in an operator and a list of numbers to evaluate 
;; and returns a numeric value based on the operator and the list of numbers
;; e.g. (eval-simple-expr '+ '(10 9)) returns 19 
;; e.g. (eval-simple-expr '/ '(16 2)) returns 8
(define eval-simple-expr
	(lambda (operator valuelst)
		(cond
			((equal? operator '+)
				(+ (car valuelst) (car (cdr valuelst)))
			)
			((equal? operator '-)
				(- (car valuelst) (car (cdr valuelst)))
			)
			((equal? operator '/)
				(cond
					((equal? (car (cdr valuelst)) 0)
						(error "eval-simple-expr: division by zero")
					)
					(else
						(/ (car valuelst) (car (cdr valuelst)))
					)
				)
			)
			((equal? operator '*)
				(* (car valuelst) (car (cdr valuelst)))
			)
			((equal? operator '**)
				(expt (car valuelst) (car (cdr valuelst)))
			)
			((equal? operator 'inc)
				(+ (car valuelst) 1)
			)
			((equal? operator 'dec)
				(- (car valuelst) 1)
			)
			(else
				(error "eval-simple-expr: invalid operator")
			)
		)
	)
)