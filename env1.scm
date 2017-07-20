(define make-empty-env
	(lambda ()
		(list) ;; creates an empty environment as a list
	)
)

(define apply-env
	(lambda (env v)
		(cond 
			((null? env) ;; if the environment is empty, return an error
				(error "apply-env: unable to find variable" v)
			)
			((equal? (car (car env)) v) ;; if the variable v is equal to the first element of the first binding in the environment
				(car (cdr (car env))) ;; return the value for that binding
			)
			(else
				(apply-env (cdr env) v) ;; otherwise, recursively search the list of bindings by removing the first one from the list
			)
		)
	)
)

(define extend-env
	(lambda (v val env) 
		(cons (list v val) env) ;; append the (v val) binding to the environment list
	)
)