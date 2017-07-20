(define (make-empty-env) 
    (lambda (x)
        (error "apply-env: unable to find variable" x) ;; the empty environment is the last function evaluated.
    )                                                  ;; since it does not contain a binding, an error is returned instead
)

(define apply-env 
    (lambda (env v) 
        (env v) ;; calls the environment function to see if v is bound. if it is, its value is returned.
    )
)

(define (extend-env v val env)
    (lambda (x)
        (cond
        	((equal? x v) ;; if x is equal to v, then return val
        		val
        	)
        	(else ;; otherwise, call the next environment closure to see if it contains a v/val binding
        		(env x)
        	)
        )
    )
)