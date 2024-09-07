#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../datatypes.rkt")

(define empty-env (lambda () (empty-environment)))

(define extend-env (lambda (var val env)
    (cases environment env
        (empty-environment () (extended-environment var val env))
        (extended-environment (pvar pval penv)
            (if (equal? var pvar)
                (extended-environment var val penv)
                (extended-environment pvar pval (extend-env var val penv))
            ))
    )))

(define apply-env 
    (lambda (var env)
        (cases environment env
            (empty-environment () (empty-environment))
            (extended-environment (pvar val penv)
                (if (equal? var pvar)
                    val
                    (apply-env var penv)
                )
            )
        )
    )
)


(provide (all-defined-out))
(#%provide (all-defined))