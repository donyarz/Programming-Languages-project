#lang racket
(require (lib "eopl.ss" "eopl"))
(require "../datatypes.rkt")
(require "environment.rkt")


(define scopes '())

(define scope-index? (lambda (n) (< n (length scopes))))



(define-datatype scope scope?
  (the-scope (env environment?) (upper-scope-index scope-index?) (globals list?))
  )

(define (scope->env s)
  (cases scope s
    (the-scope (env upper-scope-index globals) env)
    ))

(define (scope->upper s)
  (cases scope s
    (the-scope (env upper-scope-index globals) upper-scope-index)
    ))

(define (scope->globals s)
  (cases scope s
    (the-scope (env upper-scope-index globals) globals)
))


(define (extend-scope-globals scope-index var)
    (let ([-scope (get-scope scope-index)])
        (set-scope scope-index
            (the-scope (scope->env -scope) (scope->upper -scope) (append (scope->globals -scope) (list var)))
        )
    )
  )


(define correct-index? (lambda (n) (< n (length scopes))))

(define (get-scope index)
  (get-scope-on-given-scopes index scopes)
)

(define (get-scope-on-given-scopes index -scopes)
  (list-ref -scopes index)
)

(define (set-scope index s)
  (set! scopes
        (list-set scopes index s)
        ))

(define (add-scope s)
  (begin
    (set! scopes (append scopes (list s)))
    (- (length scopes) 1)
    ))

(define (renew-scope) (set! scopes '()))

(define (init-scope) (the-scope (empty-environment) -1 '()))

(define (apply-scope scope-index var)
  (apply-scope-on-given-scopes scope-index scopes var)
)

(define (apply-scope-on-given-scopes scope-index -scopes var)
    (
   let ([my-scope (get-scope-on-given-scopes scope-index -scopes)])
    (
     if (is-global-on-given-scopes? var scope-index -scopes) 
     (apply-scope-on-given-scopes 0 -scopes var)
     (
      let ([res (apply-env var (scope->env my-scope))])
       (cond
         [(not (equal? res (empty-environment))) res]
         [(>= (scope->upper my-scope) 0) (apply-scope-on-given-scopes (scope->upper my-scope) -scopes var)]
         [else (eopl:error 'binding-error!
            "\n\tIdentifier ~s is used before declaration!" var)]
         )
      )
     )
    )
)

(define (extend-scope scope-index var value)
    (let ([current-scope (get-scope scope-index)])
        (let ([current-env (scope->env current-scope)])
            (let ([new-env (extend-env var (cond 
                                                [(promise? value) value]
                                                [(proc? value) value]
                                                [else (a-promise value scope-index scopes)]
                                                )
                                                 current-env)])
                (set-scope scope-index 
                    (the-scope new-env 
                        (scope->upper current-scope)
                        (scope->globals current-scope)
                    )
                )
            )
        )
    )
)

(define (is-global? var scope-index)
  (is-global-on-given-scopes? var scope-index scopes)
)

(define (is-global-on-given-scopes? var scope-index -scopes)
  (member var (scope->globals (get-scope-on-given-scopes scope-index -scopes)))
)

(define-datatype proc proc?
  (new-proc
   (params eval-func-param*?)
   (statements list?)
   (parent-scope scope-index?)
   )
  )

(define-datatype eval-func-param eval-func-param?
  (eval_with_default (var string?) (val any?) (scope-index number?) (-scopes list?))
  )

(define-datatype eval-func-param* eval-func-param*?
  (empty-eval-func-param)
  (eval-func-params (eval-param eval-func-param?) (rest-evals eval-func-param*?))
  )


(provide (all-defined-out))