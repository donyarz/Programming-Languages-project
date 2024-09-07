; Group members:
;   1.  Ali Rahmizad: 99100393
;   2.  Ali Safarafard: 99105583
;   3.  Mehrad Milanloo: 99105775

#lang racket

(require (lib "eopl.ss" "eopl"))
(require "../memory/scoping.rkt")
(require "../datatypes.rkt")



(define run 
    (lambda (abstract-syntax-tree)
        (begin
            (renew-scope)
            (interp-ast abstract-syntax-tree (add-scope (init-scope)))
            (void)
        )
    )
)

(define interp-ast 
    (lambda (as-subtree scope-index)
        (if (null? as-subtree) 
            (sig-void)
            (let (
                [root (car as-subtree)]
                [new-subtree (cdr as-subtree)]
                )
                (let ([root-result (run-single-command root scope-index)])
                    (if (interp-signal? root-result)
                        (cases interp-signal root-result
                        (sig-break () (sig-break))
                        (sig-continue () (sig-continue))
                        (sig-void () (interp-ast new-subtree scope-index))
                        (sig-val (value) value)
                        )
                        root-result
                    )
                )
            )
        )
    )
)
(define eval-binary-op 
    (lambda (op left right scope-index)
        (let ([left-value (eval-expr left scope-index)]
              [op-name (object-name op)])
            (cond
                [(eq? op-name '*)
                    (if (zero? left-value)
                        left-value
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'expt)
                    (if (or (eq? left-value 1) (eq? left-value 0)) 
                        left-value
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'and-op)
                    (if (eq? left-value #f)
                        #f
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [(eq? op-name 'or-op)
                    (if (eq? left-value #t)
                        #t
                        (op left-value (eval-expr right scope-index))
                    )
                ]
                [else
                    (op left-value (eval-expr right scope-index))    
                ]
            )
        )
    )
)

(define eval-binary-op-promise 
    (lambda (op left right scope-index -scopes)
        (let ([left-value (eval-expr-promise left scope-index -scopes)]
              [op-name (object-name op)])
            (cond
                [(eq? op-name '*)
                    (if (zero? left-value)
                        left-value
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'expt)
                    (if (or (eq? left-value 1) (eq? left-value 0)) 
                        left-value
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'and-op)
                    (if (eq? left-value #f)
                        #f
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [(eq? op-name 'or-op)
                    (if (eq? left-value #t)
                        #t
                        (op left-value (eval-expr-promise right scope-index -scopes))
                    )
                ]
                [else
                    (op left-value (eval-expr-promise right scope-index -scopes))    
                ]
            )
        )
    )
)

(define eval-exp*
    (lambda (l scope-index)
        (cases expression* l
            (empty-expr () '())
            (expressions (expr rest-exprs) (append (eval-atomic_list_exp rest-exprs scope-index) (list (a-promise expr scope-index scopes))))
        )
    )

)
(define eval-atomic_list_exp
    (lambda (l scope-index)
    (eval-exp* l scope-index)
    )
)

(define (run-a-function params over-params stmts scope-index) 
    (cases eval-func-param* params
        (empty-eval-func-param () (cases eval-func-param* over-params
            (empty-eval-func-param () (interp-ast stmts scope-index))
            (eval-func-params (par rest) (run-a-function over-params (empty-eval-func-param) stmts scope-index) )
            ))
        (eval-func-params (par rest) 
            (begin
                (cases eval-func-param par
                    (eval_with_default (var val si -scopes)
                        (extend-scope scope-index var (if promise? val (a-promise val si -scopes)))
                    )
                )
                (run-a-function rest over-params stmts scope-index) 
            )
        )
    
    )
)

(define run-ref
    (lambda (var scope-index) 
        (let ([r (apply-scope scope-index var)])
            (if (promise? r)
                ( begin
                    (cases promise r
                    (a-promise (expr scope-index -scopes)
                        (eval-expr-promise expr scope-index -scopes)
                    )
                    ))
                r
            )
        )
    )
)

(define run-ref-promise
    (lambda (var scope-index -scopes) 
        (let ([r (apply-scope-on-given-scopes scope-index -scopes var)])
            (if (promise? r)
                (cases promise  r
                    (a-promise (expr scope-index --scopes)
                        (eval-expr-promise expr scope-index --scopes)
                    )
                )
                r
            )
        )
    )
)

(define (eval-expr-promise expr scope-index -scopes)
    (cases expression expr
        (binary_op (op left right) (eval-binary-op-promise op left right scope-index -scopes))
        (unary_op (op operand) (op (eval-expr-promise operand scope-index -scopes)))
        (function_call (func params) (run-function-call-promise func params scope-index -scopes))
        (list_ref (ref index) (eval-list-ref-promise ref index scope-index -scopes))
        (ref (var) (run-ref-promise var scope-index -scopes))
        (atomic_bool_exp (bool) bool)
        (atomic_num_exp (num) num)
        (atomic_null_exp () (sig-void))
        (atomic_list_exp (l) (eval-atomic_list_exp l scope-index))
        (else (display "else3\n"))
    )
)


(define eval-list-ref-promise 
    (lambda (ref index scope-index -scopes)
        (let ([entry (list-ref (eval-expr-promise ref scope-index -scopes) (eval-expr-promise index scope-index -scopes))])
            (if (promise? entry)
                (cases promise  entry
                (a-promise (expr scope-index --scopes)
                    (eval-expr-promise expr scope-index --scopes)
                )
                (else entry)
            )
                (eval-expr-promise entry scope-index -scopes)
            )
        )
    )
)


(define eval-list-ref 
    (lambda (ref index scope-index)
        (let ([entry (list-ref (eval-expr ref scope-index) (eval-expr index scope-index))])
            (if (promise? entry)
                (cases promise  entry
                (a-promise (expr scope-index --scopes)
                    (eval-expr-promise expr scope-index --scopes)
                )
                (else entry)
            )
                (eval-expr entry scope-index)
            )
        )
    )
)

(define (get-last params)
    (cases eval-func-param* params
                (empty-eval-func-param () (display "\n\nerror25\n\n"))
                (eval-func-params (p rest) (cases eval-func-param* rest
                                            (empty-eval-func-param () p)
                                            (eval-func-params (-p -rest) (get-last rest))
                                            ))
            )
)

(define (pop-last params)
    (cases eval-func-param* params
                (empty-eval-func-param () (display "\n\nerror44\n\n"))
                (eval-func-params (p rest) (cases eval-func-param* rest
                                            (empty-eval-func-param () (empty-eval-func-param))
                                            (eval-func-params (-p -rest) (eval-func-params p (pop-last rest)))
                                            ))
            )
)

(define (reverse-params params)
    (cases eval-func-param* params
                (empty-eval-func-param () (empty-eval-func-param))
                (eval-func-params (p rest) 
                    (let
                        ([last-elem (get-last params)])
                        (eval-func-params last-elem (reverse-params (pop-last params)))  
                    )
                )
            )
)

(define 
    (eval-over-params params over-params scope-index -scopes)
    (if (null? over-params )
        (empty-eval-func-param)
        (let ([overrided-value (car over-params)])
            (cases eval-func-param* params
                (empty-eval-func-param () (display "error"))
                (eval-func-params (p rest) (eval-func-params 
                                        (cases eval-func-param p
                                            (eval_with_default (var expr si ss) (eval_with_default var overrided-value si ss))
                                        )
                    (eval-over-params rest (cdr over-params) scope-index -scopes)))
            )
        )
    )

)

(define (run-function-call-promise func over-params scope-index -scopes) 

    (let
        (
            [func-val (eval-expr-promise func scope-index -scopes)]  
        )
            (if (proc? func-val) 
                (cases proc func-val
                    (new-proc (params stmts parent-scope)
                        (let (
                            [new-scope-index (add-scope (get-scope parent-scope))]
                            [params-val (eval-over-params (reverse-params params) (eval-exp* over-params scope-index) scope-index scopes)]  
                            )
                            (run-a-function params params-val stmts new-scope-index)
                        )
                    )
                    
                )
                func-val
            
            )
    )
)

(define (run-function-call func over-params scope-index) 

    (let
        (
            [func-val (eval-expr func scope-index)]  
        )
            (if (proc? func-val) 
                (cases proc func-val
                    (new-proc (params stmts parent-scope)
                        (let (
                            [new-scope-index (add-scope (get-scope parent-scope))]
                            [params-val (eval-over-params (reverse-params params) (eval-exp* over-params scope-index) scope-index scopes)]  
                            )
                            (run-a-function params params-val stmts new-scope-index)
                        )
                    )
                    
                )
                func-val
            
            )
    )
)

(define (eval-expr expr scope-index)
    (cases expression expr
        (binary_op (op left right) (eval-binary-op op left right scope-index))
        (unary_op (op operand) (op (eval-expr operand scope-index)))
        (function_call (func params) (run-function-call func params scope-index))
        (list_ref (ref index) (eval-list-ref ref index scope-index))
        (ref (var) (run-ref var scope-index))
        (atomic_bool_exp (bool) bool)
        (atomic_num_exp (num) num)
        (atomic_null_exp () (sig-void))
        (atomic_list_exp (l) (eval-atomic_list_exp l scope-index))
        (else (display "else2\n"))
    )
)

(define run-assign 
    (lambda (var expr scope-index) 
        (begin
            (let ([index (if
                            (is-global? var scope-index)
                                0
                                scope-index)])
                    (extend-scope index var expr)
            )
            (sig-void)
        )
    )
)

(define run-print 
    (lambda (exprs scope-index)
        (cases expression* exprs
            (empty-expr () (sig-void))
            (expressions (expr rest-exprs) 
                (begin
                    (run-print rest-exprs scope-index)
                    (display (eval-expr expr scope-index))
                    (display "\n")
                    (sig-void)
                )
            )
        )
    )
)

(define run-if
    (lambda (cond_exp if_sts else_sts scope-index)
        (let ([cond-val (eval-expr cond_exp scope-index)])
            (if (and (not (eq? cond-val #f)) (not (eq? cond-val 0)))
                (interp-ast if_sts scope-index)
                (interp-ast else_sts scope-index)
            )
        )
    )
)

(define exec-iter
    (lambda (iter lst sts scope-index)
        (if (null? lst)
            (sig-void)
            (begin
                (extend-scope scope-index iter (car lst))
                (let ([result (interp-ast sts scope-index)]) 
                    (cases interp-signal result
                        (sig-break () (sig-void))
                        (sig-val (value) value)
                        (else (exec-iter iter (cdr lst) sts scope-index))

                    )
                )
                (sig-void)
            )
        )   
    )
)

(define run-for
    (lambda (iter list_exp sts scope-index)
        (let ([lst (eval-expr list_exp scope-index)])
            (exec-iter iter lst sts scope-index)
        )
    )
)

(define (eval-params params scope-index -scopes)
    (cases func_param* params
        (empty-param () (empty-eval-func-param))
        (func_params (p rest) (eval-func-params 
                                (cases func_param p
                                    (with_default (var expr) (eval_with_default var expr scope-index -scopes))
                                )
         (eval-params rest scope-index -scopes)))
    )
)

(define (run-func-declaration name params statements scope-index -scopes) 
        (begin
            (extend-scope scope-index name 
                (new-proc
                    (eval-params params scope-index -scopes)
                    statements
                    scope-index
                )
            )
            (sig-void)
        )
)

(define run-return
    (lambda (expr scope-index) (sig-val (eval-expr expr scope-index)))    
)

(define run-global
    (lambda (var scope-index)
        (begin
            (extend-scope-globals scope-index var)
            (sig-void)
        )
    )
)

(define run-single-command 
    (lambda (command scope-index)
        (cases statement command
        (assign (var expr) (run-assign var expr scope-index))
        (global (var) (run-global var scope-index))
        (return (expr) (run-return expr scope-index))
        (return_void () (sig-val 'None))
        (pass () (sig-void))
        (break () (sig-break))
        (continue () (sig-continue))
        (func (name params statements) (run-func-declaration name params statements scope-index scopes))
        (if_stmt (cond_exp if_sts else_sts) (run-if cond_exp if_sts else_sts scope-index))
        (for_stmt (iter list_exp sts) (run-for iter list_exp sts scope-index))
        (print_stmt (expressions) (run-print expressions scope-index))
        (else (display "error\n"))
        )
    )
)

(provide (all-defined-out))