#lang racket
(require (lib "eopl.ss" "eopl"))

(define (and-op x y) (and x y))
(define (or-op x y) (or x y))


(define-datatype statement statement?
  (assign (var string?) (expr expression?))
  (global (var string?))
  (return (expr expression?))
  (return_void)
  (pass)
  (break)
  (continue)
  (func (name string?) (params func_param*?) (statements list?))
  (if_stmt (cond_exp expression?) (if_sts list?) (else_sts list?))
  (for_stmt (iter string?) (list_exp expression?) (sts list?))
  (print_stmt (expressions expression*?))
  )

(define-datatype func_param func_param?
  (with_default (var string?) (expr expression?))
  )

(define-datatype func_param* func_param*?
  (empty-param)
  (func_params (param func_param?) (rest-params func_param*?))
  )

(define-datatype expression expression?
  (binary_op (op procedure?) (left expression?) (right expression?))
  (unary_op (op procedure?) (operand expression?))
  (function_call (func expression?) (params expression*?))
  (list_ref (ref expression?) (index expression?))
  (ref (var string?))

  (atomic_bool_exp (bool boolean?))
  (atomic_num_exp (num number?))
  (atomic_null_exp)
  (atomic_list_exp (l expression*?))
)

(define-datatype expression* expression*?
  (empty-expr)
  (expressions (expr expression?) (rest-exprs expression*?))
)

(define-datatype promise promise?
  (a-promise (val expression?) (scope-index number?) (-scopes list?)) 
)

(define (any? x) #t)

(define-datatype environment environment?
  (empty-environment)
  (extended-environment (var string?) (val any?) (env environment?))
)

(define-datatype interp-signal interp-signal?
  (sig-break)
  (sig-continue)
  (sig-void)
  (sig-val (val any?))
)

(provide (all-defined-out))
(#%provide (all-defined))