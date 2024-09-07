#lang racket

(require "passes/parser.rkt")
(require "passes/lexer.rkt")
(require "passes/interpreter.rkt")

(define (parse-scan prog-string)
  (python-parser (lex-this prog-string))
  )

(define (print-ast file-name) (display (parse-scan (string-join (file->lines file-name)))))
(define (evaluate file-name)
  (run (parse-scan (string-join (file->lines file-name))))
  )

(evaluate "tests/main-test.py") 

;;; (evaluate "tests/test1.py") 

;;; (evaluate "tests/test2.py")

;;; (evaluate "tests/test3.py")

;;; (evaluate "tests/test4.py")

;;; (evaluate "tests/test5.py")

;;; (evaluate "tests/test6.py")

;;; (evaluate "tests/test7.py")

;;; (evaluate "tests/test8.py")


(provide (all-defined-out))
