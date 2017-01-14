#lang racket
(require (except-in eopl #%module-begin))

(provide error-arithmetic error-not-implemented error-syntax-call
error-syntax-else error-compare error-syntax-assignlhs
error-syntax-assignrhs error-invalid-reference error-syntax-callsufs
error-syntax-eof error-multiple-return error-syntax-tableconstructor error-nil-index
error-attempt-index)

;; error-arithmetic : Sym
(define error-arithmetic
  (lambda (type)
    (eopl:error 'error "attempt to perform arithmetic on a ~s value" type)))

;; error-not-implemented : ()
(define error-not-implemented
  (lambda ()
    (eopl:error 'error "feature not yet implemented")))

;; error-syntax-call : Id * Sym
(define error-syntax-call
  (lambda (id type)
    (eopl:error 'syntax-error "attempt to call '~s' (a ~s value)" id type)))

;; error-syntax-callsufs : ()
(define error-syntax-callsufs
  (lambda ()
    (eopl:error 'syntax-error "bad function call")))

;; error-syntax-else : ()
(define error-syntax-else
  (lambda ()
    (eopl:error 'syntax-error "only one else block is permitted")))

;; error-compare : Sym * Sym
(define error-compare
  (lambda (type1 type2)
    (eopl:error 'error "attempt to compare ~s with ~s" type1 type2)))

;; error-syntax-assignlhs : ()
(define error-syntax-assignlhs
  (lambda ()
    (eopl:error 'syntax-error "syntax error near ','")))

;; error-syntax-assignrhs : ()
(define error-syntax-assignrhs
  (lambda ()
    (eopl:error 'syntax-error "unexpected symbol near '='")))

;; error-syntax-eof : ()
(define error-syntax-eof
  (lambda ()
    (eopl:error 'error "syntax error near symbol near <eof>")))

;; error-invalid-reference : ()
(define error-invalid-reference
  (lambda (ref store)
    (eopl:error 'error "invalid reference ~s in store ~s" ref store)))

;; error-multiple-return : ()
(define error-multiple-return
  (lambda ()
    (eopl:error 'syntax-error "<eof> expected near 'return'")))

;; error-syntax-invalidid : ()
(define error-syntax-tableconstructor
  (lambda ()
    (eopl:error 'syntax-error "'}' expected near '='")))

;; error-nil-index : ()
(define error-nil-index
  (lambda ()
    (eopl:error 'error "table index is nil")))

;; error-attempt-index : Id * Sym
(define error-attempt-index
  (lambda (id type)
    (eopl:error 'error "attempt to index '~s' (a ~s value)" id type)))