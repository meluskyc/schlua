#lang racket
(require (except-in eopl #%module-begin))

(require "generated.scm")
(require "datatypes.scm")

(provide exprlist->list parlist->list expval->num expval->bool
expval->string expval->nil expval->proc num-val? bool-val? string-val?
nil-val? proc-val? expval-name)

;; expval->num : ExpVal -> Int
(define expval->num
  (lambda (v)
    (cases expval v
      (num-val (num) num)
      (else (expval-extractor-error 'num v)))))

;; expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (v)
    (cases expval v
      (bool-val (bool) bool)
      (else (expval-extractor-error 'bool v)))))

;; expval->string : ExpVal -> String
(define expval->string
  (lambda (v)
    (cases expval v
      (string-val (str) str)
      (else (expval-extractor-error 'string v)))))

;; expval->nil : ExpVal -> Nil
(define expval->nil
  (lambda (v)
    (cases expval v
      (nil-val () 'nil)
      (else (expval-extractor-error 'string v)))))

;; expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (v)
    (cases expval v
      (proc-val (proc) proc)
      (else (expval-extractor-error 'proc v)))))

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
variant value)))

;; num-val? : ExpVal -> Bool
(define num-val?
  (lambda (e)
    (cases expval e
      (num-val (n) #t)
      (else #f))))

;; bool-val? : ExpVal -> Bool
(define bool-val?
  (lambda (e)
    (cases expval e
      (bool-val (b) #t)
      (else #f))))

;; string-val? : ExpVal -> Bool
(define string-val?
  (lambda (e)
    (cases expval e
      (string-val (s) #t)
      (else #f))))

;; nil-val? : ExpVal -> Bool
(define nil-val?
  (lambda (e)
    (cases expval e
      (nil-val () #t)
      (else #f))))

;; proc-val? : ExpVal -> Bool
(define proc-val?
  (lambda (e)
    (cases expval e
      (proc-val (p) #t)
      (else #f))))

;; expval-name : ExpVal -> symbol
(define expval-name
  (lambda (e)
    (cases expval e
      (num-val (n) 'number)
      (bool-val (b) 'boolean)
      (string-val (s) 'string)
      (nil-val () 'nil)
      (proc-val (p) 'function))))

;; exprlist->list : EXPRLIST -> List(EXPR)
(define exprlist->list
  (lambda (e)
    (cases EXPRLIST e
      (exprlist (first rest) (cons first rest)))))

;; parlist->list : PARLIST -> List(PARAM)
(define parlist->list
  (lambda (p)
    (cases PARLIST p
      (parlist (first rest) (cons first rest)))))