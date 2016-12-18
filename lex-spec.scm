#lang racket
(require (except-in eopl #%module-begin))

(provide the-lexical-spec)

(define the-lexical-spec
  '((whitespace (whitespace) skip)
    (comment ("--" (arbno (not #\newline))) skip)
    (comment ("--[[" (arbno any) "--]]") skip)
    (identifier ((or letter "_") (arbno (or letter digit "_"))) symbol)
    (num (digit (arbno digit)) number)
    ;(num ("-" digit (arbno digit)) number)
    (string ("\"" (arbno (not #\")) "\"") string)
    (string ("\'" (arbno (not #\')) "\'") string)
  )
)