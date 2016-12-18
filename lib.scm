#lang racket
(require (except-in eopl #%module-begin))

(local-require "datatypes.scm")
(local-require "references.scm")

(provide lib-env)

;; lprint : List(ExpVal)
(define lprint
  (lambda (list-to-print)
    (cond
      ((or (null? list-to-print) (void? (car list-to-print))) (printf "\n"))
      ((pair? (car list-to-print))
        (begin
          (if (> (length list-to-print) 1)
              (printf (format-expval (car (car list-to-print))))
              (for-each (lambda (e) (printf (format-expval e))) (car list-to-print)))
          (lprint (cdr list-to-print))))
      (else
        (begin
          (printf (format-expval (car list-to-print)))
          (lprint (cdr list-to-print)))))))

(define proc-lprint
  (proc-val (lib-procedure '(s) lprint)))

(define format-expval
  (lambda (elem)
    (cases expval elem
      (num-val (n) (format "~a\t" n))
      (bool-val (b) (if b "true\t" "false\t"))
      (string-val (s) (format "~a\t" s))
      (nil-val () "nil\t")
      (proc-val (p) "function\t")
      (else "nil\t"))))

(define lib-env (empty-global-env))
(extend-global-env 'print proc-lprint lib-env)