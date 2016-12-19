#lang racket
(require (except-in eopl #%module-begin))

(local-require "references.scm")
(require "generated.scm")
(require data/gvector)

(provide global-env global-env? global-env-record empty-global-env apply-global-env
global-env-pos extend-local-env extend-local-env* extend-global-env extend-global-env* local-env? empty-local-env-record
ext-local-env-record empty-local-env apply-local-env apply-env extend-env extend-env*
proc proc? procedure lib-procedure expval expval? num-val bool-val string-val nil-val
proc-val table-val make-table table-set! table-get)

;; expressed values
(define-datatype expval expval?
  (num-val
    (value number?))
  (bool-val
    (boolean boolean?))
  (string-val
    (string string?))
  (nil-val)
  (proc-val
    (proc proc?))
  (table-val
    (table table?)))

;; proc? : SchemeVal -> Bool
;; procedure : Var * Exp * Env -> Proc
(define-datatype proc proc?
  (procedure
    (params (list-of symbol?))
    (body BLOCK?)
    (env local-env?))
  (lib-procedure
    (params (list-of symbol?))
    (function procedure?)))

;; table? : ExpVal -> Bool
(define table?
  (lambda (t) (hash? t)))

;; make-table : () -> Table
;; construct an empty table
(define make-table
  (lambda ()
    (make-hash)))

;; table-set! : Table * ExpVal * ExpVal
(define table-set!
  (lambda (tbl fld val)
    (hash-set! tbl fld val)))

;; table-get : Table * ExpVal
(define table-get
  (lambda (tbl fld)
    (hash-ref tbl fld (nil-val))))

;;;;;;;;;;; global environment ;;;;;;;;;;;

(define-datatype global-env global-env?
  (global-env-record
    (syms gvector?)
    (vals gvector?)))

(define empty-global-env
  (lambda ()
    (global-env-record (make-gvector) (make-gvector))))

;; apply-global-env : GlobalEnv * Sym -> ExpVal
;; retrieve from global and dereference
(define apply-global-env
  (lambda (env sym)
    (let ((ref (apply-global-env-ref env sym)))
      (if ref (deref ref) (nil-val)))))

;; apply-global-env-ref : GlobalEnv * Sym -> Ref
;; retrieve ref from global, #f if not found
(define apply-global-env-ref
  (lambda (env sym)
    (cases global-env env
      (global-env-record (syms vals)
        (for/first
          ((v (in-gvector vals))
          (s (in-gvector syms)) #:when (eq? s sym)) v)))))

;; global-env-pos : GlobalEnv * Sym -> Int
;; get position of sym in env, #f if not found
(define global-env-pos
  (lambda (env sym)
    (cases global-env env
      (global-env-record (syms vals)
        (for/first
          ((i (in-naturals 0))
          (s (in-gvector syms)) #:when (eq? s sym)) i)))))

;; extend-global-env : Sym * ExpVal * GlobalEnv
;; in global env -> update store ref
;; otherwise add to global
(define extend-global-env
  (lambda (sym val env)
    (cases global-env env
      (global-env-record (syms vals)
        (let ((pos (global-env-pos env sym)))
          (if pos
            (setref! (gvector-ref vals pos) val)
            (begin
              (gvector-add! syms sym)
              (gvector-add! vals (newref val)))))))))

;; extend-global-env : List(Sym) * List(ExpVal) * GlobalEnv
(define extend-global-env*
  (lambda (syms vals env)
    (cases global-env env
      (global-env-record (s v)
        (letrec ((loop (lambda (syms2 vals2)
          (cond
            ((null? syms2) (void))
            ((null? vals2) ; TODO: move this logic to start.scm
              (begin
                (extend-global-env (car syms2) (nil-val) env)
                (loop (cdr syms2) vals2)))
            (else
              (begin
                (extend-global-env (car syms2) (car vals2) env)
                (loop (cdr syms2) (cdr vals2))))))))
        (loop syms vals))))))

;;;;;;;;;;; local environment ;;;;;;;;;;;

(define empty-local-env-record
  (lambda ()
    '()))

(define ext-local-env-record
  (lambda (sym val old-env)
    (cons (list sym val) old-env)))

(define empty-local-env-record? null?)

(define local-env?
  (lambda (x)
    (or (empty-local-env-record? x)
        (and (pair? x)
              (symbol? (car (car x)))
              (reference? (cadr (car x)))
              (local-env? (cdr x))))))

(define ext-local-env-record->sym
  (lambda (r)
    (car (car r))))

(define ext-local-env-record->val
  (lambda (r)
    (cadr (car r))))

(define ext-local-env-record->old-env
  (lambda (r)
    (cdr r)))

(define empty-local-env
  (lambda ()
    (empty-local-env-record)))

(define empty-local-env?
  (lambda (x)
    (empty-local-env-record? x)))


;; extend-local-env : Sym * ExpVal * LocalEnv -> LocalEnv
;; in local env? -> update ref and return env
;; otherwise create ref and return env
(define extend-local-env
  (lambda (sym val old-env)
    (let ((local-ref (apply-local-env-ref old-env sym)))
      (if local-ref
        (begin
          (setref! local-ref val)
          old-env)
        (ext-local-env-record sym (newref val) old-env)))))

;; extend-local-env : List(Sym) * List(ExpVal) * LocalEnv -> LocalEnv
(define extend-local-env*
  (lambda (syms vals old-env)
    (letrec ((loop
      (lambda (syms2 vals2 env2)
        (if (or (null? syms2) (null? vals2)) env2
          (extend-local-env (car syms2) (car vals2)
            (loop (cdr syms2) (cdr vals2) env2))))))
      (loop syms vals old-env))))

;; apply-local-env : LocalEnv * Sym -> ExpVal
(define apply-local-env
  (lambda (env search-sym)
    (let ((ref (apply-local-env-ref env search-sym)))
      (if ref (deref ref) (nil-val)))))

;; apply-local-env-ref : LocalEnv * Sym -> Ref
;; #f if not found
(define apply-local-env-ref
  (lambda (env search-sym)
    (if (empty-local-env? env) #f
      (let ((sym (ext-local-env-record->sym env))
            (val (ext-local-env-record->val env))
            (old-env (ext-local-env-record->old-env env)))
        (if (eqv? search-sym sym)
          val
          (apply-local-env-ref old-env search-sym))))))

;; apply-env : LocalEnv * GlobalEnv * Sym -> ExpVal
;; if       in local env? -> return expval
;; elseif   in global env? -> return expval
;; else     return nil
(define apply-env
  (lambda (lenv genv id)
    (let ((local-ref (apply-local-env-ref lenv id)))
      (if local-ref (deref local-ref)
        (let ((global-ref (apply-global-env-ref genv id)))
          (if global-ref (deref global-ref) (nil-val)))))))

;; extend-env : LocalEnv * GlobalEnv * Sym * Val
;; if       in local env? -> update the reference
;; elseif   in global env? -> update the reference
;; else     add to global
(define extend-env
  (lambda (lenv genv id val)
    (let ((local-ref (apply-local-env-ref lenv id)))
      (if local-ref (setref! local-ref val)
        (extend-global-env id val genv)))))

;; extend-env* : LocalEnv * GlobalEnv * List(Sym) * List(Val)
(define extend-env*
  (lambda (lenv genv ids vals)
    (cond
      ((null? ids) (void))
      ((null? vals) ;; TODO: move this logic to start
        (begin
          (extend-global-env (car ids) (nil-val) genv)
          (extend-env* lenv genv (cdr ids) vals)))
      (else
        (begin
          (extend-global-env (car ids) (car vals) genv)
          (extend-env* lenv genv (cdr ids) (cdr vals)))))))