#lang racket
(require (except-in eopl #%module-begin))
(provide (all-from-out eopl))

(require "generated.scm")
(require "datatypes.scm")
(require "lib.scm")
(require "references.scm")
(require "utils.scm")
(require "errors.scm")

;; global env
(define genv lib-env)

;; empty local env
(define lenv (empty-local-env))

;; run : String
(define run
  (lambda (string)
    (eval-block (scan&parse string) lenv)))

;; eval-block : BLOCK * LocalEnv
(define eval-block
  (lambda (blk lenv)
    (cases BLOCK blk
      (block (chnk)
        (eval-chunk chnk lenv)))))

;; eval-chunk : CHUNK * LocalEnv
(define eval-chunk
  (lambda (chnk lenv)
    (cases CHUNK chnk
      (chunk (stats opt-semi1 lstat opt-semi2)
        (letrec ((loop
          (lambda (s env temp)
            (if (null? s)
              (if (null? lstat)
                temp
                (if (> (length lstat) 1) (error-multiple-return)
                  (eval-lstat (car lstat) lenv)))
              (cases STAT (car s)
                (localstat (suf) (loop (cdr s) (eval-localstat suf env) temp))
                (else
                  (loop (cdr s) env (eval-stat (car s) env))))))))
          (loop stats lenv (void)))))))

;; eval-lstat : LASTSTAT * LocalEnv
(define eval-lstat
  (lambda (lstat lenv)
    (cases LASTSTAT lstat
      (returnstat (first rest)
        (map (lambda (e) (eval-expr e lenv)) (cons first rest)))
      (breakstat () (void)))))

;; eval-stat : STAT * LocalEnv
(define eval-stat
  (lambda (stat lenv)
    (cases STAT stat
      (dostat (a) (eval-dostat a lenv))
      (whilestat (a b) (eval-whilestat a b lenv))
      (repeatstat (a b) (eval-repeatstat a b lenv))
      (ifstat (a b c d e) (eval-ifstat a b c d e lenv))
      (callassignstat (a b c) (eval-callassignstat a b c lenv))
      (functionstat (a b c d) (eval-functionstat a b c d #f lenv))
      (forstat (a b c) (eval-forstat a b c lenv))
      (else (error-not-implemented)))))

;; eval-localstat : STATLOCALSUF * LocalEnv -> LocalEnv
(define eval-localstat
  (lambda (suf lenv)
    (cases STATLOCALSUF suf
      (localfunction (id dotids colonids bdy) (eval-functionstat id dotids colonids bdy #t lenv))
      (localassign (firstid moreids firstexpr moreexprs)
        (eval-assignment (cons firstid moreids) (cons firstexpr moreexprs) #t lenv)))))

;; eval-assignstat : List(PRIMARYEXPR) * List(EXPR) * LocalEnv
(define eval-assignstat
  (lambda (pexplist exprlist lenv)
    (eval-assignment (map (lambda (p) (eval-var p lenv)) pexplist) exprlist #f lenv)))

;; eval-assignment : List * List(EXPR) * Bool * LocalEnv
;; TODO: clean this up
(define eval-assignment
  (lambda (names exprs local? lenv)
    (cond
      ((null? names) (if local? lenv (void)))
      ((null? exprs)
        (if local?
          (eval-assignment (cdr names) exprs (extend-local-env (car names) (nil-val) lenv))
          (begin
            (finish-global-assign (car names) (nil-val) lenv)
            (eval-assignment (cdr names) exprs local? lenv))))
      (else
        (let ((exp (eval-expr (car exprs) lenv)))
          (if (pair? exp)
            (if (> (length exprs) 1)
              (if local?
                (eval-assignment (cdr names) (cdr exprs) (extend-local-env (car names) (car exp) lenv))
                (begin
                  (finish-global-assign (car names) (car exp) lenv)
                  (eval-assignment (cdr names) (cdr exprs) lenv)))
              (if local?
                (letrec ((loop (lambda (names2 exprs2 lenv2)
                  (cond
                    ((null? names2) lenv2)
                    ((null? exprs2)
                      (loop (cdr names2) exprs2 (extend-local-env (car names2) (nil-val) lenv2)))
                    (else
                      (loop (cdr names2) (cdr exprs2) (extend-local-env (car names2) (car exprs2) lenv2)))))))
                (loop names exp lenv))
                (letrec ((loop (lambda (names2 exprs2)
                  (cond
                    ((null? names2) (void))
                    ((null? exprs2)
                      (begin
                        (finish-global-assign (car names2) (nil-val) lenv)
                        (loop (cdr names2) exprs2)))
                    (else
                      (begin
                        (finish-global-assign (car names2) (car exprs2) lenv)
                        (loop (cdr names2) (cdr exprs2))))))))
                (loop names exp lenv))))
            (if local?
              (eval-assignment (cdr names) (cdr exprs) local? (extend-local-env (car names) exp lenv))
              (begin
                (finish-global-assign (car names) exp lenv)
                (eval-assignment (cdr names) (cdr exprs) local? lenv)))))))))

;; eval-var : PRIMARYEXP * LocalEnv
(define eval-var
  (lambda (primaryexp lenv)
    (cases PRIMARYEXPR primaryexp
      (pexpr (pre list-sufs)
        (cases PREFIXEXPR pre
          (prefixid (id)
            (let ((len-sufs (length list-sufs)))
              (cond
                ((> len-sufs 1) (error-not-implemented))
                ((= 1 len-sufs) (eval-var-suf id (car list-sufs) lenv))
                ((zero? len-sufs) id)))))))))

;; eval-var-suf : Id * PRIMARYEXPRSUF * Bool * LocalEnv
(define eval-var-suf
  (lambda (tbl-name suf lenv)
    (let ((tbl (apply-env lenv genv tbl-name)))
      (if (table-val? tbl)
        (cases PRIMARYEXPRSUF suf
          (pexprdot (fld-name) (list tbl fld-name))
          (pexprbracket (exp) (list tbl (eval-expr exp lenv)))
          (else (error-not-implemented)))
        (error-attempt-index tbl-name (expval-name tbl))))))

(define finish-global-assign
  (lambda (var val lenv)
    (if (pair? var) ;; it's a field assignment
      (table-set! (expval->table (car var)) (cadr var) val)
      (extend-env lenv genv var val))))

;; eval-functionstat : Id * List(Id) * List(Id) * BODY * LocalEnv
(define eval-functionstat
  (lambda (name dotids colonids bdy local? lenv)
    (if (or (pair? dotids) (pair? colonids)) (error-not-implemented)
      (cases BODY bdy
        (funcbody (pars blk)
          (let ((params (if (null? pars) '()
            (map (lambda (p) (eval-param p lenv)) (parlist->list (car pars))))))
          (if local? (extend-local-env name (proc-val (procedure params blk lenv)) lenv)
            (extend-global-env
              name
              (proc-val (procedure params blk lenv))
              genv))))))))

;; eval-callassignstat : PRIMARYEXPR * List(PRIMARYEXPR) * List(EXPRLIST)
(define eval-callassignstat
  (lambda (lhs more-vars list-rhs lenv)
    (let ((len-sufs (length more-vars)) (len-rhs (length list-rhs)))
      (if (zero? len-rhs) ; call
        (if (zero? len-sufs)
          (eval-callstat lhs lenv)
          (error-syntax-assignlhs))
        (if (= 1 len-rhs) ; assignment
          (eval-assignstat (cons lhs more-vars) (exprlist->list (car list-rhs)) lenv)
          (error-syntax-assignrhs))))))

;; eval-callstat : PRIMARYEXPR * LocalEnv
(define eval-callstat
  (lambda (pexp lenv)
    (cases PRIMARYEXPR pexp
      (pexpr (pre sufs)
        (if (= 1 (length sufs))
          (eval-primaryexpr-suf pre (car sufs) lenv)
          (error-syntax-eof))))))

;; eval-primaryexpr-suf : PREFIXEXPR * PRIMARYEXPRSUF * LocalEnv
(define eval-primaryexpr-suf
  (lambda (pre suf lenv)
    (cases PRIMARYEXPRSUF suf
      (pexprargs (args)
        (cases FUNCARGS args
          (funcargs (list-exprlist)
            (letrec (
              (name (eval-prefixexpr pre lenv))
              (proc1 (apply-env lenv genv name))
              (exprs (if (null? list-exprlist) '()
                  (exprlist->list (car list-exprlist)))))
            (apply-procedure name
              proc1 (map (lambda (e) (eval-expr e lenv)) exprs))))))
      (pexprdot (id)
        (let* (
          (tbl-name (eval-prefixexpr pre lenv))
          (tbl (apply-env lenv genv tbl-name)))
        (if (table-val? tbl)
          (table-get (expval->table tbl) id)
          (error-attempt-index tbl-name (expval-name tbl)))))
      (pexprbracket (exp)
        (let* (
          (tbl-name (eval-prefixexpr pre lenv))
          (tbl (apply-env lenv genv tbl-name)))
        (if (table-val? tbl)
          (table-get (expval->table tbl) (eval-expr exp lenv))
          (error-attempt-index tbl-name (expval-name tbl)))))
      (else (error-not-implemented)))))

;; eval-prefixexpr : PREFIXEXPR * LocalEnv
(define eval-prefixexpr
  (lambda (pre lenv)
    (cases PREFIXEXPR pre
      (prefixid (id) id))))

;; eval-param : PARAM * LocalEnv
(define eval-param
  (lambda (par lenv)
    (cases PARAM par
      (idparam (id) id)
      (elipparam () (error-not-implemented)))))

;; eval-ifstat : EXPR * BLOCK * List(EXPR) * List(BLOCK) * List(BLOCK) * LocalEnv
(define eval-ifstat
  (lambda (exp blk elif-exprs elif-blocks else-blk lenv)
    (let ((ifval (eval-expr exp lenv)))
      (cond
        ((expval->bool ifval) (eval-block blk lenv))
        ((pair? elif-exprs)
          (letrec ((loop (lambda (elif-exprs1 elif-blocks1)
            (if (null? elif-exprs1)
              (if (pair? else-blk)
                (if (= (length else-blk) 1)
                  (eval-block (car else-blk) lenv)
                  (error-syntax-else))
                (void))
              (if (expval->bool (eval-expr (car elif-exprs1) lenv))
                (eval-block (car elif-blocks1) lenv)
                (loop (cdr elif-exprs1) (cdr elif-blocks1)))))))
          (loop elif-exprs elif-blocks)))
        ((pair? else-blk) (eval-block (car else-blk) lenv))))))

;; eval-forstat : Id * List(Id) * STATFORSUF
;; TODO: syntax error checking
(define eval-forstat
  (lambda (firstid moreids suf lenv)
    (cases STATFORSUF suf
      (statforsuf2 (first rest blk) (error-not-implemented)) ; for iterators
      (statforsuf1 (first rest blk)
        (let
          ((initval (expval->num (eval-expr first lenv)))
          (termval (expval->num (eval-expr (car rest) lenv)))
          (incval (expval->num (if (= 2 (length rest)) (eval-expr (cadr rest) lenv) (num-val 1)))))
        (let
          ((compare-op (if (positive? incval) > <)))
        (letrec ((loop (lambda (counter)
          (if (compare-op counter termval) (void)
            (begin
              (eval-block blk (extend-local-env firstid (num-val counter) lenv))
              (loop (+ counter incval)))))))
        (loop initval))))))))

;; eval-expr : EXPR * LocalEnv
(define eval-expr
  (lambda (exp lenv)
    (cases EXPR exp
      (expr (simple list-binop list-simple)
        (eval-expr-binops simple list-binop list-simple lenv))
      (else (error-not-implemented)))))

;; eval-expr-binops : SIMPLEEXPR * List(BINOP) * List(SIMPLEEXPR) * LocalEnv
(define eval-expr-binops
  (lambda (simple list-binop list-simple lenv)
    (begin
      (define first (eval-simpleexpr simple lenv))
      (letrec ((loop (lambda (b s)
        (cond
          ((or (null? b) (null? s)) first)
          (else
            (begin
              (set! first (apply-binop first (car b) (car s) lenv))
              (loop (cdr b) (cdr s))))))))
      (loop list-binop list-simple)))))

;; eval-simpleexpr : SIMPLEEXPR * LocalEnv
(define eval-simpleexpr
  (lambda (expr lenv)
    (cases SIMPLEEXPR expr
      (numexpr (n) (num-val n))
      (stringexpr (s) (string-val s))
      (trueexpr () (bool-val #t))
      (falseexpr () (bool-val #f))
      (nilexpr () (nil-val))
      (primaryexpr (e) (eval-primaryexpr e lenv))
      (tableexpr (t) (eval-tableconstructor t lenv))
      (else (error-not-implemented)))))

;; eval-primaryexpr : PRIMARYEXPR * LocalEnv
(define eval-primaryexpr
  (lambda (pexp lenv)
    (cases PRIMARYEXPR pexp
      (pexpr (pre sufs)
        (let ((len-sufs (length sufs)))
          (cond
          ((> len-sufs 1) (error-syntax-callsufs))
          ((= 1 len-sufs) (eval-primaryexpr-suf pre (car sufs) lenv)) ;; call
          ((zero? len-sufs)
            (apply-env lenv genv (eval-prefixexpr pre lenv)))))))))

;; eval-dostat : BLOCK * LocalEnv
(define eval-dostat
  (lambda (blk lenv)
    (eval-block blk lenv)))

;; eval-whilestat : EXPR * BLOCK * LocalEnv
(define eval-whilestat
  (lambda (exp blk lenv)
    (letrec ((loop (lambda ()
      (if (expval->bool (eval-expr exp lenv))
        (begin
          (eval-block blk lenv)
          (loop))
        (void)))))
      (loop))))

;; eval-repeatstat : BLOCK * EXPR * LocalEnv
(define eval-repeatstat
  (lambda (blk exp lenv)
    (letrec ((loop (lambda ()
      (begin
        (eval-block blk lenv)
        (if (expval->bool (eval-expr exp lenv)) (void) (loop))))))
    (loop))))

;; apply-procedure : Id * Proc * List
(define apply-procedure
  (lambda (name proc1 args)
    (if (proc-val? proc1)
      (let ((proc2 (expval->proc proc1)))
        (cases proc proc2
          (procedure (params body lenv)
            (eval-block body (extend-local-env* params args lenv)))
          (lib-procedure (params function)
            (if (null? params) (function) (function args)))))
      (error-syntax-call name (expval-name proc1)))))

;; apply-binop : ExpVal * BINOP * SIMPLEEXPR * LocalEnv
(define apply-binop
  (lambda (loperand operator roperand lenv)
    (cases BINOP operator
      (opplus () (apply-arith-binop loperand + roperand lenv))
      (opdiff () (apply-arith-binop loperand - roperand lenv))
      (opmult () (apply-arith-binop loperand * roperand lenv))
      (opdiv () (apply-arith-binop loperand / roperand lenv))
      (oplt () (apply-compare-binop loperand < roperand lenv))
      (ople () (apply-compare-binop loperand <= roperand lenv))
      (opgt () (apply-compare-binop loperand > roperand lenv))
      (opge () (apply-compare-binop loperand >= roperand lenv))
      (opeqeq () (apply-compare-binop loperand eq? roperand lenv))
      (else (error-not-implemented)))))

;; apply-arith-binop : ExpVal * SchemeProc * SIMPLEEXPR * LocalEnv -> NumVal
;; TODO: auto type conversions, multiple return values
(define apply-arith-binop
  (lambda (lval operator rexpr lenv)
    (letrec
      ((rval (eval-simpleexpr rexpr lenv))
      (loperand (if (pair? lval) (car lval) lval)) ; for multiple values use the first for now
      (roperand (if (pair? rval) (car rval) rval)))
      (cond
        ((not (num-val? loperand)) (error-arithmetic (expval-name loperand)))
        ((not (num-val? roperand)) (error-arithmetic (expval-name roperand)))
        (else
          (num-val (apply operator
            (list (expval->num loperand) (expval->num roperand)))))))))

;; apply-compare-binop : ExpVal * SchemeProc * SIMPLEEXPR * LocalEnv -> BoolVal
;; TODO: other type comparisons, multiple return values
(define apply-compare-binop
  (lambda (lval operator rexpr lenv)
    (letrec
      ((rval (eval-simpleexpr rexpr lenv))
      (loperand (if (pair? lval) (car lval) lval))
      (roperand (if (pair? rval) (car rval) rval))
      (ltype (expval-name lval))
      (rtype (expval-name rval)))
      (cond
        ((not (eq? ltype rtype)) (error-compare ltype rtype))
        ((not (and (eq? 'number ltype) (eq? 'number rtype))) ;; only numbers for now
          (error-not-implemented))
        (else
          (bool-val (apply operator
            (list (expval->num loperand) (expval->num roperand)))))))))

;; eval-tableconstructor : TABLECONSTRUCTOR * LocalEnv -> Table
(define eval-tableconstructor
  (lambda (tc lenv)
    (cases TABLECONSTRUCTOR tc
      (tableconstructor (field-list)
        (table-val (initialize-table
          (make-table) (fieldlist->list field-list) lenv))))))

;; initialize-table : Table * List(FIELD) * LocalEnv -> Table
;; add the initial fields to a table
(define initialize-table
  (lambda (tbl fields lenv)
    (letrec ((loop (lambda (flds count)
      (if (null? flds) tbl
        (cases FIELD (car flds)
          (field1 (lhs rhs)
            (let ((lhs-val (eval-expr lhs lenv)))
              (begin
                (cond
                  ((and (num-val? lhs-val) (< (expval->num lhs-val) count)) (void))
                  ((nil-val? lhs-val) (error-nil-index))
                  (else (table-set! tbl lhs-val (eval-expr rhs lenv))))
                (loop (cdr flds) count))))
          (field2 (exp rhs)
            (let ((length-rhs (length rhs)))
              (cond
                ((= length-rhs 0)
                  (begin
                    (table-set! tbl (num-val count) (eval-expr exp lenv))
                    (loop (cdr flds) (+ count 1))))
                ((= length-rhs 1)
                  (begin
                    (let ((id (expr->id exp)))
                      (if id
                        (table-set! tbl id (eval-expr (car rhs) lenv))
                        (error-syntax-tableconstructor)))
                    (loop (cdr flds) count)))
          (else (error-syntax-tableconstructor))))))))))
    (loop fields 1))))