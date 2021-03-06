#lang racket
(require (except-in eopl #%module-begin))

(provide the-grammar)

(define the-grammar
  '(
    (BLOCK (CHUNK) block)
    (CHUNK ((arbno STAT STATSUF) (arbno LASTSTAT STATSUF)) chunk)
    (STATSUF ((arbno ";")) statsuf)
    (LASTSTAT ("return" EXPR (arbno "," EXPR)) returnstat)
    (LASTSTAT ("break") breakstat)

    (STAT ("do" BLOCK "end") dostat)
    (STAT ("while" EXPR "do" BLOCK "end") whilestat)
    (STAT ("repeat" BLOCK "until" EXPR) repeatstat)
    (STAT ("if" EXPR "then" BLOCK (arbno "elseif" EXPR "then" BLOCK) (arbno "else" BLOCK) "end") ifstat)
    (STAT ("function" identifier (arbno "." identifier) (arbno ":" identifier) BODY) functionstat)
    (BODY ("(" (arbno PARLIST) ")" BLOCK "end") funcbody)
    (FUNCARGS ("(" (arbno EXPRLIST) ")") funcargs)
    ;(FUNCARGS (string) funcargs2)
    (EXPRLIST (EXPR (arbno "," EXPR)) exprlist)
    (PARLIST (PARAM (arbno "," PARAM)) parlist)
    (PARAM (identifier) idparam)
    (PARAM ("...") elipparam)

    (STAT ("local" STATLOCALSUF) localstat)
    (STATLOCALSUF ("function" identifier (arbno "." identifier) (arbno ":" identifier) BODY) localfunction)
    (STATLOCALSUF (identifier (arbno "," identifier) "=" EXPR (arbno "," EXPR)) localassign)

    (STAT ("for" identifier (arbno "," identifier) STATFORSUF) forstat)
    (STATFORSUF ("=" EXPR (arbno "," EXPR) "do" BLOCK "end") statforsuf1)
    (STATFORSUF ("in" EXPR (arbno "," EXPR) "do" BLOCK "end") statforsuf2)
    (STAT (PRIMARYEXPR (arbno "," PRIMARYEXPR) (arbno "=" EXPRLIST)) callassignstat)

    (EXPR (SIMPLEEXPR (arbno BINOP SIMPLEEXPR)) expr)
    (EXPR (UNOP SIMPLEEXPR (arbno BINOP SIMPLEEXPR)) unexpr)
    (SIMPLEEXPR (num) numexpr)
    (SIMPLEEXPR (string) stringexpr)
    (SIMPLEEXPR ("nil") nilexpr)
    (SIMPLEEXPR ("true") trueexpr)
    (SIMPLEEXPR ("false") falseexpr)
    (SIMPLEEXPR ("elip") elipexpr)
    (SIMPLEEXPR ("function" BODY) functionexpr)
    (SIMPLEEXPR (TABLECONSTRUCTOR) tableexpr)
    (SIMPLEEXPR (PRIMARYEXPR) primaryexpr)
    (PRIMARYEXPR (PREFIXEXPR (arbno PRIMARYEXPRSUF)) pexpr)
    (PREFIXEXPR (identifier) prefixid)
    ;(PREFIXEXPR ("(" EXPR ")") prefixparens)
    (PRIMARYEXPRSUF ("." identifier) pexprdot)
    (PRIMARYEXPRSUF ("[" EXPR "]") pexprbracket)
    (PRIMARYEXPRSUF (":" identifier FUNCARGS) pexprcolon)
    (PRIMARYEXPRSUF (FUNCARGS) pexprargs)

    (TABLECONSTRUCTOR ("{" FIELDLIST "}") tableconstructor)
    (FIELD ("[" EXPR "]" "=" EXPR) field1)
    (FIELD (EXPR (arbno "=" EXPR)) field2)
    (FIELDLIST ((separated-list FIELD FIELDSEP)) fieldlist)
    (FIELDSEP (",") fieldsep)
    (FIELDSEP (";") fieldsep2)

    (UNOP ("not") notunop)
    (UNOP ("#") hashunop)

    (BINOP ("+") opplus)
    (BINOP ("-") opdiff)
    (BINOP ("*") opmult)
    (BINOP ("/") opdiv)
    (BINOP ("^") opcaret)
    (BINOP ("%") oppercent)
    (BINOP ("..") opdotdot)
    (BINOP ("<") oplt)
    (BINOP ("<=") ople)
    (BINOP (">") opgt)
    (BINOP (">=") opge)
    (BINOP ("==") opeqeq)
    (BINOP ("~=") opmatch)
    (BINOP ("and") opand)
    (BINOP ("or") opor)
  )
)