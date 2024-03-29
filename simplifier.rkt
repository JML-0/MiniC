#lang racket/base

(require racket/match
         "ast.rkt")

(provide simplify)

(define (simplify-constant-str ast)
  (define counter 0)
  (define (scs-expr expr)
    (match expr
      [(Num v)
       (cons expr
             '())]
      [(Str v)
       (let ([lbl (string->symbol (format "str_~a" counter))])
         (set! counter (add1 counter))
         (cons (Data lbl)
               (list (cons lbl v))))]
      [(Bool v)
       (cons expr
             '())]
      [(Var n)
       (cons expr
             '())]
      [(Cond v t f)
       (cons expr
             '())]
      [(While v a)
       (cons expr
             '())]
      [(Call f as)
       (let ([sas (map scs-expr as)])
         (cons (Call f (map car sas))
               (apply append (map cdr sas))))]))
  (define (scs-instr instr)
    (match instr
      [(Assign v e)
       (let ([se (scs-expr e)])
         (cons (Assign v (car se))
               (cdr se)))]
      [(Call _ _)
       (scs-expr instr)]
      [(Cond _ _ _)
       (scs-expr instr)]
      [(While _ _)
       (scs-expr instr)]))
  (define (scs-prog prog)
    (match prog
      [(list)
       (cons '()
             '())]
      [(cons i p)
       (let ([si (scs-instr i)]
             [sp (scs-prog p)])
         (cons (cons (car si) (car sp))
               (append (cdr si) (cdr sp))))]))
  (scs-prog ast))

(define (simplify ast)
  (simplify-constant-str ast))
