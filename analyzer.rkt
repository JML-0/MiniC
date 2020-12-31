#lang racket/base

(require racket/match
         "ast.rkt"
         "baselib.rkt"
         "helper.rkt")

(provide analyze)

(define (type-compat? given expected)
  (match (cons given expected)
    [(cons t t) #t]
    [(cons '% t) #t]
    [(cons t '%) #t]
    [(cons (Ptr_t t1) (Ptr_t t2)) (type-compat? t1 t2)]
    [else #f]))

(define (expr-pos e)
  (match e
    [(Pnum _ p) p]
    [(Pstr _ p) p]
    [(Pbool _ p) p]
    [(Pvar _ p) p]
    [(Pcall _ _ p) p]
    [(Pcond _ _ _ p) p]
    [(Pwhile _ _ p) p]))

(define (analyze-function f as pos env proc?)
  (unless (hash-has-key? env f)
    (err (format "unknown function '~a'" f) pos))
  (let ([ft (hash-ref env f)])
    (unless (= (length (Fun_t-args ft)) (length as))
      (err (format "arity mismatch (expected ~a, given ~a)"
                   (length (Fun_t-args ft))
                   (length as))
           pos))
    (let ([aas (map (lambda (at a)
                      (let ([aa (analyze-expr a env)])
                        (if (type-compat? (cdr aa) at)
                            (car aa)
                            (errt at (cdr aa) (expr-pos a)))))
                    (Fun_t-args ft)
                    as)])
      (cons (Call f aas)
            (Fun_t-ret ft)))))

(define (analyze-expr expr env)
  (match expr
    [(Pnum v pos)
     (cons (Num v)
           'num)]
    [(Pstr v pos)
     (cons (Str v)
           'str)]
    [(Pbool v pos)
     (cons (Bool v)
           'bool)]
    [(Pvar n pos)
     (unless (hash-has-key? env n)
       (err (format "unbound variable: '~a'" n) pos))
     (cons (Var n)
           (hash-ref env n))]
    [(Pcall f as pos)
     (analyze-function f as pos env #f)]
    ))

(define (analyze-instrs exprs env) ;; { Instrs }
  (let* ((rev (reverse exprs))
         (last-expr (car rev))
         (exprs (reverse (cdr rev)))
         (ce (foldl
              (lambda (expr acc)
                (let ((ce (analyze-instr expr (cdr acc))))
                  (cons (append (car acc) (list (car ce)))
                        (cdr ce))))
              (cons '() env)
              exprs))
         (l (analyze-instr last-expr (cdr ce))))
      (cons (append (car ce) (list (car l)))
            (cdr l))))

(define (analyze-instr instr env)
  (match instr
    [(Passign v e pos)
     (let ([ae (analyze-expr e env)])
       (cons (Assign v (car ae))
             (hash-set env v (cdr ae))))]
    [(Pcall f as pos)
     (cons (car (analyze-function f as pos env #t))
           env)]
    [(Pcond test true false pos)
     (let ((t (analyze-instr test env))   ;;test
           (y (analyze-instr true env))   ;;true
           (n (analyze-instr false env))) ;;false
     (cons (Cond (car t) (car y) (car n))
             env))]
    [(Pwhile test args pos)
     (let ((t  (analyze-instr test env))
           (a  (analyze-instr args env)))
       (cons (While (car t) (car a))
             env))]
    [(Pblock exprs pos)
     (cons (Block (car (analyze-instrs exprs env)))
           env)]))

(define (analyze-prog prog env)
  (match prog
    [(list i)
     (list (car (analyze-instr i env)))]
    [(cons i p)
     (let ([ai (analyze-instr i env)])
       (cons (car ai)
             (analyze-prog p (cdr ai))))]))

(define (analyze ast)
  (analyze-prog ast *baselib-types*))