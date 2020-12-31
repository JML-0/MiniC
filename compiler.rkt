#lang racket/base

(require racket/match
         racket/list
         "ast.rkt"
         "baselib.rkt"
         "helper.rkt")

(provide compile)

(define (compile-data data)
  (append (list (Asciiz 'nl "\n"))
          (map (lambda (d) (Asciiz (car d) (cdr d))) data)))

(define (compile-expr-and-push expr env)
  (append (compile-expr expr env)
          (list (Addi 'sp 'sp -4)
                (Sw 'v0 (Mem 'sp 0)))))

(define (compile-expr expr env)
  (match expr
    [(Num v)
     (list (Li 'v0 v))]
    [(Str v)
     (list (Li 'v0 v))]
    [(Bool v)
     (list (Li 'v0 v))]
    [(Data l)
     (list (La 'v0 (Lbl l)))]
    [(Var n)
     (list (Lw 'v0 (hash-ref env n)))]
    [(Call f as)
     (append
      (apply append (map (lambda (e) (compile-expr-and-push e env)) as))
      (hash-ref env f)
      (list (Addi 'sp 'sp (* 4 (length as)))))]))

(struct Cinstr (code env fp-sp) #:transparent)

(define counterCond 0)  ;;multi if
(define counterWhile 0) ;;multi while

(define (compile-instr instr env fp-sp)
  (match instr
    [(Assign v e) ;;=
     (Cinstr (compile-expr-and-push e env)
             (hash-set env v (Mem 'fp (- (+ 4 fp-sp))))
             (+ 4 fp-sp))]
    [(Call _ _) ;;call
     (Cinstr (compile-expr instr env)
             env
             fp-sp)]
    [(Block body) ;;{}
     (foldl (lambda (expr acc)
              (let ((ce (compile-instr expr (Cinstr-env acc) fp-sp)))
                (Cinstr (append (Cinstr-code acc)
                              (Cinstr-code ce))
                       (Cinstr-env ce) fp-sp)))
            (Cinstr '() env fp-sp)
            body)]
    [(Cond test yes no) ;;if
     (set! counterCond (add1 counterCond))
     (define ctest (compile-instr test env fp-sp))
     (define cyes (compile-instr yes env (- fp-sp 4)))
     (define cno (compile-instr no env (- fp-sp 8)))
     (Cinstr
      (append
       (Cinstr-code ctest)
       (list (Bnez 't5 (string-append "then_" (number->string counterCond)))  ;;then_1, 2, ...
             (Beqz 't5 (string-append "else_" (number->string counterCond)))) ;;$t5 registre des tests (if, <, >, ...)
       (list (Label (string-append "then_" (number->string counterCond))))
       (Cinstr-code cyes)
       (list (B (string-append "endif_" (number->string counterCond))))
       (list (Label (string-append "else_" (number->string counterCond))))
       (Cinstr-code cno)
       (list (Label (string-append "endif_" (number->string counterCond)))))
      env fp-sp)]
    [(While test args) ;;while
     (set! counterWhile (add1 counterWhile))
     (define ctest (compile-instr test env fp-sp))
     (define cargs (compile-instr args env (- fp-sp 4)))
     (Cinstr
      (append
       (Cinstr-code ctest)
       (list (Label (string-append "while_" (number->string counterWhile)))
             (Beqz 't5 (string-append "endwhile_" (number->string counterWhile))))
       (Cinstr-code cargs)
       (list (B (string-append "while_" (number->string counterWhile))))
       (list (Label (string-append "endwhile_" (number->string counterWhile)))))
      env fp-sp)]))

(define (compile-prog prog env fp-sp)
  (match prog
    [(list)
     (cons '()
           0)]
    [(cons i p)
     (let* ([ci (compile-instr i env fp-sp)]
            [cp (compile-prog p (Cinstr-env ci) (Cinstr-fp-sp ci))])
       (cons (append (Cinstr-code ci) (car cp))
             (+ (if (= fp-sp (Cinstr-fp-sp ci)) 0 4)
                (cdr cp))))]))

(define (compile ast)
  (Mips
   (compile-data (cdr ast))
   (append *fake-user-code*
           (list (Label 'main)
                 (Addi 'sp 'sp -8)
                 (Sw 'ra (Mem 'sp 0))
                 (Sw 'fp (Mem 'sp 4))
                 (Addi 'fp 'sp 8))
           (let ([cp (compile-prog (car ast) *baselib-builtins* 8)])
             (append (car cp)
                     (list (Addi 'sp 'sp (cdr cp)))))
           (list (Lw 'ra (Mem 'sp 0))
                 (Lw 'fp (Mem 'sp 4))
                 (Addi 'sp 'sp 8)
                 (Jr 'ra)))))
