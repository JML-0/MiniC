#lang racket/base

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt"
         "ast.rkt"
         "helper.rkt")

(provide parse)

(define parse-syntax
  (parser
   (src-pos)
   (tokens constants operators punctuations keywords)
   (start prog)
   (end Leof)
   (grammar
    (prog
     [(instr)                     (list $1)]
     [(instr prog)                (cons $1 $2)])
    (instr
     [(type Lident Lassign sexpr Lsemicol) (Passign $2 $4 $1-start-pos)]
     ;[(Lident Lopar args Lcpar)   (Pcall $1 $3 $1-start-pos)]
     [(expr)                      $1])
    (expr
     ;[(instr)                     $1]
     [(cond)                      $1]
     [(loop)                      $1]
     [(funcall)                   $1]
     [(sexpr Lpplus Lsemicol)     (Pcall '%add (list $1 (Pnum 1 $1-start-pos)) $1-start-pos)]
     [(Locbra exprs Lccbra)       (Pblock $2 $1-start-pos)])
    (sexpr
     [(funcall)                   $1]
     [(constant)                  $1]
     [(operator)                  $1]
     [(Lopar sexpr Lcpar)         $2])
    (exprs
     [(expr exprs)                (cons $1 $2)]
     [(expr)                      (list $1)])
    (funcall
     [(Lident Lopar args Lcpar Lsemicol)   (Pcall $1 $3 $1-start-pos)])
    (cond
     [(Lif Lopar sexpr Lcpar expr Lelse expr) (Pcond $3 $5 $7 $1-start-pos)])
    (loop
     [(Lwhile Lopar sexpr Lcpar expr) (Ploop $3 $5 $1-start-pos)])
    (constant
     [(Lnum)                      (Pnum $1 $1-start-pos)]
     [(Lstr)                      (Pstr $1 $1-start-pos)]
     [(Lident)                    (Pvar $1 $1-start-pos)])
    (type
     [(Ltype)      $1]) ;;Llist
    (operator
     [(sexpr Lplus sexpr)         (Pcall '%add (list $1 $3) $2-start-pos)]
     [(sexpr Lsub sexpr)          (Pcall '%sub (list $1 $3) $2-start-pos)]
     [(sexpr Lmul sexpr)          (Pcall '%mul (list $1 $3) $2-start-pos)]
     [(sexpr Ldiv sexpr)          (Pcall '%div (list $1 $3) $2-start-pos)]
     [(sexpr Lmod sexpr)          (Pcall '%mod (list $1 $3) $2-start-pos)]
     [(sexpr Lequal sexpr)        (Pcall '%seq (list $1 $3) $2-start-pos)]
     [(sexpr Lnequal sexpr)       (Pcall '%sne (list $1 $3) $2-start-pos)]
     [(sexpr Lpp sexpr)           (Pcall '%slt (list $1 $3) $2-start-pos)]
     [(sexpr Lpg sexpr)           (Pcall '%sgt (list $1 $3) $2-start-pos)]
     [(sexpr Lppe sexpr)          (Pcall '%sle (list $1 $3) $2-start-pos)]
     [(sexpr Lpge sexpr)          (Pcall '%sge (list $1 $3) $2-start-pos)])
    (args
     [()                          (list)]
     [(sexpr)                     (list $1)]
     ((sexpr Lcomma args)         (cons $1 $3))))
   (precs 
    (left Lequal)
    (left Lnequal)
    (left Lpp)
    (left Lpg)
    (left Lppe)
    (left Lpge)

    (left Lmod)
    (left Lplus)
    (left Lsub)
    (left Lmul)
    (left Ldiv))
   (error
    (lambda (tok-ok? tok-name tok-value spos epos)
      (err (format "syntax error near ~a~a"
                   tok-name
                   (if tok-value (format " (~a)" tok-value) ""))
           spos)))))

(define (parse src)
  (port-count-lines! src)
  (parse-syntax (lambda () (get-token src))))
