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
     [(expr)                      $1])
    (expr
     [(cond)                      $1]
     [(loop)                      $1]
     [(funcall)                   $1]
     [(sexpr Lpplus Lsemicol)     (Pcall '%pp (list $1) $2-start-pos)]
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
     [(Lwhile Lopar sexpr Lcpar expr) (Pwhile $3 $5 $1-start-pos)]) ;;TODO for
    (constant
     [(Lnum)                      (Pnum $1 $1-start-pos)]
     [(Lstr)                      (Pstr $1 $1-start-pos)]
     [(Lbool)                     (Pbool $1 $1-start-pos)]
     [(Lident)                    (Pvar $1 $1-start-pos)])
    (type
     [(Ltype)                     $1])
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
     [(sexpr Lpge sexpr)          (Pcall '%sge (list $1 $3) $2-start-pos)]
     [(sexpr Land sexpr)          (Pcall '%and (list $1 $3) $2-start-pos)]
     [(sexpr Lor sexpr)           (Pcall '%or (list $1 $3) $2-start-pos)]
     [(sexpr Lxor sexpr)          (Pcall '%xor (list $1 $3) $2-start-pos)]
     [(sexpr Lsll sexpr)          (Pcall '%sll (list $1 $3) $2-start-pos)]
     [(sexpr Lsrl sexpr)          (Pcall '%srl (list $1 $3) $2-start-pos)]
     [(Lnot sexpr)                (Pcall '%not (list $2) $2-start-pos)])
    (args
     [()                          (list)]
     [(sexpr)                     (list $1)]
     ((sexpr Lcomma args)         (cons $1 $3))))
   (precs 
    (left Lor)
    (left Lxor)
    (left Land)
    (right Lnot)

    (left Lsll)
    (left Lsrl)

    (left Lequal)
    (left Lnequal)
    (left Lpp)
    (left Lpg)
    (left Lppe)
    (left Lpge)
    
    (left Lmod Lsub Lplus Lmul Ldiv))
   (error
    (lambda (tok-ok? tok-name tok-value spos epos)
      (err (format "syntax error near ~a~a"
                   tok-name
                   (if tok-value (format " (~a)" tok-value) ""))
           spos)))))

(define (parse src)
  (port-count-lines! src)
  (parse-syntax (lambda () (get-token src))))
