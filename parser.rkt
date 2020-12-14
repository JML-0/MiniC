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
     [(instr Lsemicol)      (list $1)]
     [(instr Lsemicol prog) (cons $1 $3)])
    (instr
     [(Lident Lassign expr)      (Passign $1 $3 $1-start-pos)]
     [(Lident Lopar args Lcpar) (Pcall $1 $3 $1-start-pos)])
    (expr
     [(Lnum)                    (Pnum $1 $1-start-pos)]
     [(Lstr)                    (Pstr $1 $1-start-pos)]
     [(Lident)                  (Pvar $1 $1-start-pos)]
     [(expr Lplus expr)         (Pcall '%add (list $1 $3) $2-start-pos)]
     [(expr Lsub expr)          (Pcall '%sub (list $1 $3) $2-start-pos)]
     [(expr Lmul expr)          (Pcall '%mul (list $1 $3) $2-start-pos)]
     [(expr Ldiv expr)          (Pcall '%div (list $1 $3) $2-start-pos)]
     [(expr Lmod expr)          (Pcall '%mod (list $1 $3) $2-start-pos)]
     [(expr Lequal expr)        (Pcall '%seq (list $1 $3) $2-start-pos)]
     [(expr Lnequal expr)       (Pcall '%!= (list $1 $3) $2-start-pos)]
     [(expr Lpp expr)           (Pcall '%<  (list $1 $3) $2-start-pos)]
     [(expr Lpg expr)           (Pcall '%>  (list $1 $3) $2-start-pos)]
     [(expr Lppe expr)          (Pcall '%<= (list $1 $3) $2-start-pos)]
     [(expr Lpge expr)          (Pcall '%>= (list $1 $3) $2-start-pos)]
     [(Lident Lopar args Lcpar) (Pcall $1 $3 $1-start-pos)])
    (args
     [()                 (list)]
     [(expr)             (list $1)]
     ((expr Lcomma args) (cons $1 $3))))
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
