#lang racket/base

(require "parser.rkt"
         "analyzer.rkt"
         "simplifier.rkt"
         "compiler.rkt"
         "mips.rkt")

(define argv (current-command-line-arguments))

(cond
  [(= (vector-length argv) 1)
   (define src (open-input-file (vector-ref argv 0)))
   (define prs (parse src))
   (printf "Parser ok.\n")
   (close-input-port src)
   (define ast (analyze prs))
   (printf "Analyzer ok.\n")
   ;(displayln ast)
   (define smp (simplify ast))
   (printf "Simplifier ok.\n")
   ;(displayln smp)
   (define asm (compile smp))
   (printf "Compiler ok.\n")
   (with-output-to-file #:exists 'replace
     (string-append (vector-ref argv 0) ".s")
     (lambda ()
       (mips-print asm)))]
  [else
   (eprintf "Usage: racket projet.rkt <file>.\n")
   (exit 1)])
