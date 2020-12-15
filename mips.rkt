#lang racket/base

(require racket/match
         "ast.rkt")

(provide mips-print)

(define (print-datum datum)
  (match datum
    [(Asciiz l s)
     (printf "~a: .asciiz ~s\n" l s)]))

(define (print-data data)
  (for-each print-datum data))

(define (fmt-loc loc)
  (match loc
    [(Mem r o) (format "~a($~a)" o r)]
    [(Lbl l)   (format "~a" l)]))

(define (print-instr instr)
  (match instr
    [(Label l)     (printf "~a:\n" l)]
    [(Move d r)    (printf "\tmove $~a, $~a\n" d r)]
    [(Li d i)      (printf "\tli $~a, ~a\n" d i)]
    [(La d a)      (printf "\tla $~a, ~a\n" d (fmt-loc a))]
    [(Addi d r i)  (printf "\taddi $~a, $~a, ~a\n" d r i)]
    [(Add d r1 r2) (printf "\tadd $~a, $~a, $~a\n" d r1 r2)]
    [(Sub d r1 r2) (printf "\tsub $~a, $~a, $~a\n" d r1 r2)]
    [(Mul d r1 r2) (printf "\tmul $~a, $~a, $~a\n" d r1 r2)]
    [(Div d r1 r2) (printf "\tdiv $~a, $~a, $~a\n" d r1 r2)]
    [(Seq d r1 r2) (printf "\tseq $~a, $~a, $~a\n" d r1 r2)]
    [(Sne d r1 r2) (printf "\tsne $~a, $~a, $~a\n" d r1 r2)]
    [(Slt d r1 r2) (printf "\tslt $~a, $~a, $~a\n" d r1 r2)]
    [(Sgt d r1 r2) (printf "\tsgt $~a, $~a, $~a\n" d r1 r2)]
    [(Sle d r1 r2) (printf "\tsle $~a, $~a, $~a\n" d r1 r2)]
    [(Sge d r1 r2) (printf "\tsge $~a, $~a, $~a\n" d r1 r2)]
    [(Mfhi d)      (printf "\tmfhi $~a\n" d)]
    [(Sw r l)      (printf "\tsw $~a, ~a\n" r (fmt-loc l))]
    [(Lw r l)      (printf "\tlw $~a, ~a\n" r (fmt-loc l))]
    [(Jr r)        (printf "\tjr $~a\n" r)]
    [(Jal l)       (printf "\tjal ~a\n" (fmt-loc l))]
    [(Syscall)     (printf "\tsyscall\n")]))

(define (print-text text)
  (for-each print-instr text))

(define (mips-print mips)
  (printf ".data\n")
  (print-data (Mips-data mips))
  (printf "\n.text\n")
  (print-text (Mips-text mips)))
