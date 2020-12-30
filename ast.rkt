#lang racket/base

(provide (all-defined-out))

;; parsed syntax
(struct Pnum (val pos)          #:transparent)
(struct Pstr (val pos)          #:transparent)
(struct Pbool (val pos)         #:transparent)
(struct Pvar (name pos)         #:transparent)
(struct Pcall (func args pos)   #:transparent)
(struct Passign (var expr pos)  #:transparent)
(struct Pcond (val t f pos)     #:transparent)
(struct Ploop (val args pos)    #:transparent)
(struct Pblock (args pos)       #:transparent)

;; abstract syntax
(struct Num (val)               #:transparent)
(struct Str (val)               #:transparent)
(struct Bool (val)              #:transparent)
(struct Var (name)              #:transparent)
(struct Call (func args)        #:transparent)
(struct Assign (var expr)       #:transparent)
(struct Cond (test t f)         #:transparent)
(struct Loop (test args)        #:transparent)
(struct Block (n)               #:transparent)

;; inter lang
(struct Data (lbl)              #:transparent)

;; MIPS assembly
(struct Mips (data text)        #:transparent)

;; MIPS data
(struct Asciiz (name str)       #:transparent)

;; MIPS instruction
(struct Label   (lbl)           #:transparent)
(struct Move    (dst reg)       #:transparent)
(struct Li      (dst imm)       #:transparent)
(struct La      (dst loc)       #:transparent)
(struct Add     (dst rg1 rg2)   #:transparent)
(struct Addi    (dst reg imm)   #:transparent)
(struct Sub     (dst rg1 rg2)   #:transparent)
(struct Mul     (dst rg1 rg2)   #:transparent)
(struct Div     (dst rg1 rg2)   #:transparent)
(struct Mod     (dst rg1 rg2)   #:transparent)
(struct Seq     (dst rg1 rg2)   #:transparent)
(struct Sne     (dst rg1 rg2)   #:transparent)
(struct Slt     (dst rg1 rg2)   #:transparent)
(struct Sgt     (dst rg1 rg2)   #:transparent)
(struct Sle     (dst rg1 rg2)   #:transparent)
(struct Sge     (dst rg1 rg2)   #:transparent)
(struct Sw      (reg loc)       #:transparent)
(struct Lw      (reg loc)       #:transparent)
(struct Jr      (reg)           #:transparent)
(struct Jal     (loc)           #:transparent)
(struct Mfhi    (loc)           #:transparent)
(struct Beqz    (r l)           #:transparent)
(struct Bnez    (r l)           #:transparent)
(struct B       (l)             #:transparent)
(struct Syscall ()              #:transparent)

(struct Fun_t (ret args))
(struct Ptr_t (t)               #:transparent)
(struct Struct_t (name fields))

;; MIPS memory location
(struct Mem (reg offset)        #:transparent)
(struct Lbl (name)              #:transparent)

;; MIPS syscall
(define PRINT_INT    1)
(define PRINT_STRING 4)
(define SBRK         9)
