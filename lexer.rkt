#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "helper.rkt")

(provide get-token
         constants
         operators
         punctuations
         keywords)

(define-empty-tokens punctuations
  (Lsemicol Lcol
  Lopar Lcpar Locbra Lccbra
  Lcomma Llist))

(define-empty-tokens keywords
  (Lrec Lreturn
  Lif Lthen Lelse
  Lwhile
  Lempty
  Leof))

(define-tokens constants
  (Lident Lnum Lstr Lbool Ltype))

(define-empty-tokens operators
  (Lplus Lpplus Lsub Lmul Ldiv Lmod
   Lequal Lnequal Lpp Lpg Lppe Lpge
   Land Lor Lxor Lnot
   Lsll Lsrl
   Lassign))

(define-lex-abbrev identifier
  (:: alphabetic (:* (:or alphabetic numeric #\_))))

(define-lex-abbrev bool
  (:or "true" "false"))

(define-lex-abbrev types
  (:or "int" "str" "bool"))

(define get-token
  (lexer-src-pos
   [(eof)        (token-Leof)]
   [whitespace   (return-without-pos (get-token input-port))]
   ["//"         (return-without-pos (comment-lex input-port))] ;;#
   ["/*"         (return-without-pos (long-comment-lex input-port))]
   ["("          (token-Lopar)]
   [")"          (token-Lcpar)]
   ["{"          (token-Locbra)] ;;curly bra
   ["}"          (token-Lccbra)]
   [","          (token-Lcomma)]
   [";"          (token-Lsemicol)]
   [":"          (token-Lcol)]
   ["="          (token-Lassign)]
   ["=="         (token-Lequal)]
   ["!="         (token-Lnequal)]
   ["<"          (token-Lpp)]
   [">"          (token-Lpg)]
   ["<="         (token-Lppe)]
   [">="         (token-Lpge)]
   ["&&"         (token-Land)]
   ["||"         (token-Lor)]
   ["|"          (token-Lxor)]
   ["!"          (token-Lnot)]
   ["+"          (token-Lplus)]
   ["++"         (token-Lpplus)]
   ["-"          (token-Lsub)]
   ["*"          (token-Lmul)]
   ["/"          (token-Ldiv)]
   ["<<"         (token-Lsll)]
   [">>"         (token-Lsrl)]
   ["%"          (token-Lmod)]
   ["if"         (token-Lif)]
   ["else"       (token-Lelse)]
   ["while"      (token-Lwhile)]
   ["return"     (token-Lreturn)]
   [types        (token-Ltype (string->symbol lexeme))]
   [identifier   (token-Lident (string->symbol lexeme))]
   ["\""         (token-Lstr (string-lex input-port))]
   [(:+ numeric) (token-Lnum (string->number lexeme))]
   [bool         (token-Lbool (string=? "true" lexeme))]
   [any-char (err (format "unrecognized character '~a'" lexeme)
                  start-pos)]))

(define string-lex
  (lexer
   [(eof)  (error 'lexer "eof while reading string")]
   ["\\\"" (string-append "\"" (string-lex input-port))]
   ["\\\\" (string-append "\\" (string-lex input-port))]
   ["\\n"  (string-append "\n" (string-lex input-port))]
   ["\\t"  (string-append "\t" (string-lex input-port))]
   ["\""   ""]
   [any-char (string-append lexeme (string-lex input-port))]))

(define comment-lex
  (lexer
   [(eof)     (get-token input-port)]
   ["\n"      (get-token input-port)]
   [any-char  (comment-lex input-port)]))

(define long-comment-lex
  (lexer
   [(eof)     (get-token input-port)]
   ["*/"      (get-token input-port)] ;; fin du block
   [any-char  (long-comment-lex input-port)]))
