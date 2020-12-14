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
  (Lsemicol
  Lopar Lcpar
  Lcomma))

(define-empty-tokens keywords
  (Leof))

(define-tokens constants
  (Lident Lnum Lstr))

(define-empty-tokens operators
  (Lplus Lsub Lmul Ldiv Lmod
   Lequal Lnequal Lpp Lpg Lppe Lpge
   Lassign))

(define-lex-abbrev identifier
  (:: alphabetic (:* (:or alphabetic numeric #\_))))

(define get-token
  (lexer-src-pos
   [(eof)        (token-Leof)]
   [whitespace   (return-without-pos (get-token input-port))]
   ["//"         (return-without-pos (comment-lex input-port))] ;;#
   ["/*"         (return-without-pos (long-comment-lex input-port))]
   ["("          (token-Lopar)]
   [")"          (token-Lcpar)]
   [","          (token-Lcomma)]
   [";"          (token-Lsemicol)]
   ["="          (token-Lassign)]
   ["=="         (token-Lequal)]
   ["!="         (token-Lnequal)]
   ["<"          (token-Lpp)]
   [">"          (token-Lpg)]
   ["<="         (token-Lppe)]
   [">="         (token-Lpge)]
   ["+"          (token-Lplus)]
   ["-"          (token-Lsub)]
   ["*"          (token-Lmul)]
   ["/"          (token-Ldiv)]
   ["%"          (token-Lmod)]
   [identifier   (token-Lident (string->symbol lexeme))]
   ["\""         (token-Lstr (string-lex input-port))]
   [(:+ numeric) (token-Lnum (string->number lexeme))]
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
