#lang racket/base

(require scribble/manual
         (for-syntax racket/base
                     syntax/parse
                     (only-in racket/file file->string)))

(provide (except-out (all-defined-out) main))

(define-syntax (racketfile stx)
  (syntax-parse stx
    [(_ file-name:str)
     #:with file-str (file->string (syntax-e #'file-name))
     #:with ctx      (syntax/loc stx #'file-name)
     (syntax/loc stx (typeset-code #:context ctx 'file-str))]))

(define-syntax (define-tech stx)
  (syntax-parse stx
    [(_ tech/name module-path)
     (syntax/loc stx
       (define (tech/name
                #:key          [key        #f]
                #:normalize?   [normalize? #t]
                #:tag-prefixes [prefixes   #f]
                #:indirect?    [indirect?  #f]
                .
                alt)
         (apply tech
                #:key          key
                #:normalize?   normalize?
                #:doc          module-path
                #:tag-prefixes prefixes
                #:indirect?    indirect?
                alt)))]))

(define-tech tech/guide '(lib "scribblings/guide/guide.scrbl"))
(define-tech tech/refer '(lib "scribblings/reference/reference.scrbl"))
(define-tech tech/math  '(lib "math/scribblings/math.scrbl"))

(define ^ (compose superscript math))
(define _ (compose subscript   math))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
