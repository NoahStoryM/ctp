#lang racket/base

(require scribble/manual
         (for-syntax racket/base
                     syntax/parse
                     syntax/location
                     (only-in racket/file file->string)))

(provide (except-out (all-defined-out) main))

(define-syntax (racketfile stx)
  (syntax-parse stx
    [(_ file-name:str)
     #:with ctx (syntax/loc stx #'file-name)
     #:with file-str
     (file->string
      (build-path
       (syntax-source-directory stx)
       'up
       (syntax-e #'file-name)))
     (syntax/loc stx
       (filebox file-name (typeset-code #:indent 0 #:context ctx 'file-str)))]))

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

(define ^ (compose1 superscript math))
(define _ (compose1 subscript   math))

(define-syntax deftech@
  (syntax-parser
    [(_ t)
     #:with key #'(symbol->string 't)
     #'(define t (tech key))]
    [(_ t* ...+)
     #'(begin (deftech@ t*) ...)]))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
