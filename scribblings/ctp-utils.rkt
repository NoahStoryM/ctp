#lang racket/base

(provide rkt-scrbl math-scrbl)

(define rkt-scrbl  '(lib "scribblings/reference/reference.scrbl"))
(define math-scrbl '(lib "math/scribblings/math.scrbl"))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
