#lang racket/base

(provide (except-out (all-defined-out) main))

(define call (λ (f m) (f m)))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
