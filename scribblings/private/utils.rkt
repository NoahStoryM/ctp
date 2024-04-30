#lang racket/base

(provide (except-out (all-defined-out) main))

(define main (λ ([argv (current-command-line-arguments)]) (values)))
(module+ main (call-with-values main exit))
