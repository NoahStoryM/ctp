#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(define-type Type Symbol)
(struct graph ([value : Char] [source : Type] [target : Type]))

;; (define-type 𝒢)
