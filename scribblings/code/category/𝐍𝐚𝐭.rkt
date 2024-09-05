#lang racket/base

(provide 𝐍𝐚𝐭)
(define (𝐍𝐚𝐭 . _) (values dom cod ∘ ? =))

(define (dom _) 0)
(define (cod _) 0)
(define (∘ . m*) (apply + m*))
(define (? m) (exact-nonnegative-integer? m))

(module+ test
  (require "check.rkt")

  ;; Morphisms
  (define f 1)
  (define g 2)
  (define h 3)

  (define check-𝐍𝐚𝐭 (check-ooc 𝐍𝐚𝐭))
  (check-𝐍𝐚𝐭 f g h))
