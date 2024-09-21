#lang racket/base

(provide 𝐋𝐢𝐬𝐭)
(define (𝐋𝐢𝐬𝐭 . _) (values dom cod ∘ ? =))

(define (dom _) '())
(define (cod _) '())
(define (∘ . m*) (apply append m*))
(define (? m) (list? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt")

  ;; Morphisms
  (define f '(1 2 3))
  (define g '(a b c))
  (define h '(A B C))

  (define check-𝐋𝐢𝐬𝐭 (check-ooc 𝐋𝐢𝐬𝐭))
  (check-𝐋𝐢𝐬𝐭 f g h))
