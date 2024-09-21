#lang racket/base

(require math/matrix)

(provide 𝐌𝐚𝐭𝐫)
(define (𝐌𝐚𝐭𝐫 . _) (values dom cod ∘ ? =))

(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))
(define (? m) (matrix? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt")
  (define (rand m n) (random 1 9))

  ;; Objects
  (define a (identity-matrix 1))
  (define b (identity-matrix 2))
  (define c (identity-matrix 3))
  (define d (identity-matrix 4))

  ;; Morphisms
  (define f (build-matrix 2 1 rand))
  (define g (build-matrix 3 2 rand))
  (define h (build-matrix 4 3 rand))

  (define check-𝐌𝐚𝐭𝐫 (check-cat 𝐌𝐚𝐭𝐫))
  (check-𝐌𝐚𝐭𝐫 a b c d f g h))
