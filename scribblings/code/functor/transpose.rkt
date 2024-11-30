#lang typed/racket/base/no-check

(require "../category/Matr.rkt"
         "../category/dual.rkt"
         math/matrix)

(define T matrix-transpose)

(module+ test
  (require "check.rkt")
  (define (rand m n) (random 1 9))
  (define ∘ compose)

  ;; Objects
  (define a (identity-matrix 1))
  (define b (identity-matrix 2))
  (define c (identity-matrix 3))
  (define d (identity-matrix 4))

  ;; Morphisms
  (define f (build-matrix 1 2 rand))
  (define g (build-matrix 2 3 rand))
  (define h (build-matrix 3 4 rand))

  (define check-†𝐌𝐚𝐭𝐫→𝐌𝐚𝐭𝐫 (check-ftr (∘ † 𝐌𝐚𝐭𝐫) 𝐌𝐚𝐭𝐫))
  (define check-T (check-†𝐌𝐚𝐭𝐫→𝐌𝐚𝐭𝐫 T))
  (check-T a b c f g))
