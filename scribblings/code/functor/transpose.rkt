#lang typed/racket/base/no-check

(require "../category/Matr.rkt"
         "../category/dual.rkt"
         math/matrix)

(: T (∀ ([b : ℳ] [a : ℳ]) (→ (→ℳ a b) (→ℳ (T b) (T a)))))
(define T matrix-transpose)

(module+ test
  (require "check.rkt")
  (define (rand m n) (random 1 9))
  (define ∘ compose)

  ;; Objects
  (: a ℳ) (define a (identity-matrix 1))
  (: b ℳ) (define b (identity-matrix 2))
  (: c ℳ) (define c (identity-matrix 3))
  (: d ℳ) (define d (identity-matrix 4))

  ;; Morphisms
  (: f (→ℳ a b)) (define f (build-matrix 2 1 rand))
  (: g (→ℳ b c)) (define g (build-matrix 3 2 rand))
  (: h (→ℳ c d)) (define h (build-matrix 4 3 rand))

  (define check-†𝐌𝐚𝐭𝐫→𝐌𝐚𝐭𝐫 (check-ftr (∘ † 𝐌𝐚𝐭𝐫) 𝐌𝐚𝐭𝐫))
  (define check-T (check-†𝐌𝐚𝐭𝐫→𝐌𝐚𝐭𝐫 T))
  (check-T c b a g f))
