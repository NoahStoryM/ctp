#lang typed/racket/base/no-check

(require math/array math/matrix)
(require "../category/Matr.rkt")

(provide ⊗ (all-from-out "../category/Matr.rkt"))

(: I (Matrix Nothing))
(define I #;(identity-matrix 0) (array #[]))

(: ⊗ (∀ ([a : ℳ] [b : ℳ] [x : ℳ] [y : ℳ]) (→ (× (→ℳ a b) (→ℳ x y)) (→ℳ (⊗ a x) (⊗ b y)))))
(define (⊗ . m*)
  (let ([m* (remq* (list I) m*)])
    (if (null? m*) I (block-diagonal-matrix m*))))

(module+ test
  (require "../category/check.rkt"
           rackunit)
  (define-values (dom cod ∘ ? =) (𝐌𝐚𝐭𝐫))
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
  (check-𝐌𝐚𝐭𝐫 a b c d f g h)

  ;; Strict Monoidal Category
  (check-true (= f (⊗ f (⊗)) (⊗ (⊗) f)))
  (check-true (= (⊗ h g f) (⊗ h (⊗ g f)) (⊗ (⊗ h g) f)))

  ;; Interchange Law
  (define f0 f)
  (define f1 g)
  (define g0 g)
  (define g1 h)
  (check-true
   (= (∘ (⊗ g0 g1) (⊗ f0 f1))
      (⊗ (∘ g0 f0) (∘ g1 f1)))))
