#lang racket/base

(provide 𝐍𝐚𝐭)
(define (𝐍𝐚𝐭 . _) (values dom cod ∘ ? =))

(define (dom _) 0)
(define (cod _) 0)
(define (∘ . m*) (apply + m*))
(define (? m) (exact-nonnegative-integer? m))

(module+ test
  (require rackunit)

  ;; Objects
  (define ∗ (∘)) (check-pred ? ∗)

  ;; Morphisms
  (define f 1) (check-pred ? f)
  (define g 2) (check-pred ? g)
  (define h 3) (check-pred ? h)


  ;; Existence of composition
  (check-true (= ∗ (cod f) (dom g)))
  (check-true (= ∗ (dom (∘ g f)) (dom f)))
  (check-true (= ∗ (cod (∘ g f)) (cod g)))

  ;; Associativity of composition
  (check-true (= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f))))

  ;; Existence of identity morphisms
  (check-true (= ∗ (dom ∗) (cod ∗)))

  ;; Composition and identity morphisms
  (check-true (= f (∘ f (dom f)) (∘ (cod f) f))))
