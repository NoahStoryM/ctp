#lang racket/base

(require rackunit)
(provide (all-defined-out))

(define (check-cat 𝒞)
  (define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))
  (λ (a b c d f g h)
    (check-pred ?𝒞 a)
    (check-pred ?𝒞 b)
    (check-pred ?𝒞 c)
    (check-pred ?𝒞 d)
    (check-pred ?𝒞 f)
    (check-pred ?𝒞 g)
    (check-pred ?𝒞 h)

    ;; Existence of composition
    (check-true (=𝒞 b (cod𝒞 f) (dom𝒞 g)))
    (check-true (=𝒞 a (dom𝒞 (∘𝒞 g f)) (dom𝒞 f)))
    (check-true (=𝒞 c (cod𝒞 (∘𝒞 g f)) (cod𝒞 g)))

    ;; Associativity of composition
    (check-true (=𝒞 (∘𝒞 h g f) (∘𝒞 (∘𝒞 h g) f) (∘𝒞 h (∘𝒞 g f))))

    ;; Existence of identity morphisms
    (check-true (=𝒞 a (dom𝒞 a) (cod𝒞 a)))

    ;; Composition and identity morphisms
    (check-true (=𝒞 f (∘𝒞 f (dom𝒞 f)) (∘𝒞 (cod𝒞 f) f)))))

(define (check-ooc 𝒞)
  (define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))
  (define ∗ (∘𝒞))
  (define check-𝒞 (check-cat 𝒞))
  (λ (f g h) (check-𝒞 ∗ ∗ ∗ ∗ f g h)))
