#lang racket/base

(require rackunit)
(provide (all-defined-out))

(define (check-ntf 𝒞 𝒟)
  (define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))
  (define-values (dom𝒟 cod𝒟 ∘𝒟 ?𝒟 =𝒟) (𝒟))
  (λ (F G)
    (λ (α)
      (λ (a b f)
        (check-pred ?𝒞 a) (check-pred ?𝒟 (α a))
        (check-pred ?𝒞 b) (check-pred ?𝒟 (α b))
        (check-pred ?𝒞 f) (check-pred ?𝒟 (α f))

        ;; Naturality
        (check-true (=𝒟 (dom𝒟 (α f)) (dom𝒟 (F f)) (F a)))
        (check-true (=𝒟 (cod𝒟 (α f)) (cod𝒟 (G f)) (G b)))
        (check-true (=𝒟 (α f) (∘𝒟 (α b) (F f)) (∘𝒟 (G f) (α a))))))))
