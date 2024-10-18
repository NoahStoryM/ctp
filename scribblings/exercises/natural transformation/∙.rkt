#lang racket/base

(provide make-vertical-compose)

(define (make-vertical-compose 𝒞 𝒟)
  (define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))
  (define-values (dom𝒟 cod𝒟 ∘𝒟 ?𝒟 =𝒟) (𝒟))
  (define ∙
    (case-λ
      [() values]
      [(α) α]
      [(α . α*)
       (λ (f)
         (define a (dom𝒞 f))
         (define α->αa (λ (α) (α a)))
         (define α*a (apply ∘𝒟 (map α->αa α*)))
         (∘𝒟 (α f) α*a))]))
  ∙)
