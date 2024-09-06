#lang racket/base

(require rackunit)
(provide (all-defined-out))

(define (check-ftr 𝒞 𝒟)
  (define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))
  (define-values (dom𝒟 cod𝒟 ∘𝒟 ?𝒟 =𝒟) (𝒟))
  (λ (F)
    (λ (a b c f g)
      (check-pred ?𝒞 a) (check-pred ?𝒟 (F a))
      (check-pred ?𝒞 b) (check-pred ?𝒟 (F b))
      (check-pred ?𝒞 c) (check-pred ?𝒟 (F c))
      (check-pred ?𝒞 f) (check-pred ?𝒟 (F f))
      (check-pred ?𝒞 g) (check-pred ?𝒟 (F g))

      ;; Preservation of domain and codomain
      (check-true (=𝒟 (F a) (dom𝒟 (F f)) (F (dom𝒞 f))))
      (check-true (=𝒟 (F b) (cod𝒟 (F f)) (F (cod𝒞 f))))

      ;; Preservation of identity morphisms
      (check-true (=𝒞    c  (dom𝒞    c)  (cod𝒞    c)))
      (check-true (=𝒟 (F c) (dom𝒟 (F c)) (cod𝒟 (F c))))

      ;; Preservation of composable pairs
      (check-true (=𝒟 (∘𝒟 (F g) (F f)) (F (∘𝒞 g f)))))))
