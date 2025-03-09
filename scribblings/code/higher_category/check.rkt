#lang racket/base

(require rackunit)
(provide (all-defined-out))

(define (check-2-cat 𝐂)
  (define-values (domᵛ𝐂 codᵛ𝐂 ∙𝐂 domʰ𝐂 codʰ𝐂 ∘𝐂 ?𝐂 =𝐂) (𝐂))
  (λ (𝒞 𝒟 ℰ F G H K L M α0 β0 α1 β1)
    (check-pred ?𝐂 𝒞) (check-pred ?𝐂 𝒟) (check-pred ?𝐂 ℰ)
    (check-pred ?𝐂 F) (check-pred ?𝐂 G) (check-pred ?𝐂 H)
    (check-pred ?𝐂 K) (check-pred ?𝐂 L) (check-pred ?𝐂 M)
    (check-pred ?𝐂 α0) (check-pred ?𝐂 β0)
    (check-pred ?𝐂 α1) (check-pred ?𝐂 β1)

    ;; Existence of identity 1-cells
    (check-true (=𝐂 𝒞 (domʰ𝐂 𝒞) (codʰ𝐂 𝒞)))

    ;; Existence of identity 2-cells
    (check-true (=𝐂 F (domᵛ𝐂 F) (codᵛ𝐂 F)))

    ;; Identity 1-cells are also identity 2-cells
    (check-true (=𝐂 𝒞 (domᵛ𝐂 𝒞) (codᵛ𝐂 𝒞)))

    ;; Existence of vertical composition
    (check-true (=𝐂 G (codᵛ𝐂 α0) (domᵛ𝐂 β0)))
    (check-true (=𝐂 F (domᵛ𝐂 (∙𝐂 β0 α0)) (domᵛ𝐂 α0)))
    (check-true (=𝐂 H (codᵛ𝐂 (∙𝐂 β0 α0)) (codᵛ𝐂 β0)))

    ;; Existence of horizontally composition
    (check-true (=𝐂 𝒟 (codʰ𝐂 α0) (domʰ𝐂 α1)))
    (check-true (=𝐂 𝒞 (domʰ𝐂 (∘𝐂 α1 α0)) (domʰ𝐂 α0)))
    (check-true (=𝐂 ℰ (codʰ𝐂 (∘𝐂 α1 α0)) (codʰ𝐂 α1)))

    ;; Domain/codomain alignment for 2-cells
    (check-true (=𝐂 𝒞 (domʰ𝐂 α0) (domʰ𝐂 (domᵛ𝐂 α0)) (domʰ𝐂 (codᵛ𝐂 α0))))
    (check-true (=𝐂 𝒟 (codʰ𝐂 α0) (codʰ𝐂 (domᵛ𝐂 α0)) (codʰ𝐂 (codᵛ𝐂 α0))))

    ;; Source/target preservation under horizontal composition
    (check-true (=𝐂 (∘𝐂 K F) (domᵛ𝐂 (∘𝐂 α1 α0)) (∘𝐂 (domᵛ𝐂 α1) (domᵛ𝐂 α0))))
    (check-true (=𝐂 (∘𝐂 L G) (codᵛ𝐂 (∘𝐂 α1 α0)) (∘𝐂 (codᵛ𝐂 α1) (codᵛ𝐂 α0))))

    ;; Interchange law
    (check-true (=𝐂 (∙𝐂 (∘𝐂 β1 β0) (∘𝐂 α1 α0)) (∘𝐂 (∙𝐂 β1 α1) (∙𝐂 β0 α0))))))

(define (check-oo2c 𝐂)
  (define-values (domᵛ𝐂 codᵛ𝐂 ∙𝐂 domʰ𝐂 codʰ𝐂 ∘𝐂 ?𝐂 =𝐂) (𝐂))
  (define ∗ (∘𝐂))
  (check-pred ?𝐂 ∗)
  (check-true (=𝐂 ∗ (domʰ𝐂 ∗) (codʰ𝐂 ∗)))
  (define check-𝐂 (check-2-cat 𝐂))
  (λ (F G H K L M α0 β0 α1 β1) (check-𝐂 ∗ ∗ ∗ F G H K L M α0 β0 α1 β1)))
