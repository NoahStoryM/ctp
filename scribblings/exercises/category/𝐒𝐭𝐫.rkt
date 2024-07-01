#lang racket/base

(provide 𝐒𝐭𝐫)
(define (𝐒𝐭𝐫 . _) (values dom cod ∘ ? =))

(define (dom _) "")
(define (cod _) "")
(define (∘ . m*) (apply string-append m*))
(define (? m) (string? m))
(define (= m . m*) (apply string=? m m*))

(module+ test
  (require rackunit)

  ;; Objects
  (define ∗ (∘)) (check-pred ? ∗)

  ;; Morphisms
  (define f "123") (check-pred ? f)
  (define g "abc") (check-pred ? g)
  (define h "ABC") (check-pred ? h)


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
