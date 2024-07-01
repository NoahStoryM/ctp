#lang racket/base

(provide 𝐋𝐢𝐬𝐭)
(define (𝐋𝐢𝐬𝐭 . _) (values dom cod ∘ ? =))

(define (dom _) '())
(define (cod _) '())
(define (∘ . m*) (apply append m*))
(define (? m) (list? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require rackunit)

  ;; Objects
  (define ∗ (∘)) (check-pred ? ∗)

  ;; Morphisms
  (define f '(1 2 3)) (check-pred ? f)
  (define g '(a b c)) (check-pred ? g)
  (define h '(A B C)) (check-pred ? h)


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
