#lang racket/base

;; Category of Lists
(define (dom _) ∗)
(define (cod _) ∗)
(define (∘ . m*) (apply append m*))
(define (? m) (list? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define ∗ (∘))

;; Morphisms
(define f '(1 2 3))
(define g '(a b c))
(define h '(A B C))

;; Existence of composition
(= ∗ (cod f) (dom g))
(= ∗ (dom (∘ g f)) (dom f))
(= ∗ (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= ∗ (dom ∗) (cod ∗))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
