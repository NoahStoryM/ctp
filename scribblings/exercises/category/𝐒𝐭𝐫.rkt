#lang racket/base

;; Category of Strings
(define (dom _) ∗)
(define (cod _) ∗)
(define (∘ . m*) (apply string-append m*))
(define (? m) (string? m))
(define (= m . m*) (apply string=? m m*))

;; Objects
(define ∗ (∘)) (? ∗)

;; Morphisms
(define f "123") (? f)
(define g "abc") (? g)
(define h "ABC") (? h)

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
