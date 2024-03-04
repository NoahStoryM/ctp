#lang racket/base

;; Category of Natural Numbers
(define (dom _) *)
(define (cod _) *)
(define (∘ . m*) (apply + m*))

(define (morphism? m) (exact-nonnegative-integer? m))
(define (morphism=? m . m*) (apply = m m*))

;; Objects
(define * (∘))

;; Morphisms
(define f 1)
(define g 2)
(define h 3)

;; Existence of composition
(morphism=? * (cod f) (dom g))
(morphism=? * (dom (∘ g f)) (dom f))
(morphism=? * (cod (∘ g f)) (cod g))

;; Associativity of composition
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity
(morphism=? * (dom *) (cod *))

;; Identity and composition
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
