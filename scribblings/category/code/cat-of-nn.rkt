#lang racket/base

;; Category of Natural Numbers
(define (dom _) *)
(define (cod _) *)
(define (∘ . m*) (apply + m*))
(define (? m) (exact-nonnegative-integer? m))

;; Objects
(define * (∘))

;; Morphisms
(define f 1)
(define g 2)
(define h 3)

;; Existence of composition
(= * (cod f) (dom g))
(= * (dom (∘ g f)) (dom f))
(= * (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= * (dom *) (cod *))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
