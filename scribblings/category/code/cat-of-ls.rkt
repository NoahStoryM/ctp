#lang racket/base

;; Category of Lists
(define (dom _) *)
(define (cod _) *)
(define (∘ . m*) (apply append m*))

(define (morphism? m) (list? m))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

;; Objects
(define * (∘))

;; Morphisms
(define f '(1 2 3))
(define g '(a b c))
(define h '(A B C))

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
