#lang racket/base

(require math/matrix)

;; Category of Matrices
(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))

(define (morphism? m) (matrix? m))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

;; Objects
(define a (identity-matrix 1))
(define b (identity-matrix 2))
(define c (identity-matrix 3))
(define d (identity-matrix 4))

;; Morphisms
(define (rand m n) (random 1 9))
(define f (build-matrix 2 1 rand))
(define g (build-matrix 3 2 rand))
(define h (build-matrix 4 3 rand))

;; Existence of composition
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

;; Associativity of composition
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity
(morphism=? a (dom a) (cod a))

;; Identity and composition
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
