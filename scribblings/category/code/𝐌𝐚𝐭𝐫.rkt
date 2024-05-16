#lang racket/base

(require math/matrix)

(define (rand m n) (random 1 9))

;; Category of Matrices
(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))
(define (? m) (matrix? m))
(define =
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a (identity-matrix 1))
(define b (identity-matrix 2))
(define c (identity-matrix 3))
(define d (identity-matrix 4))

;; Morphisms
(define f (build-matrix 2 1 rand))
(define g (build-matrix 3 2 rand))
(define h (build-matrix 4 3 rand))

;; Existence of composition
(= b (cod f) (dom g))
(= a (dom (∘ g f)) (dom f))
(= c (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= a (dom a) (cod a))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
