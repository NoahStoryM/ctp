#lang racket/base

(require math/matrix)

(define (rand m n) (random 1 9))

;; Category of Matrices
(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))
(define (? m) (matrix? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a (identity-matrix 1)) (? a)
(define b (identity-matrix 2)) (? b)
(define c (identity-matrix 3)) (? c)
(define d (identity-matrix 4)) (? d)

;; Morphisms
(define f (build-matrix 2 1 rand)) (? f)
(define g (build-matrix 3 2 rand)) (? g)
(define h (build-matrix 4 3 rand)) (? h)

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
