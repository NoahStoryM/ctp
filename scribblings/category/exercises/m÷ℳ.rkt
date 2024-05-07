#lang racket/base

(require ctp racket/match math/matrix)

;; Category of Matrices ℳ
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))
(define (?ℳ m) (matrix? m))
(define =ℳ
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (=ℳ m1 m2) (apply =ℳ m*))]))

;; Objects in ℳ
(define a0 (identity-matrix 1))
(define b0 (identity-matrix 2))
(define c0 (identity-matrix 3))
(define d0 (identity-matrix 4))

(define m  (identity-matrix 5))

;; Morphisms in ℳ
(define (rand m n) (random 1 9))

(define p0 (build-matrix 1 5 rand))
(define q0 (build-matrix 2 5 rand))
(define r0 (build-matrix 3 5 rand))
(define s0 (build-matrix 4 5 rand))

(define f0 (build-matrix 2 1 rand))
(define g0 (build-matrix 3 2 rand))
(define h0 (build-matrix 4 3 rand))


;; Coslice Category m/ℳ
(define-values (dom cod ∘ ? =)
  ((¬Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

;; Objects in m/ℳ
(define a `((,a0 ,p0) (,p0))) ; p0
(define b `((,b0 ,q0) (,q0))) ; q0
(define c `((,c0 ,r0) (,r0))) ; r0
(define d `((,d0 ,s0) (,s0))) ; s0

;; Morphisms in m/ℳ
(define f `((,f0 ,p0) (,q0))) ; f0
(define g `((,g0 ,q0) (,r0))) ; g0
(define h `((,h0 ,r0) (,s0))) ; h0

;; Existence of composition
(= b (cod f) (dom g))
(= a (dom (∘ g f)) (dom f))
(= c (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity
(= a (dom a) (cod a))

;; Identity and composition
(= f (∘ f (dom f)) (∘ (cod f) f))
