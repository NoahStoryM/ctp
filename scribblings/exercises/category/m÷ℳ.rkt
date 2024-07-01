#lang racket/base

(require ctp math/matrix)

;; Category of Matrices ℳ
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))
(define (?ℳ m) (matrix? m))
(define =ℳ
  (case-λ
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (=ℳ m1 m2) (apply =ℳ m2 m*))]))

;; Objects in ℳ
(define a0 (identity-matrix 1)) (?ℳ a0)
(define b0 (identity-matrix 2)) (?ℳ b0)
(define c0 (identity-matrix 3)) (?ℳ c0)
(define d0 (identity-matrix 4)) (?ℳ d0)

(define m  (identity-matrix 5)) (?ℳ m)

;; Morphisms in ℳ
(define (rand m n) (random 1 9))

(define f0 (build-matrix 2 1 rand)) (?ℳ f0)
(define g0 (build-matrix 3 2 rand)) (?ℳ g0)
(define h0 (build-matrix 4 3 rand)) (?ℳ h0)

(define p0 (build-matrix 1 5 rand)) (?ℳ p0)

(define q0 (∘ℳ f0 p0))       (?ℳ q0)
(define r0 (∘ℳ g0 f0 p0))    (?ℳ r0)
(define s0 (∘ℳ h0 g0 f0 p0)) (?ℳ s0)


;; Coslice Category m/ℳ
(define-values (dom cod ∘ ? =)
  ((¬Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

;; Objects in m/ℳ
(define a `((,a0 ,p0) (,p0))) (? a) ; p0
(define b `((,b0 ,q0) (,q0))) (? b) ; q0
(define c `((,c0 ,r0) (,r0))) (? c) ; r0
(define d `((,d0 ,s0) (,s0))) (? d) ; s0

;; Morphisms in m/ℳ
(define f `((,f0 ,p0) (,q0))) (? f) ; f0
(define g `((,g0 ,q0) (,r0))) (? g) ; g0
(define h `((,h0 ,r0) (,s0))) (? h) ; h0

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
