#lang racket/base

(require racket/match math/matrix "Sli.rkt")

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
(define a1 (identity-matrix 1))
(define b1 (identity-matrix 2))
(define c1 (identity-matrix 3))
(define d1 (identity-matrix 4))

(define m  (identity-matrix 5))

;; Morphisms in ℳ
(define (rand m n) (random 1 9))

(define p1 (build-matrix 5 1 rand))
(define q1 (build-matrix 5 2 rand))
(define r1 (build-matrix 5 3 rand))
(define s1 (build-matrix 5 4 rand))

(define f1 (build-matrix 2 1 rand))
(define g1 (build-matrix 3 2 rand))
(define h1 (build-matrix 4 3 rand))


;; Slice Category ℳ/m
(define-values (dom cod ∘ ? =)
  ((Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

;; Objects in ℳ/m
(define a `((,p1) (,p1 ,a1))) ; p1
(define b `((,q1) (,q1 ,b1))) ; q1
(define c `((,r1) (,r1 ,c1))) ; r1
(define d `((,s1) (,s1 ,d1))) ; s1

;; Morphisms in ℳ/m
(define f `((,p1) (,q1 ,f1))) ; f1
(define g `((,q1) (,r1 ,g1))) ; g1
(define h `((,r1) (,s1 ,h1))) ; h1

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
