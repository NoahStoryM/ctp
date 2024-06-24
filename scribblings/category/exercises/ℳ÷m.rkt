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
(define a1 (identity-matrix 1)) (?ℳ a1)
(define b1 (identity-matrix 2)) (?ℳ b1)
(define c1 (identity-matrix 3)) (?ℳ c1)
(define d1 (identity-matrix 4)) (?ℳ d1)

(define m  (identity-matrix 5)) (?ℳ m)

;; Morphisms in ℳ
(define (rand m n) (random 1 9))

(define f1 (build-matrix 2 1 rand)) (?ℳ f1)
(define g1 (build-matrix 3 2 rand)) (?ℳ g1)
(define h1 (build-matrix 4 3 rand)) (?ℳ h1)

(define s1 (build-matrix 5 4 rand)) (?ℳ s1)

(define r1 (∘ℳ s1 h1))       (?ℳ r1)
(define q1 (∘ℳ s1 h1 g1))    (?ℳ q1)
(define p1 (∘ℳ s1 h1 g1 f1)) (?ℳ p1)


;; Slice Category ℳ/m
(define-values (dom cod ∘ ? =)
  ((Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

;; Objects in ℳ/m
(define a `((,p1) (,p1 ,a1))) (? a) ; p1
(define b `((,q1) (,q1 ,b1))) (? b) ; q1
(define c `((,r1) (,r1 ,c1))) (? c) ; r1
(define d `((,s1) (,s1 ,d1))) (? d) ; s1

;; Morphisms in ℳ/m
(define f `((,p1) (,q1 ,f1))) (? f) ; f1
(define g `((,q1) (,r1 ,g1))) (? g) ; g1
(define h `((,r1) (,s1 ,h1))) (? h) ; h1

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
