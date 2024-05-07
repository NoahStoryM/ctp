#lang racket/base

(require racket/match math/matrix)

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
(define e0 (identity-matrix 5))
(define f0 (identity-matrix 6))
(define g0 (identity-matrix 7))
(define h0 (identity-matrix 8))

;; Morphisms in ℳ
(define (rand m n) (random 1 9))

(define p0 (build-matrix 2 1 rand))
(define q0 (build-matrix 4 3 rand))
(define r0 (build-matrix 6 5 rand))
(define s0 (build-matrix 8 7 rand))

(define i0 (build-matrix 3 1 rand))
(define j0 (build-matrix 4 2 rand))
(define k0 (build-matrix 5 3 rand))
(define l0 (build-matrix 6 4 rand))
(define m0 (build-matrix 7 5 rand))
(define n0 (build-matrix 8 6 rand))


;; Arrow Category Arr(ℳ)
(define (dom s)
  (match s
    [`((,j ,p) (,q ,i))
     (define a (domℳ i))
     (define b (domℳ j))
     `((,b ,p) (,p ,a))]))
(define (cod s)
  (match s
    [`((,j ,p) (,q ,i))
     (define c (codℳ i))
     (define d (codℳ j))
     `((,d ,q) (,q ,c))]))
(define ∘
  (case-lambda
    [(s) s]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,l ,q) (,r ,k))
         `((,j ,p) (,q ,i)))
        `((,(∘ℳ l j) ,p) (,r ,(∘ℳ k i)))])]
    [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))
(define (? s)
  (match s
    [`((,j ,p) (,q ,i))
     (and (?ℳ j) (?ℳ p)
          (?ℳ q) (?ℳ i)
          (=ℳ (∘ℳ j p) (∘ℳ q i)))]
    [_ #f]))
(define =
  (case-lambda
    [(_) #t]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,n ,r) (,s ,m))
         `((,j ,p) (,q ,i)))
        (and (=ℳ n j) (=ℳ r p)
             (=ℳ s q) (=ℳ m i))]
       [(_ _) #f])]
    [(s1 s2 . s*) (and (= s1 s2) (apply = s*))]))

;; Objects in Arr(ℳ)
(define a `((,b0 ,p0) (,p0 ,a0))) ; p0
(define b `((,d0 ,q0) (,q0 ,c0))) ; q0
(define c `((,f0 ,r0) (,r0 ,e0))) ; r0
(define d `((,h0 ,s0) (,s0 ,g0))) ; s0

;; Morphisms in Arr(ℳ)
(define f `((,j0 ,p0) (,q0 ,i0))) ; (i0, j0)
(define g `((,l0 ,q0) (,r0 ,k0))) ; (k0, l0)
(define h `((,n0 ,r0) (,s0 ,m0))) ; (m0, n0)

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
