#lang racket/base

(require ctp math/matrix)
(require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))

(define (rand m n) (random 1 9))

(define m (identity-matrix 5))
(define-values (dom cod ∘ ? =) ((¬Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

(module+ test
  (require rackunit)

  ;; Objects in ℳ
  (define a0 (identity-matrix 1)) (check-pred ?ℳ a0)
  (define b0 (identity-matrix 2)) (check-pred ?ℳ b0)
  (define c0 (identity-matrix 3)) (check-pred ?ℳ c0)
  (define d0 (identity-matrix 4)) (check-pred ?ℳ d0)

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand)) (check-pred ?ℳ f0)
  (define g0 (build-matrix 3 2 rand)) (check-pred ?ℳ g0)
  (define h0 (build-matrix 4 3 rand)) (check-pred ?ℳ h0)

  (define p0 (build-matrix 1 5 rand)) (check-pred ?ℳ p0)

  (define q0 (∘ℳ f0 p0))       (check-pred ?ℳ q0)
  (define r0 (∘ℳ g0 f0 p0))    (check-pred ?ℳ r0)
  (define s0 (∘ℳ h0 g0 f0 p0)) (check-pred ?ℳ s0)


  ;; Objects in m/ℳ
  (define a `((,a0 ,p0) (,p0))) (check-pred ? a) ; p0
  (define b `((,b0 ,q0) (,q0))) (check-pred ? b) ; q0
  (define c `((,c0 ,r0) (,r0))) (check-pred ? c) ; r0
  (define d `((,d0 ,s0) (,s0))) (check-pred ? d) ; s0

  ;; Morphisms in m/ℳ
  (define f `((,f0 ,p0) (,q0))) (check-pred ? f) ; f0
  (define g `((,g0 ,q0) (,r0))) (check-pred ? g) ; g0
  (define h `((,h0 ,r0) (,s0))) (check-pred ? h) ; h0


  ;; Existence of composition
  (check-true (= b (cod f) (dom g)))
  (check-true (= a (dom (∘ g f)) (dom f)))
  (check-true (= c (cod (∘ g f)) (cod g)))

  ;; Associativity of composition
  (check-true (= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f))))

  ;; Existence of identity morphisms
  (check-true (= a (dom a) (cod a)))

  ;; Composition and identity morphisms
  (check-true (= f (∘ f (dom f)) (∘ (cod f) f))))
