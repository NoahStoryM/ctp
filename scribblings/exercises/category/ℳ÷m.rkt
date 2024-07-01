#lang racket/base

(require ctp math/matrix)
(require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))

(define (rand m n) (random 1 9))

(define m (identity-matrix 5))
(define-values (dom cod ∘ ? =) ((Sli domℳ codℳ ∘ℳ ?ℳ =ℳ) m))

(module+ test
  (require rackunit)

  ;; Objects in ℳ
  (define a1 (identity-matrix 1)) (check-pred ?ℳ a1)
  (define b1 (identity-matrix 2)) (check-pred ?ℳ b1)
  (define c1 (identity-matrix 3)) (check-pred ?ℳ c1)
  (define d1 (identity-matrix 4)) (check-pred ?ℳ d1)

  ;; Morphisms in ℳ
  (define f1 (build-matrix 2 1 rand)) (check-pred ?ℳ f1)
  (define g1 (build-matrix 3 2 rand)) (check-pred ?ℳ g1)
  (define h1 (build-matrix 4 3 rand)) (check-pred ?ℳ h1)

  (define s1 (build-matrix 5 4 rand)) (check-pred ?ℳ s1)

  (define r1 (∘ℳ s1 h1))       (check-pred ?ℳ r1)
  (define q1 (∘ℳ s1 h1 g1))    (check-pred ?ℳ q1)
  (define p1 (∘ℳ s1 h1 g1 f1)) (check-pred ?ℳ p1)


  ;; Objects in ℳ/m
  (define a `((,p1) (,p1 ,a1))) (check-pred ? a) ; p1
  (define b `((,q1) (,q1 ,b1))) (check-pred ? b) ; q1
  (define c `((,r1) (,r1 ,c1))) (check-pred ? c) ; r1
  (define d `((,s1) (,s1 ,d1))) (check-pred ? d) ; s1

  ;; Morphisms in ℳ/m
  (define f `((,p1) (,q1 ,f1))) (check-pred ? f) ; f1
  (define g `((,q1) (,r1 ,g1))) (check-pred ? g) ; g1
  (define h `((,r1) (,s1 ,h1))) (check-pred ? h) ; h1


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
