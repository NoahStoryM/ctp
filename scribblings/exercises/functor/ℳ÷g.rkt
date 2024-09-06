#lang typed/racket/base/no-check

(require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt")
         (file "../../code/functor/SliF.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define ℳ/- (SliF ∘ℳ))

(module+ test
  (require math/matrix
           (file "../../code/category/check.rkt")
           (file "../../code/functor/check.rkt")
           (file "..//category/Sli.rkt"))
  (define (rand m n) (random 1 9))

  ;; Objects in ℳ
  (define a1 (identity-matrix 1))
  (define b1 (identity-matrix 2))
  (define c1 (identity-matrix 3))
  (define d1 (identity-matrix 4))

  ;; Morphisms in ℳ
  (define f1 (build-matrix 2 1 rand))
  (define g1 (build-matrix 3 2 rand))
  (define h1 (build-matrix 4 3 rand))

  (define check-𝐌𝐚𝐭𝐫 (check-cat 𝐌𝐚𝐭𝐫))
  (check-𝐌𝐚𝐭𝐫 a1 b1 c1 d1 f1 g1 h1)

  (define s1 (build-matrix 2 4 rand))
  (define r1 (∘ℳ s1 h1))
  (define q1 (∘ℳ s1 h1 g1))
  (define p1 (∘ℳ s1 h1 g1 f1))

  ;; Objects in ℳ/b
  (define a `((,p1) (,p1 ,a1)))
  (define b `((,q1) (,q1 ,b1)))
  (define c `((,r1) (,r1 ,c1)))
  (define d `((,s1) (,s1 ,d1)))

  ;; Morphisms in ℳ/b
  (define f `((,p1) (,q1 ,f1)))
  (define g `((,q1) (,r1 ,g1)))
  (define h `((,r1) (,s1 ,h1)))

  (define ℳ/b (compose (Sli b1) 𝐌𝐚𝐭𝐫))
  (define ℳ/c (compose (Sli c1) 𝐌𝐚𝐭𝐫))
  (define ℳ/g (ℳ/- g1))

  (define check-ℳ/b (check-cat ℳ/b))
  (check-ℳ/b a b c d f g h)

  (define check-ℳ/c (check-cat ℳ/c))
  (check-ℳ/c (ℳ/g a) (ℳ/g b) (ℳ/g c) (ℳ/g d) (ℳ/g f) (ℳ/g g) (ℳ/g h))

  (define check-ℳ/b→ℳ/c (check-ftr ℳ/b ℳ/c))
  (define check-ℳ/g (check-ℳ/b→ℳ/c ℳ/g))
  (check-ℳ/g a b c f g))
