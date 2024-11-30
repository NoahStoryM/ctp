#lang typed/racket/base/no-check

(require "../../code/category/Matr.rkt"
         "../../code/functor/coSliF.rkt")

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define -/ℳ (SliF† ∘ℳ))

(module+ test
  (require math/matrix
           "../../code/category/check.rkt"
           "../../code/functor/check.rkt"
           "../category/coSli.rkt")
  (define (rand m n) (random 1 9))
  (define ∘ compose)

  ;; Objects in ℳ
  (define a0 (identity-matrix 1))
  (define b0 (identity-matrix 2))
  (define c0 (identity-matrix 3))
  (define d0 (identity-matrix 4))

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand))
  (define g0 (build-matrix 3 2 rand))
  (define h0 (build-matrix 4 3 rand))

  (define check-𝐌𝐚𝐭𝐫 (check-cat 𝐌𝐚𝐭𝐫))
  (check-𝐌𝐚𝐭𝐫 a0 b0 c0 d0 f0 g0 h0)

  (define p0 (build-matrix 1 2 rand))
  (define q0 (∘ℳ f0 p0))
  (define r0 (∘ℳ g0 f0 p0))
  (define s0 (∘ℳ h0 g0 f0 p0))

  ;; Objects in b/ℳ
  (define a `((,a0 ,p0) (,p0)))
  (define b `((,b0 ,q0) (,q0)))
  (define c `((,c0 ,r0) (,r0)))
  (define d `((,d0 ,s0) (,s0)))

  ;; Morphisms in b/ℳ
  (define f `((,f0 ,p0) (,q0)))
  (define g `((,g0 ,q0) (,r0)))
  (define h `((,h0 ,r0) (,s0)))

  (define b/ℳ (∘ (Sli† b0) 𝐌𝐚𝐭𝐫))
  (define a/ℳ (∘ (Sli† a0) 𝐌𝐚𝐭𝐫))
  (define f/ℳ (-/ℳ f0))

  (define check-b/ℳ (check-cat b/ℳ))
  (check-b/ℳ a b c d f g h)

  (define check-a/ℳ (check-cat a/ℳ))
  (check-a/ℳ (f/ℳ a) (f/ℳ b) (f/ℳ c) (f/ℳ d) (f/ℳ f) (f/ℳ g) (f/ℳ h))

  (define check-b/ℳ→a/ℳ (check-ftr b/ℳ a/ℳ))
  (define check-f/ℳ (check-b/ℳ→a/ℳ f/ℳ))
  (check-f/ℳ a b c f g))
