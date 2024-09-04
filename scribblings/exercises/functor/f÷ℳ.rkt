#lang typed/racket/base/no-check

(require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt")
         (file "../../code/functor/SliF†.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define -/ℳ (SliF† ∘ℳ))

(module+ test
  (require rackunit)
  (require math/matrix)
  (require (file "../../exercises/category/Sli†.rkt"))

  (define (rand m n) (random 1 9))

  ;; Objects in ℳ
  (define a0 (identity-matrix 1)) (check-pred ?ℳ a0)
  (define b0 (identity-matrix 2)) (check-pred ?ℳ b0)
  (define c0 (identity-matrix 3)) (check-pred ?ℳ c0)
  (define d0 (identity-matrix 4)) (check-pred ?ℳ d0)

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand)) (check-pred ?ℳ f0)
  (define g0 (build-matrix 3 2 rand)) (check-pred ?ℳ g0)
  (define h0 (build-matrix 4 3 rand)) (check-pred ?ℳ h0)


  ;; b/ℳ
  (define-values (domb/ℳ codb/ℳ ∘b/ℳ ?b/ℳ =b/ℳ)
    ((Sli† b0) domℳ codℳ ∘ℳ ?ℳ =ℳ))

  (define p0 (build-matrix 1 2 rand)) (check-pred ?ℳ p0)
  (define q0 (∘ℳ f0 p0))              (check-pred ?ℳ q0)
  (define r0 (∘ℳ g0 f0 p0))           (check-pred ?ℳ r0)
  (define s0 (∘ℳ h0 g0 f0 p0))        (check-pred ?ℳ s0)

  ;; Objects in b/ℳ
  (define a `((,a0 ,p0) (,p0))) (check-pred ?b/ℳ a)
  (define b `((,b0 ,q0) (,q0))) (check-pred ?b/ℳ b)
  (define c `((,c0 ,r0) (,r0))) (check-pred ?b/ℳ c)
  (define d `((,d0 ,s0) (,s0))) (check-pred ?b/ℳ d)

  ;; Morphisms in b/ℳ
  (define f `((,f0 ,p0) (,q0))) (check-pred ?b/ℳ f)
  (define g `((,g0 ,q0) (,r0))) (check-pred ?b/ℳ g)
  (define h `((,h0 ,r0) (,s0))) (check-pred ?b/ℳ h)


  ;; a/ℳ
  (define-values (doma/ℳ coda/ℳ ∘a/ℳ ?a/ℳ =a/ℳ)
    ((Sli† a0) domℳ codℳ ∘ℳ ?ℳ =ℳ))

  ;; Functors
  (define f/ℳ (-/ℳ f0))

  ;; Preservation of domain and codomain
  (check-true (=a/ℳ (f/ℳ b) (doma/ℳ (f/ℳ g)) (f/ℳ (doma/ℳ g))))
  (check-true (=a/ℳ (f/ℳ c) (coda/ℳ (f/ℳ g)) (f/ℳ (codb/ℳ g))))

  ;; Preservation of identity morphisms
  (check-true (=b/ℳ      c  (domb/ℳ      c)  (codb/ℳ      c)))
  (check-true (=a/ℳ (f/ℳ c) (doma/ℳ (f/ℳ c)) (coda/ℳ (f/ℳ c))))

  ;; Preservation of composable pairs
  (check-true (=a/ℳ (∘a/ℳ (f/ℳ g) (f/ℳ f)) (f/ℳ (∘b/ℳ g f)))))
