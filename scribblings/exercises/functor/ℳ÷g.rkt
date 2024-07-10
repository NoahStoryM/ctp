#lang typed/racket/base/no-check

(require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt")
         (file "../../code/functor/SliF.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define ℳ/- (SliF ∘ℳ))

(module+ test
  (require rackunit)
  (require math/matrix)
  (require (file "../../exercises/category/Sli.rkt"))

  (define (rand m n) (random 1 9))

  ;; Objects in ℳ
  (define a1 (identity-matrix 1)) (check-pred ?ℳ a1)
  (define b1 (identity-matrix 2)) (check-pred ?ℳ b1)
  (define c1 (identity-matrix 3)) (check-pred ?ℳ c1)
  (define d1 (identity-matrix 4)) (check-pred ?ℳ d1)

  ;; Morphisms in ℳ
  (define f1 (build-matrix 2 1 rand)) (check-pred ?ℳ f1)
  (define g1 (build-matrix 3 2 rand)) (check-pred ?ℳ g1)
  (define h1 (build-matrix 4 3 rand)) (check-pred ?ℳ h1)


  ;; ℳ/b
  (define-values (domℳ/b codℳ/b ∘ℳ/b ?ℳ/b =ℳ/b)
    ((Sli b1) domℳ codℳ ∘ℳ ?ℳ =ℳ))

  (define s1 (build-matrix 2 4 rand)) (check-pred ?ℳ s1)
  (define r1 (∘ℳ s1 h1))              (check-pred ?ℳ r1)
  (define q1 (∘ℳ s1 h1 g1))           (check-pred ?ℳ q1)
  (define p1 (∘ℳ s1 h1 g1 f1))        (check-pred ?ℳ p1)

  ;; Objects in ℳ/b
  (define a `((,p1) (,p1 ,a1))) (check-pred ?ℳ/b a)
  (define b `((,q1) (,q1 ,b1))) (check-pred ?ℳ/b b)
  (define c `((,r1) (,r1 ,c1))) (check-pred ?ℳ/b c)
  (define d `((,s1) (,s1 ,d1))) (check-pred ?ℳ/b d)

  ;; Morphisms in ℳ/b
  (define f `((,p1) (,q1 ,f1))) (check-pred ?ℳ/b f)
  (define g `((,q1) (,r1 ,g1))) (check-pred ?ℳ/b g)
  (define h `((,r1) (,s1 ,h1))) (check-pred ?ℳ/b h)


  ;; ℳ/c
  (define-values (domℳ/c codℳ/c ∘ℳ/c ?ℳ/c =ℳ/c)
    ((Sli c1) domℳ codℳ ∘ℳ ?ℳ =ℳ))

  ;; Functors
  (define ℳ/g (ℳ/- g1))

  ;; Preservation of domain and codomain
  (check-true (=ℳ/c (ℳ/g a) (domℳ/c (ℳ/g f)) (ℳ/g (domℳ/b f))))
  (check-true (=ℳ/c (ℳ/g b) (codℳ/c (ℳ/g f)) (ℳ/g (codℳ/b f))))

  ;; Preservation of identity morphisms
  (check-true (=ℳ/b      a  (domℳ/b      a)  (codℳ/b      a)))
  (check-true (=ℳ/c (ℳ/g a) (domℳ/c (ℳ/g a)) (codℳ/c (ℳ/g a))))

  ;; Preservation of composable pairs
  (check-true (=ℳ/c (∘ℳ/c (ℳ/g g) (ℳ/g f)) (ℳ/g (∘ℳ/b g f)))))
