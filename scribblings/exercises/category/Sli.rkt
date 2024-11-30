#lang racket/base

(require racket/match)

(provide Sli)
(define ((Sli c) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define dom
    (match-λ
      [`((,p) (,q ,f))
       (define a (dom𝒞 f))
       `((,p) (,p ,a))]))
  (define cod
    (match-λ
      [`((,p) (,q ,f))
       (define b (cod𝒞 f))
       `((,q) (,q ,b))]))
  (define ∘
    (case-λ
      [(t) t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,q) (,r ,g))
           `((,p) (,q ,f)))
          #:when (=𝒞 (dom𝒞 g) (cod𝒞 f))
          `((,p) (,r ,(∘𝒞 g f)))])]
      [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))
  (define ?
    (match-λ
      [`((,p) (,q ,f))
       #:when
       (and (?𝒞 p)
            (?𝒞 q) (?𝒞 f) (=𝒞 (dom𝒞 q) (cod𝒞 f))
            (=𝒞 c (cod𝒞 p) (cod𝒞 q)))
       (=𝒞 p (∘𝒞 q f))]
      [_ #f]))
  (define =
    (case-λ
      [(_) #t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,r) (,s ,h))
           `((,p) (,q ,f)))
          (and (=𝒞 r p)
               (=𝒞 s q) (=𝒞 h f))]
         [(_ _) #f])]
      [(t1 t2 . t*) (and (= t1 t2) (apply = t*))]))

  (values dom cod ∘ ? =))


(module+ test
  (require math/matrix)
  (require "../../code/category/check.rkt"
           "../../code/category/Matr.rkt")
  (define (rand m n) (random 1 9))
  (define ∘ compose)
  (define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))

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

  ;; ℳ/m
  (define m (identity-matrix 5))
  (define ℳ/m (∘ (Sli m) 𝐌𝐚𝐭𝐫))

  (define s1 (build-matrix 5 4 rand))
  (define r1 (∘ℳ s1 h1))
  (define q1 (∘ℳ s1 h1 g1))
  (define p1 (∘ℳ s1 h1 g1 f1))

  ;; Objects in ℳ/m
  (define a `((,p1) (,p1 ,a1))) ; p1
  (define b `((,q1) (,q1 ,b1))) ; q1
  (define c `((,r1) (,r1 ,c1))) ; r1
  (define d `((,s1) (,s1 ,d1))) ; s1

  ;; Morphisms in ℳ/m
  (define f `((,p1) (,q1 ,f1))) ; f1
  (define g `((,q1) (,r1 ,g1))) ; g1
  (define h `((,r1) (,s1 ,h1))) ; h1

  (define check-ℳ/m (check-cat ℳ/m))
  (check-ℳ/m a b c d f g h))
