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
  (require rackunit)
  (require math/matrix)
  (require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt"))

  (define (rand m n) (random 1 9))
  (define m (identity-matrix 5))

  (define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
  (define-values (dom cod ∘ ? =)
    ((Sli m) domℳ codℳ ∘ℳ ?ℳ =ℳ))


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
