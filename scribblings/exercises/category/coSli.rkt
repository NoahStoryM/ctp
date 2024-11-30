#lang racket/base

(require "../../code/category/dual.rkt"
         "Sli.rkt")

(provide Sli†)
(define ((Sli† c) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  ;; 𝒞†
  (define-values (dom𝒞† cod𝒞† ∘𝒞† ?𝒞† =𝒞†)
    († dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞))

  ;; reverse commutative triangle
  (define (~ t)
    (for/fold ([t† '()]) ([s* (in-list t)])
      (cons (reverse s*) t†)))

  ;; 𝒞†/c
  (define-values (dom𝒞†/c cod𝒞†/c ∘𝒞†/c ?𝒞†/c =𝒞†/c)
    ((Sli c) dom𝒞† cod𝒞† ∘𝒞† ?𝒞 =𝒞))

  ;; (𝒞†/c)†
  (define-values (†dom𝒞†/c †cod𝒞†/c †∘𝒞†/c †?𝒞†/c †=𝒞†/c)
    († dom𝒞†/c cod𝒞†/c ∘𝒞†/c ?𝒞†/c =𝒞†/c))

  ;; c/𝒞 = (𝒞†/c)†
  (define (dom t) (~ (†dom𝒞†/c (~ t))))
  (define (cod t) (~ (†cod𝒞†/c (~ t))))
  (define (∘ . t*) (~ (apply †∘𝒞†/c (map ~ t*))))
  (define (? t) (†?𝒞†/c (~ t)))
  (define (= . t*) (apply †=𝒞†/c (map ~ t*)))

  (values dom cod ∘ ? =))

#;(define ((Sli† c) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
    (define dom
      (match-λ
        [`((,f ,p) (,q))
         (define a (dom𝒞 f))
         `((,a ,p) (,p))]))
    (define cod
      (match-λ
        [`((,f ,p) (,q))
         (define b (cod𝒞 f))
         `((,b ,q) (,q))]))
    (define ∘
      (case-λ
        [(t) t]
        [(t1 t2)
         (match* (t1 t2)
           [(`((,g ,q) (,r))
             `((,f ,p) (,q)))
            #:when (=𝒞 (dom𝒞 g) (cod𝒞 f))
            `((,(∘𝒞 g f) ,p) (,r))])]
        [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))
    (define ?
      (match-λ
        [`((,f ,p) (,q))
         #:when
         (and (?𝒞 f) (?𝒞 p) (=𝒞 (dom𝒞 f) (cod𝒞 p))
              (?𝒞 q)
              (=𝒞 (dom𝒞 p) (dom𝒞 q) c))
         (=𝒞 (∘𝒞 f p) q)]
        [_ #f]))
    (define =
      (case-λ
        [(_) #t]
        [(t1 t2)
         (match* (t1 t2)
           [(`((,h ,r) (,s))
             `((,f ,p) (,q)))
            (and (=𝒞 h f) (=𝒞 r p)
                 (=𝒞 s q))]
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

  ;; m/ℳ
  (define m (identity-matrix 5))
  (define m/ℳ (∘ (Sli† m) 𝐌𝐚𝐭𝐫))

  (define p0 (build-matrix 1 5 rand))
  (define q0 (∘ℳ f0 p0))
  (define r0 (∘ℳ g0 f0 p0))
  (define s0 (∘ℳ h0 g0 f0 p0))

  ;; Objects in m/ℳ
  (define a `((,a0 ,p0) (,p0))) ; p0
  (define b `((,b0 ,q0) (,q0))) ; q0
  (define c `((,c0 ,r0) (,r0))) ; r0
  (define d `((,d0 ,s0) (,s0))) ; s0

  ;; Morphisms in m/ℳ
  (define f `((,f0 ,p0) (,q0))) ; f0
  (define g `((,g0 ,q0) (,r0))) ; g0
  (define h `((,h0 ,r0) (,s0))) ; h0

  (define check-m/ℳ (check-cat m/ℳ))
  (check-m/ℳ a b c d f g h))
