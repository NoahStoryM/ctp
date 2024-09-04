#lang racket/base

(require (file "../../code/category/†.rkt")
         (file "Sli.rkt"))

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
  (require rackunit)
  (require math/matrix)
  (require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt"))

  (define (rand m n) (random 1 9))

  ;; ℳ
  (define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))

  ;; Objects in ℳ
  (define a0 (identity-matrix 1)) (check-pred ?ℳ a0)
  (define b0 (identity-matrix 2)) (check-pred ?ℳ b0)
  (define c0 (identity-matrix 3)) (check-pred ?ℳ c0)
  (define d0 (identity-matrix 4)) (check-pred ?ℳ d0)

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand)) (check-pred ?ℳ f0)
  (define g0 (build-matrix 3 2 rand)) (check-pred ?ℳ g0)
  (define h0 (build-matrix 4 3 rand)) (check-pred ?ℳ h0)


  ;; m/ℳ
  (define m (identity-matrix 5))
  (define-values (dom cod ∘ ? =)
    ((Sli† m) domℳ codℳ ∘ℳ ?ℳ =ℳ))

  (define p0 (build-matrix 1 5 rand)) (check-pred ?ℳ p0)
  (define q0 (∘ℳ f0 p0))              (check-pred ?ℳ q0)
  (define r0 (∘ℳ g0 f0 p0))           (check-pred ?ℳ r0)
  (define s0 (∘ℳ h0 g0 f0 p0))        (check-pred ?ℳ s0)

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
