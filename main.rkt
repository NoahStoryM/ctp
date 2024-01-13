#lang racket/base

(require racket/match "private/utils.rkt")

(provide (all-defined-out))


;; Procedure Category
(define ∘ (procedure-rename compose '∘))
(define ·
  (let ([α->αid (λ (α) (α values))])
    (case-lambda
      [() values]
      [(α) α]
      [(α . α*)
       (define αid (apply ∘ (map α->αid α*)))
       (define composed (λ (f) (∘ (α f) αid)))
       composed])))


;; Product Category
(define (dom× . dom*) (define (dom m*) (map call dom* m*)) dom)
(define (cod× . cod*) (define (cod m*) (map call cod* m*)) cod)
(define (∘× . ∘*)
  (define (∘ . m**)
    (for/list ([∘ (in-list ∘*)]
               [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply ∘ m*)))
  ∘)

(define (morphism× . m*) m*)
(define (morphism×? . morphism?*)
  (define n (length morphism?*))
  (define (morphism? . m*)
    (and (list? m*) (= n (length m*))
         (andmap call morphism?* m*)))
  morphism?)
(define (morphism×=? . morphism=?*)
  (define (morphism=? . m**)
    (for/and ([morphism=? (in-list morphism=?*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply morphism=? m*)))
  morphism=?)


;; Arrow Category Arr(𝒞)
(define (Arr dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?)
  (define (dom s)
    (match s
      [`((,j ,p) (,q ,i))
       (define a (dom𝒞 i))
       (define b (dom𝒞 j))
       `((,b ,p) (,p ,a))]))
  (define (cod s)
    (match s
      [`((,j ,p) (,q ,i))
       (define c (cod𝒞 i))
       (define d (cod𝒞 j))
       `((,d ,q) (,q ,c))]))
  (define ∘
    (case-lambda
      [(s) s]
      [(s1 s2)
       (match* (s1 s2)
         [(`((,l ,q) (,r ,k))
           `((,j ,p) (,q ,i)))
          `((,(∘𝒞 l j) ,p) (,r ,(∘𝒞 k i)))])]
      [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))

  (define (morphism? s)
    (match s
      [`((,j ,p) (,q ,i))
       (and (morphism𝒞? j)
            (morphism𝒞? p)
            (morphism𝒞? q)
            (morphism𝒞? i)
            (morphism𝒞=? (∘𝒞 j p) (∘𝒞 q i)))]
      [_ #f]))
  (define morphism=?
    (case-lambda
      [(_) #t]
      [(s1 s2)
       (match* (s1 s2)
         [(`((,n ,r) (,s ,m))
           `((,j ,p) (,q ,i)))
          (and (morphism𝒞=? n j)
               (morphism𝒞=? r p)
               (morphism𝒞=? s q)
               (morphism𝒞=? m i))]
         [(_ _) #f])]
      [(s1 s2 . s*) (and (morphism=? s1 s2) (apply morphism=? s*))]))

  (values dom cod ∘ morphism? morphism=?))


;; TODO Slice Category 𝒞/x
#;(define (Sli dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?)
    (define (𝒞/_ x)
      (define dom)
      (define cod)
      (define ∘)
      (define morphism?)
      (define morphism=?)
      (values dom cod ∘ morphism? morphism=?))
    𝒞/_)

;; TODO Coslice Category x/𝒞
#;(define (Cos dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?)
    (define (_/𝒞 x)
      (define dom)
      (define cod)
      (define ∘)
      (define morphism?)
      (define morphism=?)
      (values dom cod ∘ morphism? morphism=?))
    _/𝒞)
