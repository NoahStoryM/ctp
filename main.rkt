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


;; Dual Category
(define (¬ dom𝒞 cod𝒞 ∘𝒞)
  (define (dom m) (cod𝒞 m))
  (define (cod m) (dom𝒞 m))
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values dom cod ∘))


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


;; Slice Category 𝒞/c
(define ((Sli dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?) c)
  (define (dom t)
    (match t
      [`((,p) (,q ,f))
       (define a (dom𝒞 f))
       `((,p) (,p ,a))]))
  (define (cod t)
    (match t
      [`((,p) (,q ,f))
       (define b (cod𝒞 f))
       `((,q) (,q ,b))]))
  (define ∘
    (case-lambda
      [(t) t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,q) (,r ,g))
           `((,p) (,q ,f)))
          `((,p) (,r ,(∘𝒞 g f)))])]
      [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))

  (define (morphism? t)
    (match t
      [`((,p) (,q ,f))
       (and (morphism𝒞? p)
            (morphism𝒞? q)
            (morphism𝒞? f)
            (morphism𝒞=? c (cod𝒞 p) (cod𝒞 q))
            (morphism𝒞=? p (∘𝒞 q f)))]
      [_ #f]))
  (define morphism=?
    (case-lambda
      [(_) #t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,r) (,s ,h))
           `((,p) (,q ,f)))
          (and (morphism𝒞=? r p)
               (morphism𝒞=? s q)
               (morphism𝒞=? h f))]
         [(_ _) #f])]
      [(t1 t2 . t*) (and (morphism=? t1 t2) (apply morphism=? t*))]))

  (values dom cod ∘ morphism? morphism=?))

;; Coslice Category c/𝒞
(define ((¬Sli dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?) c)
  (define (dom t)
    (match t
      [`((,f ,p) (,q))
       (define a (dom𝒞 f))
       `((,a ,p) (,p))]))
  (define (cod t)
    (match t
      [`((,f ,p) (,q))
       (define b (cod𝒞 f))
       `((,b ,q) (,q))]))
  (define ∘
    (case-lambda
      [(t) t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,g ,q) (,r))
           `((,f ,p) (,q)))
          `((,(∘𝒞 g f) ,p) (,r))])]
      [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))

  (define (morphism? t)
    (match t
      [`((,f ,p) (,q))
       (and (morphism𝒞? f)
            (morphism𝒞? p)
            (morphism𝒞? q)
            (morphism𝒞=? (dom𝒞 p) (dom𝒞 q) c)
            (morphism𝒞=? (∘𝒞 f p) q))]
      [_ #f]))
  (define morphism=?
    (case-lambda
      [(_) #t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,h ,r) (,s))
           `((,f ,p) (,q)))
          (and (morphism𝒞=? h f)
               (morphism𝒞=? r p)
               (morphism𝒞=? s q))]
         [(_ _) #f])]
      [(t1 t2 . t*) (and (morphism=? t1 t2) (apply morphism=? t*))]))

  (values dom cod ∘ morphism? morphism=?))
