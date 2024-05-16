#lang racket/base

(require racket/match
         "private/utils.rkt")

(provide (except-out (all-defined-out)
                     make-horizontal-composition
                     make-vertical-composition))

(struct composition (procedure* body)
  #:property prop:procedure
  (struct-field-index body))

(struct horizontal-composition composition ()
  #:constructor-name make-horizontal-composition)
(struct vertical-composition composition ()
  #:constructor-name make-vertical-composition)

(define (make-· dom𝒞 cod𝒞 ∘𝒟)
  (define ·
    (case-λ
      [() values]
      [(α) α]
      [(α . α*)
       ;; TODO vertical-composition
       (define composed
         (λ (f)
           (define a (dom𝒞 f))
           (define (α->αdom α) (α a))
           (define αdom* (apply ∘𝒟 (map α->αdom α*)))
           (∘𝒟 (α f) αdom*)))
       composed]))
  ·)

;; Category of Procedures
(define (dom _) values)
(define (cod _) values)
(define ∘
  (case-λ
    [() values]
    [(m) m]
    [m*
     (define procedure**
       (for/list ([m (in-list m*)])
         (cond
           [(eq? values m) '()]
           [(horizontal-composition? m) (composition-procedure* m)]
           #;[(vertical-composition? m) ] ; TODO Interchange Law
           [else (list m)])))
     (define procedure* (apply append procedure**))
     (define body (apply compose procedure*))
     (case (length procedure*)
       [(0 1) body]
       [else (make-horizontal-composition procedure* body)])]))
(define (? m) (procedure? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2)
     (or (eq? m1 m2)
         (and (horizontal-composition? m1)
              (horizontal-composition? m2)
              (equal? (composition-procedure* m1)
                      (composition-procedure* m2)))
         #;(and ;; TODO Interchange Law
            (vertical-composition? m1)
            (vertical-composition? m2)))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Opposite Category
(define (¬ dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values cod𝒞 dom𝒞 ∘ ?𝒞 =𝒞))

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
(define (?× . ?*)
  (define n (length ?*))
  (define (? . m*)
    (and (list? m*) (= n (length m*))
         (andmap call ?* m*)))
  ?)
(define (=× . =*)
  (define (= . m**)
    (for/and ([= (in-list =*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply = m*)))
  =)

;; Arrow Category Arr(𝒞)
(define (Arr dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
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
    (case-λ
      [(s) s]
      [(s1 s2)
       (match* (s1 s2)
         [(`((,l ,q) (,r ,k))
           `((,j ,p) (,q ,i)))
          `((,(∘𝒞 l j) ,p) (,r ,(∘𝒞 k i)))])]
      [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))
  (define (? s)
    (match s
      [`((,j ,p) (,q ,i))
       (and (?𝒞 j) (?𝒞 p)
            (?𝒞 q) (?𝒞 i)
            (=𝒞 (∘𝒞 j p) (∘𝒞 q i)))]
      [_ #f]))
  (define =
    (case-λ
      [(_) #t]
      [(s1 s2)
       (match* (s1 s2)
         [(`((,n ,r) (,s ,m))
           `((,j ,p) (,q ,i)))
          (and (=𝒞 n j) (=𝒞 r p)
               (=𝒞 s q) (=𝒞 m i))]
         [(_ _) #f])]
      [(s1 s2 . s*) (and (= s1 s2) (apply = s*))]))

  (values dom cod ∘ ? =))

;; Slice Category 𝒞/c
(define ((Sli dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) c)
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
    (case-λ
      [(t) t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,q) (,r ,g))
           `((,p) (,q ,f)))
          `((,p) (,r ,(∘𝒞 g f)))])]
      [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))
  (define (? t)
    (match t
      [`((,p) (,q ,f))
       (and (?𝒞 p)
            (?𝒞 q) (?𝒞 f)
            (=𝒞 c (cod𝒞 p) (cod𝒞 q))
            (=𝒞 p (∘𝒞 q f)))]
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

;; Coslice Category c/𝒞
#;(define (¬Sli dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
    ;; ¬𝒞
    (define-values (dom¬𝒞 cod¬𝒞 ∘¬𝒞 ?¬𝒞 =¬𝒞)
      (¬ dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞))

    ;; reverse commutative triangle
    (define (~ t)
      (for/fold ([¬t '()]) ([s* (in-list t)])
        (cons (reverse s*) ¬t)))

    (λ (c)
      ;; ¬𝒞/c
      (define-values (dom¬𝒞/c cod¬𝒞/c ∘¬𝒞/c ?¬𝒞/c =¬𝒞/c)
        ((Sli dom¬𝒞 cod¬𝒞 ∘¬𝒞 ?𝒞 =𝒞) c))

      ;; ¬(¬𝒞/c)
      (define-values (¬dom¬𝒞/c ¬cod¬𝒞/c ¬∘¬𝒞/c ¬?¬𝒞/c ¬=¬𝒞/c)
        (¬ dom¬𝒞/c cod¬𝒞/c ∘¬𝒞/c ?¬𝒞/c =¬𝒞/c))

      ;; c/𝒞 = ¬(¬𝒞/c)
      (define (dom t) (~ (¬dom¬𝒞/c (~ t))))
      (define (cod t) (~ (¬cod¬𝒞/c (~ t))))
      (define (∘ . t*) (~ (apply ¬∘¬𝒞/c (map ~ t*))))
      (define (? t) (¬?¬𝒞/c (~ t)))
      (define (= . t*) (apply ¬=¬𝒞/c (map ~ t*)))

      (values dom cod ∘ ? =)))

(define ((¬Sli dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) c)
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
    (case-λ
      [(t) t]
      [(t1 t2)
       (match* (t1 t2)
         [(`((,g ,q) (,r))
           `((,f ,p) (,q)))
          `((,(∘𝒞 g f) ,p) (,r))])]
      [(t1 t2 . t*) (apply ∘ (∘ t1 t2) t*)]))
  (define (? t)
    (match t
      [`((,f ,p) (,q))
       (and (?𝒞 f) (?𝒞 p)
            (?𝒞 q)
            (=𝒞 (dom𝒞 p) (dom𝒞 q) c)
            (=𝒞 (∘𝒞 f p) q))]
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
