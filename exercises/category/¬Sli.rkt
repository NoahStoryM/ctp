#lang racket/base

(require racket/match (file "¬.rkt") "Sli.rkt")

(provide ¬Sli)

;; Coslice Category c/𝒞
(define (¬Sli dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?)
  ;; ¬𝒞
  (define-values (dom¬𝒞 cod¬𝒞 ∘¬𝒞) (¬ dom𝒞 cod𝒞 ∘𝒞))

  ;; reverse commutative triangle
  (define (reverse-triangle t)
    (for/fold ([¬t '()]) ([s* (in-list t)])
      (cons (reverse s*) ¬t)))

  (λ (c)
    ;; ¬𝒞/c
    (define-values (dom¬𝒞/c cod¬𝒞/c ∘¬𝒞/c morphism¬𝒞/c? morphism¬𝒞/c=?)
      ((Sli dom¬𝒞 cod¬𝒞 ∘¬𝒞 morphism𝒞? morphism𝒞=?) c))

    ;; ¬(¬𝒞/c)
    (define-values (¬dom¬𝒞/c ¬cod¬𝒞/c ¬∘¬𝒞/c)
      (¬ dom¬𝒞/c cod¬𝒞/c ∘¬𝒞/c))

    ;; c/𝒞 = ¬(¬𝒞/c)
    (define (dom t) (reverse-triangle (¬dom¬𝒞/c (reverse-triangle t))))
    (define (cod t) (reverse-triangle (¬cod¬𝒞/c (reverse-triangle t))))
    (define (∘ . t*) (reverse-triangle (apply ¬∘¬𝒞/c (map reverse-triangle t*))))

    (define (morphism?  t) (morphism¬𝒞/c? (reverse-triangle t)))
    (define (morphism=? . t*) (apply morphism¬𝒞/c=? (map reverse-triangle t*)))

    (values dom cod ∘ morphism? morphism=?)))

#;(define ((¬Sli dom𝒞 cod𝒞 ∘𝒞 morphism𝒞? morphism𝒞=?) c)
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
