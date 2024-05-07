#lang racket/base

(require racket/match (file "¬.rkt") "Sli.rkt")

(provide ¬Sli)

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
    (case-lambda
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
    (case-lambda
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
