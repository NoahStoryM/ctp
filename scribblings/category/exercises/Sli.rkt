#lang racket/base

(require racket/match)

(provide Sli)

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
    (case-lambda
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
    (case-lambda
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
