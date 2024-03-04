#lang racket/base

(require racket/match)

(provide Arr)

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
