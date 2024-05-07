#lang racket/base

(require racket/match)

(provide Arr)

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
    (case-lambda
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
    (case-lambda
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
