#lang racket/base

(require racket/match)

(provide Arr)
(define (Arr dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define dom
    (match-λ
      [`((,j ,p) (,q ,i))
       (define a (dom𝒞 i))
       (define b (dom𝒞 j))
       `((,b ,p) (,p ,a))]))
  (define cod
    (match-λ
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
          #:when
          (and (=𝒞 (dom𝒞 l) (cod𝒞 j))
               (=𝒞 (dom𝒞 k) (cod𝒞 i)))
          `((,(∘𝒞 l j) ,p) (,r ,(∘𝒞 k i)))])]
      [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))
  (define ?
    (match-λ
      [`((,j ,p) (,q ,i))
       #:when
       (and (?𝒞 j) (?𝒞 p) (=𝒞 (dom𝒞 j) (cod𝒞 p))
            (?𝒞 q) (?𝒞 i) (=𝒞 (dom𝒞 q) (cod𝒞 i)))
       (=𝒞 (∘𝒞 j p) (∘𝒞 q i))]
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

(module+ test
  (require "../../code/category/check.rkt"
           "../../code/category/Pair.rkt")
  (define Arr_𝐏𝐚𝐢𝐫 (compose Arr 𝐏𝐚𝐢𝐫))

  ;; Objects in 𝒫
  (define a~ '(a . a))
  (define b~ '(b . b))
  (define c~ '(c . c))
  (define d~ '(d . d))
  (define e~ '(e . e))
  (define f~ '(f . f))
  (define g~ '(g . g))
  (define h~ '(h . h))

  ;; Morphisms in 𝒫
  (define p~ '(a . b))
  (define q~ '(c . d))
  (define r~ '(e . f))
  (define s~ '(g . h))

  (define i~ '(a . c))
  (define j~ '(b . d))
  (define k~ '(c . e))
  (define l~ '(d . f))
  (define m~ '(e . g))
  (define n~ '(f . h))

  (define check-𝐏𝐚𝐢𝐫 (check-cat 𝐏𝐚𝐢𝐫))
  (for ([a (in-list (list a~ b~))]
        [b (in-list (list c~ d~))]
        [c (in-list (list e~ f~))]
        [d (in-list (list g~ h~))]
        [f (in-list (list i~ j~))]
        [g (in-list (list k~ l~))]
        [h (in-list (list m~ n~))])
    (check-𝐏𝐚𝐢𝐫 a b c d f g h))

  ;; Objects in Arr(𝒫)
  (define a `((,b~ ,p~) (,p~ ,a~))) ; p~
  (define b `((,d~ ,q~) (,q~ ,c~))) ; q~
  (define c `((,f~ ,r~) (,r~ ,e~))) ; r~
  (define d `((,h~ ,s~) (,s~ ,g~))) ; s~

  ;; Morphisms in Arr(𝒫)
  (define f `((,j~ ,p~) (,q~ ,i~))) ; (i~, j~)
  (define g `((,l~ ,q~) (,r~ ,k~))) ; (k~, l~)
  (define h `((,n~ ,r~) (,s~ ,m~))) ; (m~, n~)

  (define check-Arr_𝐏𝐚𝐢𝐫 (check-cat Arr_𝐏𝐚𝐢𝐫))
  (check-Arr_𝐏𝐚𝐢𝐫 a b c d f g h))
