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
  (require rackunit)
  (require (file "../../code/category/𝐏𝐚𝐢𝐫.rkt"))

  (define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))
  (define-values (dom cod ∘ ? =)
    (Arr dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫))

  ;; Objects in 𝒫
  (define a~ '(a . a)) (check-pred ?𝒫 a~)
  (define b~ '(b . b)) (check-pred ?𝒫 b~)
  (define c~ '(c . c)) (check-pred ?𝒫 c~)
  (define d~ '(d . d)) (check-pred ?𝒫 d~)
  (define e~ '(e . e)) (check-pred ?𝒫 e~)
  (define f~ '(f . f)) (check-pred ?𝒫 f~)
  (define g~ '(g . g)) (check-pred ?𝒫 g~)
  (define h~ '(h . h)) (check-pred ?𝒫 h~)

  ;; Morphisms in 𝒫
  (define p~ '(a . b)) (check-pred ?𝒫 p~)
  (define q~ '(c . d)) (check-pred ?𝒫 q~)
  (define r~ '(e . f)) (check-pred ?𝒫 r~)
  (define s~ '(g . h)) (check-pred ?𝒫 s~)

  (define i~ '(a . c)) (check-pred ?𝒫 i~)
  (define j~ '(b . d)) (check-pred ?𝒫 j~)
  (define k~ '(c . e)) (check-pred ?𝒫 k~)
  (define l~ '(d . f)) (check-pred ?𝒫 l~)
  (define m~ '(e . g)) (check-pred ?𝒫 m~)
  (define n~ '(f . h)) (check-pred ?𝒫 n~)


  ;; Objects in Arr(𝒫)
  (define a `((,b~ ,p~) (,p~ ,a~))) (check-pred ? a) ; p~
  (define b `((,d~ ,q~) (,q~ ,c~))) (check-pred ? b) ; q~
  (define c `((,f~ ,r~) (,r~ ,e~))) (check-pred ? c) ; r~
  (define d `((,h~ ,s~) (,s~ ,g~))) (check-pred ? d) ; s~

  ;; Morphisms in Arr(𝒫)
  (define f `((,j~ ,p~) (,q~ ,i~))) (check-pred ? f) ; (i~, j~)
  (define g `((,l~ ,q~) (,r~ ,k~))) (check-pred ? g) ; (k~, l~)
  (define h `((,n~ ,r~) (,s~ ,m~))) (check-pred ? h) ; (m~, n~)


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
