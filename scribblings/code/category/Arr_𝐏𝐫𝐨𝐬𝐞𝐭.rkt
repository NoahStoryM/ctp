#lang racket/base

(require racket/match)
(require (file "𝐏𝐫𝐨𝐬𝐞𝐭.rkt"))

(define-values (domℛ codℛ ∘ℛ ?ℛ =ℛ) (𝐏𝐫𝐨𝐬𝐞𝐭))

(provide Arr_𝐏𝐫𝐨𝐬𝐞𝐭)
(define (Arr_𝐏𝐫𝐨𝐬𝐞𝐭 . _) (values dom cod ∘ ? =))

(define dom
  (match-λ
    [`((,j ,p) (,q ,i))
     (define a (domℛ i))
     (define b (domℛ j))
     `((,b ,p) (,p ,a))]))
(define cod
  (match-λ
    [`((,j ,p) (,q ,i))
     (define c (codℛ i))
     (define d (codℛ j))
     `((,d ,q) (,q ,c))]))
(define ∘
  (case-λ
    [(s) s]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,l ,q) (,r ,k))
         `((,j ,p) (,q ,i)))
        #:when
        (and (=ℛ (domℛ l) (codℛ j))
             (=ℛ (domℛ k) (codℛ i)))
        `((,(∘ℛ l j) ,p) (,r ,(∘ℛ k i)))])]
    [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))
(define ?
  (match-λ
    [`((,j ,p) (,q ,i))
     #:when
     (and (?ℛ j) (?ℛ p) (=ℛ (domℛ j) (codℛ p))
          (?ℛ q) (?ℛ i) (=ℛ (domℛ q) (codℛ i)))
     (=ℛ (∘ℛ j p) (∘ℛ q i))]
    [_ #f]))
(define =
  (case-λ
    [(_) #t]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,n ,r) (,s ,m))
         `((,j ,p) (,q ,i)))
        (and (=ℛ n j) (=ℛ r p)
             (=ℛ s q) (=ℛ m i))]
       [(_ _) #f])]
    [(s1 s2 . s*) (and (= s1 s2) (apply = s2 s*))]))

(module+ test
  (require rackunit)

  ;; Objects in ℛ
  (define a~ '(a . a)) (check-pred ?ℛ a~)
  (define b~ '(b . b)) (check-pred ?ℛ b~)
  (define c~ '(c . c)) (check-pred ?ℛ c~)
  (define d~ '(d . d)) (check-pred ?ℛ d~)
  (define e~ '(e . e)) (check-pred ?ℛ e~)
  (define f~ '(f . f)) (check-pred ?ℛ f~)
  (define g~ '(g . g)) (check-pred ?ℛ g~)
  (define h~ '(h . h)) (check-pred ?ℛ h~)

  ;; Morphisms in ℛ
  (define p~ '(a . b)) (check-pred ?ℛ p~)
  (define q~ '(c . d)) (check-pred ?ℛ q~)
  (define r~ '(e . f)) (check-pred ?ℛ r~)
  (define s~ '(g . h)) (check-pred ?ℛ s~)

  (define i~ '(a . c)) (check-pred ?ℛ i~)
  (define j~ '(b . d)) (check-pred ?ℛ j~)
  (define k~ '(c . e)) (check-pred ?ℛ k~)
  (define l~ '(d . f)) (check-pred ?ℛ l~)
  (define m~ '(e . g)) (check-pred ?ℛ m~)
  (define n~ '(f . h)) (check-pred ?ℛ n~)


  ;; Objects in Arr(ℛ)
  (define a `((,b~ ,p~) (,p~ ,a~))) (check-pred ? a) ; p~
  (define b `((,d~ ,q~) (,q~ ,c~))) (check-pred ? b) ; q~
  (define c `((,f~ ,r~) (,r~ ,e~))) (check-pred ? c) ; r~
  (define d `((,h~ ,s~) (,s~ ,g~))) (check-pred ? d) ; s~

  ;; Morphisms in Arr(ℛ)
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
