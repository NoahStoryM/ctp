#lang racket/base

(require racket/match)

;; Category of Binary Relations ℛ
(define (domℛ r) (define o (car r)) (cons o o))
(define (codℛ r) (define o (cdr r)) (cons o o))
(define ∘ℛ
  (case-λ
    [(r) r]
    [(r1 r2) (match* (r1 r2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(r1 r2 . r*) (apply ∘ℛ (∘ℛ r1 r2) r*)]))
(define (?ℛ r) (pair? r))
(define =ℛ
  (case-λ
    [(_) #t]
    [(r1 r2) (equal? r1 r2)]
    [(r1 r2 . r*) (and (=ℛ r1 r2) (apply =ℛ r2 r*))]))

;; Objects in ℛ
(define a~ '(a . a)) (?ℛ a~)
(define b~ '(b . b)) (?ℛ b~)
(define c~ '(c . c)) (?ℛ c~)
(define d~ '(d . d)) (?ℛ d~)
(define e~ '(e . e)) (?ℛ e~)
(define f~ '(f . f)) (?ℛ f~)
(define g~ '(g . g)) (?ℛ g~)
(define h~ '(h . h)) (?ℛ h~)

;; Morphisms in ℛ
(define p~ '(a . b)) (?ℛ p~)
(define q~ '(c . d)) (?ℛ q~)
(define r~ '(e . f)) (?ℛ r~)
(define s~ '(g . h)) (?ℛ s~)

(define i~ '(a . c)) (?ℛ i~)
(define j~ '(b . d)) (?ℛ j~)
(define k~ '(c . e)) (?ℛ k~)
(define l~ '(d . f)) (?ℛ l~)
(define m~ '(e . g)) (?ℛ m~)
(define n~ '(f . h)) (?ℛ n~)


;; Arrow Category Arr(ℛ)
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

;; Objects in Arr(ℛ)
(define a `((,b~ ,p~) (,p~ ,a~))) (? a) ; p~
(define b `((,d~ ,q~) (,q~ ,c~))) (? b) ; q~
(define c `((,f~ ,r~) (,r~ ,e~))) (? c) ; r~
(define d `((,h~ ,s~) (,s~ ,g~))) (? d) ; s~

;; Morphisms in Arr(ℛ)
(define f `((,j~ ,p~) (,q~ ,i~))) (? f) ; (i~, j~)
(define g `((,l~ ,q~) (,r~ ,k~))) (? g) ; (k~, l~)
(define h `((,n~ ,r~) (,s~ ,m~))) (? h) ; (m~, n~)

;; Existence of composition
(= b (cod f) (dom g))
(= a (dom (∘ g f)) (dom f))
(= c (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= a (dom a) (cod a))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
