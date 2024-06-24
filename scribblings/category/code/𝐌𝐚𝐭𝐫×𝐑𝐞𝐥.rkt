#lang racket/base

(require racket/match math/matrix)

(define (rand m n) (random 1 9))

;; Category of Matrices ℳ
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))
(define (?ℳ m) (matrix? m))
(define =ℳ
  (case-λ
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (=ℳ m1 m2) (apply =ℳ m2 m*))]))

;; Objects in ℳ
(define a0 (identity-matrix 1)) (?ℳ a0)
(define b0 (identity-matrix 2)) (?ℳ b0)
(define c0 (identity-matrix 3)) (?ℳ c0)
(define d0 (identity-matrix 4)) (?ℳ d0)

;; Morphisms in ℳ
(define f0 (build-matrix 2 1 rand)) (?ℳ f0)
(define g0 (build-matrix 3 2 rand)) (?ℳ g0)
(define h0 (build-matrix 4 3 rand)) (?ℳ h0)


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
(define a1 '(a . a)) (?ℛ a1)
(define b1 '(b . b)) (?ℛ b1)
(define c1 '(c . c)) (?ℛ c1)
(define d1 '(d . d)) (?ℛ d1)

;; Morphisms in ℛ
(define f1 '(a . b)) (?ℛ f1)
(define g1 '(b . c)) (?ℛ g1)
(define h1 '(c . d)) (?ℛ h1)


;; Product Category ℳ × ℛ
(define (× . m*) m*)
(define dom (match-λ [`(,m ,r) (× (domℳ m) (domℛ r))]))
(define cod (match-λ [`(,m ,r) (× (codℳ m) (codℛ r))]))
(define (∘ p . p*)
  (define m* (map car  (cons p p*)))
  (define r* (map cadr (cons p p*)))
  (× (apply ∘ℳ m*) (apply ∘ℛ r*)))
(define (? p)
  (and (list? p) (eqv? 2 (length p))
       (?ℳ (car  p))
       (?ℛ (cadr p))))
(define (= . p*)
  (define m* (map car  p*))
  (define r* (map cadr p*))
  (and (apply =ℳ m*) (apply =ℛ r*)))

;; Objects in ℳ × ℛ
(define a (× a0 a1)) (? a) ; (a0, a1)
(define b (× b0 b1)) (? b) ; (b0, b1)
(define c (× c0 c1)) (? c) ; (c0, c1)
(define d (× d0 d1)) (? d) ; (d0, d1)

;; Morphisms in ℳ × ℛ
(define f (× f0 f1)) (? f) ; (f0, f1)
(define g (× g0 g1)) (? g) ; (g0, g1)
(define h (× h0 h1)) (? h) ; (h0, h1)

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
