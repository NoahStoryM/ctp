#lang racket/base

(require math/matrix racket/match)
(require (file "𝐌𝐚𝐭𝐫.rkt") (file "𝐑𝐞𝐥.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define-values (domℛ codℛ ∘ℛ ?ℛ =ℛ) (𝐑𝐞𝐥))

(define (× . m*) m*)
(define (rand m n) (random 1 9))

(provide 𝐌𝐚𝐭𝐫×𝐑𝐞𝐥)
(define (𝐌𝐚𝐭𝐫×𝐑𝐞𝐥 . _) (values dom cod ∘ ? =))

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

(module+ test
  (require rackunit)

  ;; Objects in ℳ
  (define a0 (identity-matrix 1)) (check-pred ?ℳ a0)
  (define b0 (identity-matrix 2)) (check-pred ?ℳ b0)
  (define c0 (identity-matrix 3)) (check-pred ?ℳ c0)
  (define d0 (identity-matrix 4)) (check-pred ?ℳ d0)

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand)) (check-pred ?ℳ f0)
  (define g0 (build-matrix 3 2 rand)) (check-pred ?ℳ g0)
  (define h0 (build-matrix 4 3 rand)) (check-pred ?ℳ h0)


  ;; Objects in ℛ
  (define a1 '(a . a)) (check-pred ?ℛ a1)
  (define b1 '(b . b)) (check-pred ?ℛ b1)
  (define c1 '(c . c)) (check-pred ?ℛ c1)
  (define d1 '(d . d)) (check-pred ?ℛ d1)

  ;; Morphisms in ℛ
  (define f1 '(a . b)) (check-pred ?ℛ f1)
  (define g1 '(b . c)) (check-pred ?ℛ g1)
  (define h1 '(c . d)) (check-pred ?ℛ h1)


  ;; Objects in ℳ × ℛ
  (define a (× a0 a1)) (check-pred ? a) ; (a0, a1)
  (define b (× b0 b1)) (check-pred ? b) ; (b0, b1)
  (define c (× c0 c1)) (check-pred ? c) ; (c0, c1)
  (define d (× d0 d1)) (check-pred ? d) ; (d0, d1)

  ;; Morphisms in ℳ × ℛ
  (define f (× f0 f1)) (check-pred ? f) ; (f0, f1)
  (define g (× g0 g1)) (check-pred ? g) ; (g0, g1)
  (define h (× h0 h1)) (check-pred ? h) ; (h0, h1)


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
