#lang racket/base

(require math/matrix racket/match racket/promise racket/set)
(require (file "𝐌𝐚𝐭𝐫.rkt") (file "𝐏𝐚𝐢𝐫.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))

(define (× . m*) m*)
(define (rand m n) (random 1 9))

(provide 𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫)
(define (𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

(define dom (match-λ [`(,m ,p) (× (domℳ m) (dom𝒫 p))]))
(define cod (match-λ [`(,m ,p) (× (codℳ m) (cod𝒫 p))]))
(define (∘ l . l*)
  (define m* (map car  (cons l l*)))
  (define p* (map cadr (cons l l*)))
  (× (apply ∘ℳ m*) (apply ∘𝒫 p*)))
(define (? l)
  (and (list? l) (eqv? 2 (length l))
       (?ℳ (car  l))
       (?𝒫 (cadr l))))
(define (= . l*)
  (define m* (map car  l*))
  (define p* (map cadr l*))
  (and (apply =ℳ m*) (apply =𝒫 p*)))

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


  ;; Objects in 𝒫
  (define a1 '(a . a)) (check-pred ?𝒫 a1)
  (define b1 '(b . b)) (check-pred ?𝒫 b1)
  (define c1 '(c . c)) (check-pred ?𝒫 c1)
  (define d1 '(d . d)) (check-pred ?𝒫 d1)

  ;; Morphisms in 𝒫
  (define f1 '(a . b)) (check-pred ?𝒫 f1)
  (define g1 '(b . c)) (check-pred ?𝒫 g1)
  (define h1 '(c . d)) (check-pred ?𝒫 h1)


  ;; Objects in ℳ × 𝒫
  (define a (× a0 a1)) (check-pred ? a) ; (a0, a1)
  (define b (× b0 b1)) (check-pred ? b) ; (b0, b1)
  (define c (× c0 c1)) (check-pred ? c) ; (c0, c1)
  (define d (× d0 d1)) (check-pred ? d) ; (d0, d1)

  ;; Morphisms in ℳ × 𝒫
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
