#lang racket/base

(require math/matrix racket/match)
(require "Matr.rkt" "Pair.rkt")

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))

(provide 𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫)
(define (𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

(define dom (match-λ [`(,m ,p) (list (domℳ m) (dom𝒫 p))]))
(define cod (match-λ [`(,m ,p) (list (codℳ m) (cod𝒫 p))]))
(define (∘ l . l*)
  (define m* (map car  (cons l l*)))
  (define p* (map cadr (cons l l*)))
  (list (apply ∘ℳ m*) (apply ∘𝒫 p*)))
(define (? l)
  (and (list? l) (eqv? 2 (length l))
       (?ℳ (car  l))
       (?𝒫 (cadr l))))
(define (= . l*)
  (define m* (map car  l*))
  (define p* (map cadr l*))
  (and (apply =ℳ m*) (apply =𝒫 p*)))

(module+ test
  (require "check.rkt")
  (define (rand m n) (random 1 9))

  ;; Objects in ℳ
  (define a0 (identity-matrix 1))
  (define b0 (identity-matrix 2))
  (define c0 (identity-matrix 3))
  (define d0 (identity-matrix 4))

  ;; Morphisms in ℳ
  (define f0 (build-matrix 2 1 rand))
  (define g0 (build-matrix 3 2 rand))
  (define h0 (build-matrix 4 3 rand))

  (define check-𝐌𝐚𝐭𝐫 (check-cat 𝐌𝐚𝐭𝐫))
  (check-𝐌𝐚𝐭𝐫 a0 b0 c0 d0 f0 g0 h0)

  ;; Objects in 𝒫
  (define a1 '(a . a))
  (define b1 '(b . b))
  (define c1 '(c . c))
  (define d1 '(d . d))

  ;; Morphisms in 𝒫
  (define f1 '(a . b))
  (define g1 '(b . c))
  (define h1 '(c . d))

  (define check-𝐏𝐚𝐢𝐫 (check-cat 𝐏𝐚𝐢𝐫))
  (check-𝐏𝐚𝐢𝐫 a1 b1 c1 d1 f1 g1 h1)

  ;; Objects in ℳ × 𝒫
  (define a (list a0 a1)) ; (a0, a1)
  (define b (list b0 b1)) ; (b0, b1)
  (define c (list c0 c1)) ; (c0, c1)
  (define d (list d0 d1)) ; (d0, d1)

  ;; Morphisms in ℳ × 𝒫
  (define f (list f0 f1)) ; (f0, f1)
  (define g (list g0 g1)) ; (g0, g1)
  (define h (list h0 h1)) ; (h0, h1)

  (define check-𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 (check-cat 𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫))
  (check-𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 a b c d f g h))
