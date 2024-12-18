#lang racket/base

(define call (λ (f m) (f m)))

(provide dom× cod× ∘× ?× =×)
(define (dom× . dom*) (define (dom m*) (map call dom* m*)) dom)
(define (cod× . cod*) (define (cod m*) (map call cod* m*)) cod)
(define (∘× . ∘*)
  (define (∘ . m**)
    (for/list ([∘ (in-list ∘*)]
               [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply ∘ m*)))
  ∘)
(define (?× . ?*)
  (define n (length ?*))
  (define (? m*)
    (and (list? m*) (eqv? n (length m*))
         (andmap call ?* m*)))
  ?)
(define (=× . =*)
  (define (= . m**)
    (for/and ([= (in-list =*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply = m*)))
  =)

(module+ test
  (require math/matrix)
  (require "../../code/category/check.rkt"
           "../../code/category/Matr.rkt"
           "../../code/category/Pair.rkt")
  (define (× . m*) m*)
  (define (rand m n) (random 1 9))

  (define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
  (define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))

  (define dom (dom× domℳ dom𝒫))
  (define cod (cod× codℳ cod𝒫))
  (define ∘ (∘× ∘ℳ ∘𝒫))
  (define ? (?× ?ℳ ?𝒫))
  (define = (=× =ℳ =𝒫))
  (define (𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

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
  (define a (× a0 a1)) ; (a0, a1)
  (define b (× b0 b1)) ; (b0, b1)
  (define c (× c0 c1)) ; (c0, c1)
  (define d (× d0 d1)) ; (d0, d1)

  ;; Morphisms in ℳ × 𝒫
  (define f (× f0 f1)) ; (f0, f1)
  (define g (× g0 g1)) ; (g0, g1)
  (define h (× h0 h1)) ; (h0, h1)

  (define check-𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 (check-cat 𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫))
  (check-𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫 a b c d f g h))
