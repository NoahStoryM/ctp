#lang racket/base

(require racket/match)

(provide dom+ cod+ ∘+ ?+ =+)
(define (dom+ . dom*)
  (define dom
    (match-λ
      [`(,m . ,n)
       (define dom (list-ref dom* n))
       (cons (dom m) n)]))
  dom)
(define (cod+ . cod*)
  (define cod
    (match-λ
      [`(,m . ,n)
       (define cod (list-ref cod* n))
       (cons (cod m) n)]))
  cod)
(define (∘+ . ∘*)
  (define (∘ t . t*)
    (define n (cdr t))
    (define m* (map car (cons t t*)))
    (define ∘ (list-ref ∘* n))
    (cons (apply ∘ m*) n))
  ∘)
(define (?+ . ?*)
  (define len (length ?*))
  (define ?
    (match-λ
      [`(,m . ,n)
       #:when (and (<= 0 n) (< n len))
       (define ? (list-ref ?* n))
       (? m)]
      [_ #f]))
  ?)
(define (=+ . =*)
  (define (= t . t*)
    (define n (cdr t))
    (define m* (map car (cons t t*)))
    (define = (list-ref =* n))
    (apply = m*))
  =)

(module+ test
  (require math/matrix)
  (require "../../code/category/check.rkt"
           "../../code/category/Matr.rkt"
           "../../code/category/Pair.rkt")
  (define (rand m n) (random 1 9))

  (define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
  (define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))

  (define dom (dom+ domℳ dom𝒫))
  (define cod (cod+ codℳ cod𝒫))
  (define ∘ (∘+ ∘ℳ ∘𝒫))
  (define ? (?+ ?ℳ ?𝒫))
  (define = (=+ =ℳ =𝒫))
  (define (𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

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

  (define check-𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫 (check-cat 𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫))
  (for ([i (in-naturals)]
        [a2 (in-list (list a0 a1))]
        [b2 (in-list (list b0 b1))]
        [c2 (in-list (list c0 c1))]
        [d2 (in-list (list d0 d1))]
        [f2 (in-list (list f0 f1))]
        [g2 (in-list (list g0 g1))]
        [h2 (in-list (list h0 h1))])
    ;; Objects in ℳ + 𝒫
    (define a (cons a2 i))
    (define b (cons b2 i))
    (define c (cons c2 i))
    (define d (cons d2 i))

    ;; Morphisms in ℳ + 𝒫
    (define f (cons f2 i))
    (define g (cons g2 i))
    (define h (cons h2 i))

    (check-𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫 a b c d f g h)))
