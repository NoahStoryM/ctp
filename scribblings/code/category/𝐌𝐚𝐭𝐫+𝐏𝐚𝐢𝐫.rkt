#lang racket/base

(require math/matrix racket/match)
(require (file "𝐌𝐚𝐭𝐫.rkt") (file "𝐏𝐚𝐢𝐫.rkt"))

(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))
(define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))

(provide 𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫)
(define (𝐌𝐚𝐭𝐫+𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

(define dom
  (match-λ
    [`(,m . 0) (cons (domℳ m) 0)]
    [`(,p . 1) (cons (dom𝒫 p) 1)]))
(define cod
  (match-λ
    [`(,m . 0) (cons (codℳ m) 0)]
    [`(,p . 1) (cons (cod𝒫 p) 1)]))
(define (∘ t . t*)
  (define v* (map car (cons t t*)))
  (match t
    [`(,m . 0) (cons (apply ∘ℳ v*) 0)]
    [`(,p . 1) (cons (apply ∘𝒫 v*) 1)]))
(define (? t)
  (and (pair? t)
       (case (cdr t)
         [(0) (?ℳ (car t))]
         [(1) (?𝒫 (car t))]
         [else #f])))
(define =
  (case-λ
    [(_) #t]
    [(t1 t2)
     (match* (t1 t2)
       [(`(,m1 . 0) `(,m2 . 0)) (=ℳ m1 m2)]
       [(`(,p1 . 1) `(,p2 . 1)) (=𝒫 p1 p2)]
       [(_ _) #f])]
    [(t1 t2 . t*) (and (= t1 t2) (apply = t2 t*))]))

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
