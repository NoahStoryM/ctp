#lang typed/racket/base/no-check

(require racket/set racket/promise
         "../category/Pair.rkt"
         "../category/Rel.rkt")

(provide 𝐏𝐑𝐞𝐥)
(define (𝐏𝐑𝐞𝐥 . _) (values domᵛ codᵛ ∙ domʰ codʰ ∘ ? =))

(define-values (dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫) (𝐏𝐚𝐢𝐫))
(define-values (domℛ codℛ ∘ℛ ?ℛ =ℛ) (𝐑𝐞𝐥))

(define domᵛ dom𝒫)
(define codᵛ cod𝒫)
(define ∙ ∘𝒫)
(define (domʰ m) (define a (domℛ (car m))) (cons a a))
(define (codʰ m) (define b (codℛ (cdr m))) (cons b b))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2)
     (define a (∘ℛ (car m1) (car m2)))
     (define b (∘ℛ (cdr m1) (cdr m2)))
     (cons a b)]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2)
     (and (=ℛ (car m1) (car m2))
          (=ℛ (cdr m1) (cdr m2)))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))
(define (? m)
  (and (?𝒫 m)
       (let ([a (car m)] [b (cdr m)])
         (and (?ℛ a) (?ℛ b)
              (=ℛ (domℛ a) (domℛ b))
              (=ℛ (codℛ a) (codℛ b))))))

(module+ test
  (require "check.rkt" rackunit)

  ;; Relations
  (define c (relation (lazy c) (lazy c) (set '(c0 . c0) '(c1 . c1) '(c2 . c2))))
  (define d (relation (lazy d) (lazy d) (set '(d0 . d0) '(d1 . d1) '(d2 . d2))))
  (define e (relation (lazy e) (lazy e) (set '(e0 . e0) '(e1 . e1) '(e2 . e2))))

  (define f (relation (lazy c) (lazy d) (set '(c0 . d0) '(c1 . d1) '(c2 . d2))))
  (define g (relation (lazy c) (lazy d) (set '(c0 . d1) '(c1 . d2) '(c2 . d0))))
  (define h (relation (lazy c) (lazy d) (set '(c0 . d2) '(c1 . d0) '(c2 . d1))))
  (define k (relation (lazy d) (lazy e) (set '(d0 . e0) '(d1 . e1) '(d2 . e2))))
  (define l (relation (lazy d) (lazy e) (set '(d0 . e1) '(d1 . e2) '(d2 . e0))))
  (define m (relation (lazy d) (lazy e) (set '(d0 . e2) '(d1 . e0) '(d2 . e1))))

  ;; 0-cells
  (define 𝒞 (cons c c))
  (define 𝒟 (cons d d))
  (define ℰ (cons e e))

  ;; 1-cells
  (define F (cons f f))
  (define G (cons g g))
  (define H (cons h h))
  (define K (cons k k))
  (define L (cons l l))
  (define M (cons m m))

  ;; 2-cells
  (define α0 (cons f g))
  (define β0 (cons g h))
  (define α1 (cons k l))
  (define β1 (cons l m))

  (define check-𝐏𝐑𝐞𝐥 (check-2-cat 𝐏𝐑𝐞𝐥))
  (check-𝐏𝐑𝐞𝐥 𝒞 𝒟 ℰ F G H K L M α0 β0 α1 β1))
