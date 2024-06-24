#lang racket/base

(require racket/match)

;; ≤ Category
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define ?
  (match-λ
    [`(,a . ,b)
     (and (real? a) (real? b)
          (<= a b))]
    [_ #f]))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a '(0 . 0)) (? a)
(define b '(1 . 1)) (? b)
(define c '(2 . 2)) (? c)
(define d '(3 . 3)) (? d)

;; Morphisms
(define f '(0 . 1)) (? f)
(define g '(1 . 2)) (? g)
(define h '(2 . 3)) (? h)

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
