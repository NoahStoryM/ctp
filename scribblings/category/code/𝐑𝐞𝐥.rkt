#lang racket/base

(require racket/match)

;; Category of Binary Relations
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (pair? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a '(a . a))
(define b '(b . b))
(define c '(c . c))
(define d '(d . d))

;; Morphisms
(define f '(a . b))
(define g '(b . c))
(define h '(c . d))

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
