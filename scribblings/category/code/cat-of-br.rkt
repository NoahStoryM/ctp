#lang racket/base

(require racket/match)

;; Category of Binary Relations
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))

(define (morphism? m) (pair? m))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

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
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

;; Associativity of composition
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity
(morphism=? a (dom a) (cod a))

;; Identity and composition
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
