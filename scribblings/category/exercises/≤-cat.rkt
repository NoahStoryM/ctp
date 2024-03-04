#lang racket/base

(require racket/match)

;; ≤ Category
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))

(define (morphism? m)
  (match m
    [`(,a . ,b)
     (and (real? a) (real? b)
          (<= a b))]
    [_ #f]))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))
