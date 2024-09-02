#lang racket/base

(require "private/utils.rkt")

(provide (all-defined-out))

(define × values)
(define (dom _) ×)
(define (cod _) ×)
(define (src _) ×)
(define (tgt _) ×)
(define ∘ (procedure-rename compose    '∘))
(define · (procedure-rename compose    '·))
(define ? (procedure-rename procedure? '?))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (eq? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))
