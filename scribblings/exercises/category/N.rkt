#lang racket/base

(require racket/match)
(require "../../code/category/Pair.rkt"
         "../../code/category/sub.rkt")

(define ?
  (match-λ
    [`(,(? exact-nonnegative-integer?)
       .
       ,(? exact-nonnegative-integer?))
     #t]
    [_ #f]))

(provide ℕ)
(define ℕ (compose (⊆ ?) 𝐏𝐚𝐢𝐫))

(module+ test
  (require "../../code/category/check.rkt")

  ;; Objects
  (define a '(0 . 0))
  (define b '(1 . 1))
  (define c '(2 . 2))
  (define d '(3 . 3))

  ;; Morphisms
  (define f '(0 . 1))
  (define g '(1 . 2))
  (define h '(2 . 3))

  (define check-ℕ (check-cat ℕ))
  (check-ℕ a b c d f g h))
