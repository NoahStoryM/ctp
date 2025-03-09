#lang racket/base

(require racket/match)

(provide 𝐏𝐚𝐢𝐫)
(define (𝐏𝐚𝐢𝐫 . _) (values dom cod ∘ ? =))

(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2) (match* (m2 m1) [(`(,a . ,_) `(,_ . ,b)) `(,a . ,b)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (pair? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt")

  ;; Objects
  (define a '(a . a))
  (define b '(b . b))
  (define c '(c . c))
  (define d '(d . d))

  ;; Morphisms
  (define f '(a . b))
  (define g '(b . c))
  (define h '(c . d))

  (define check-𝐏𝐚𝐢𝐫 (check-cat 𝐏𝐚𝐢𝐫))
  (check-𝐏𝐚𝐢𝐫 a b c d f g h))
