#lang racket/base

(provide 𝐒𝐭𝐫)
(define (𝐒𝐭𝐫 . _) (values dom cod ∘ ? =))

(define (dom _) "")
(define (cod _) "")
(define (∘ . m*) (apply string-append m*))
(define (? m) (string? m))
(define (= m . m*) (apply string=? m m*))

(module+ test
  (require "../../code/category/check.rkt")

  ;; Morphisms
  (define f "123")
  (define g "abc")
  (define h "ABC")

  (define check-𝐒𝐭𝐫 (check-ooc 𝐒𝐭𝐫))
  (check-𝐒𝐭𝐫 f g h))
