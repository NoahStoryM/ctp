#lang racket/base

(provide ¬)

;; Opposite Category
(define (¬ dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define (dom m) (cod𝒞 m))
  (define (cod m) (dom𝒞 m))
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (define (? m) (?𝒞 *))
  (define (= . m*) (apply =𝒞 m*))

  (values dom cod ∘ ? =))
