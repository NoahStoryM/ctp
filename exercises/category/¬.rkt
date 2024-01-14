#lang racket/base

(provide ¬)

(define (¬ dom𝒞 cod𝒞 ∘𝒞)
  (define (dom m) (cod𝒞 m))
  (define (cod m) (dom𝒞 m))
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values dom cod ∘))
