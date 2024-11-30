#lang racket/base

(provide †)
(define († dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define (⨾𝒞 . m*) (apply ∘𝒞 (reverse m*)))
  (values cod𝒞 dom𝒞 ⨾𝒞 ?𝒞 =𝒞))
