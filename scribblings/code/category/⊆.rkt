#lang racket/base

(provide ⊆)
(define ((⊆ ?𝒟) dom𝒞 cod𝒞 ∘𝒞 _ =𝒞)
  (values dom𝒞 cod𝒞 ∘𝒞 ?𝒟 =𝒞))
