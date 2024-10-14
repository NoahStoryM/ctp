#lang racket/base

(require "private/utils.rkt")

(provide (all-defined-out))

(define (dom _) ×)
(define (cod _) ×)
(define (src _) ×)
(define (tgt _) ×)
(define ∘ (procedure-rename compose    '∘))
(define ∙ (procedure-rename compose    '∙))
(define ? (procedure-rename procedure? '?))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (eq? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(define × (∘))
(define ∼ (procedure-rename = '∼))
(define († dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define (∘𝒞† . m*) (apply ∘𝒞 (reverse m*)))
  (values cod𝒞 dom𝒞 ∘𝒞† ?𝒞 =𝒞))
(define ((÷ ∼𝒞) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 _)
  (values dom𝒞 cod𝒞 ∘𝒞 ?𝒞 ∼𝒞))
(define ((⊆ ?𝒟) dom𝒞 cod𝒞 ∘𝒞 _ =𝒞)
  (values dom𝒞 cod𝒞 ∘𝒞 ?𝒟 =𝒞))
