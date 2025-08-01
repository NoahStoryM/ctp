#lang racket/base

(require "private/utils.rkt")

(provide (all-defined-out))

(define (dom  _) ×)
(define (cod  _) ×)
(define (domᵇ _) ×)
(define (codᵇ _) ×)
(define (domʰ _) ×)
(define (codʰ _) ×)
(define (domᵛ _) ×)
(define (codᵛ _) ×)
(define (⨾ . proc*) (apply compose proc*))
(define ∘ (procedure-rename compose    '∘))
(define ∙ (procedure-rename compose    '∙))
(define ⊗ (procedure-rename compose    '⊗))
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


(require racket/match variant)

(define ((and/p . p*) . v*) (for/and ([p (in-list p*)]) (apply p v*)))
(define ((or/p . p*) . v*) (for/or ([p (in-list p*)]) (apply p v*)))
(define ((not/p p) . v*) (not (apply p v*)))

(define ((limit #:is-equal? [eq= equal?] . m*) . v*)
  (for/and ([m (in-list m*)])
    (match-define (vector f s t) m)
    (define vs (list-ref v* s))
    (define vt (list-ref v* t))
    (eq= (f vs) vt)))

(define ((coequalizer #:is-equal? [eq= equal?] m1 m2) #:tag [_ 0] . v*)
  (define-variant (#:tag [t1 0] . v1*) (apply m2 v*))
  (define-variant (#:tag [t2 0] . v2*) (apply m2 v*))
  (and (= t1 t2)
       (= (length v1*) (length v2*))
       (andmap eq= v1* v2*)))
