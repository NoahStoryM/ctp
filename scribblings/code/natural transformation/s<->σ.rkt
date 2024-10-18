#lang typed/racket/base/no-check

(require (only-in (file "../functor/TFSM.rkt") [φ* F]))
(provide s->σ σ->s)

(: 𝒞 𝐂𝐚𝐭)
(define (𝒞 m) m)
(define ∘𝒞 append)

(: S 𝒞)
(define S '())

(: |(→𝒞 S _)| (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 A B) (→ (→𝒞 S A) (→𝒞 S B)))))
(define (|(→𝒞 S _)| f)
  (define |(→𝒞 S f)| (λ (i) (∘𝒞 f i)))
  |(→𝒞 S f)|)

(: F (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 A B) (→ (F A) (F B)))))

(: s->σ (→ (F S) (⇒ |(→𝒞 S _)| F)))
(define (s->σ s)
  (: σ (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 A B) (→ (→𝒞 S A) (F B)))))
  (define σ (λ (j) (λ (f) ((F (∘𝒞 j f)) s))))
  σ)

(: σ->s (→ (⇒ |(→𝒞 S _)| F) (F S)))
(define (σ->s σ)
  (: s (F S))
  (define s ((σ S) S))
  s)
