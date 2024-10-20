#lang typed/racket/base/no-check

(require "../../exercises/functor/make-path.rkt"
         (only-in "../functor/TFSM.rkt" 𝒢 [F𝒢 𝒞]))
(require/typed "../functor/TFSM.rkt"
  [(φ* F) (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (F X) (F Y))))])
(provide s->σ σ->s)

(module+ test (require rackunit))

(define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))

(: S0 𝒞) (define S0 (make-path 𝒢 'S0 ""))

(: |(→𝒞 S0 _)| (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S0 X) (→𝒞 S0 Y)))))
(define (|(→𝒞 S0 _)| j)
  (define |(→𝒞 S0 j)| (λ (f) (∘𝒞 j f)))
  |(→𝒞 S0 j)|)

(: s->σ (→ (F S0) (⇒ |(→𝒞 S0 _)| F)))
(define (s->σ s)
  (: σ (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S0 X) (F Y)))))
  (define (σ j)
    (define (ρ f) ((F (∘𝒞 j f)) s))
    ρ)
  σ)

(: σ->s (→ (⇒ |(→𝒞 S0 _)| F) (F S0)))
(define (σ->s σ)
  (define ρ (σ S0))
  (define s (ρ S0))
  s)

(module+ test
  (for ([s (in-list '(s0 a0 r0))])
    (define σ (s->σ s))
    (check-eq? s (σ->s σ))
    (for ([w (in-list (list (make-path 𝒢 'S0 "1xyxyxx")))])
      (define X (cod𝒞 w))
      (define ρ (σ X))
      (check-eq? (ρ w) ((F w) s)))))
