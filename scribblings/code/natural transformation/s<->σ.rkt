#lang typed/racket/base/no-check

(require (only-in "../functor/TFSM.rkt" 𝒢 [F𝒢 𝒞] [φ* F])
         "../../exercises/functor/make-path.rkt")
(provide s->σ σ->s)

(module+ test (require rackunit))

(define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))

(: S0 𝒞) (define S0 (make-path 𝒢 'S0 ""))

(: |(→𝒞 S0 _)| (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S0 X) (→𝒞 S0 Y)))))
(define (|(→𝒞 S0 _)| j)
  (define |(→𝒞 S0 j)| (λ (f) (∘𝒞 j f)))
  |(→𝒞 S0 j)|)

(: F (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (F X) (F Y)))))

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
    (define (ρ f) ((F f) s))
    (for ([f (in-list (list (make-path 𝒢 'S0 "1xyxyxx")))])
      (define X (cod𝒞 f))
      (check-eq? ((σ X) f) (ρ f)))))
