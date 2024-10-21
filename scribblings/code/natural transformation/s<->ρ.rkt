#lang typed/racket/base/no-check

(require "../../exercises/functor/make-path.rkt"
         (only-in "../functor/TFSM.rkt" 𝒢 [F𝒢 𝒞]))
(require/typed "../functor/TFSM.rkt"
  [(φ* F) (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (F X) (F Y))))])

(module+ test (require rackunit))

(define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))

(: S 𝒞) (define S (make-path 𝒢 'S0 ""))

(: |(→𝒞 S _)| (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S X) (→𝒞 S Y)))))
(define (|(→𝒞 S _)| j)
  (define |(→𝒞 S j)| (λ (f) (∘𝒞 j f)))
  |(→𝒞 S j)|)

(: s->ρ (→ (F S) (⇒ |(→𝒞 S _)| F)))
(define (s->ρ s)
  (: ρ (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S X) (F Y)))))
  (define (ρ j) (λ (f) ((F (∘𝒞 j f)) s)))
  ρ)

(: ρ->s (→ (⇒ |(→𝒞 S _)| F) (F S)))
(define (ρ->s ρ)
  (define s ((ρ S) S))
  s)

(module+ test
  (for ([s (in-list '(s0 a0 r0))])
    (define ρ (s->ρ s))
    (check-eq? s (ρ->s ρ))
    (for ([w (in-list (list (make-path 𝒢 'S0 "1xyxyxx")))])
      (define T (cod𝒞 w))
      (check-eq? ((ρ T) w) ((F w) s)))))
