#lang typed/racket/base/no-check

(require (only-in (file "../functor/TFSM.rkt")
                  make-path [φ* F] [→F𝒢 →𝒞]))
(provide s->σ σ->s)

(module+ test (require rackunit))

(: 𝒞 𝐂𝐚𝐭)
(: ∘𝒞 (∀ ([X : 𝒞] [Y : 𝒞] [C : 𝒞] ... [Z : 𝒞]) (→ (× (→𝒞 X Y) (→𝒞 Y C) ...) (→𝒞 X Z))))
(define (𝒞 m) m)
(define ∘𝒞 append)

(: S0 𝒞)
(define S0 (make-path "" 'S0))

(: |(→𝒞 S0 _)| (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S0 X) (→𝒞 S0 Y)))))
(define (|(→𝒞 S0 _)| j)
  (define |(→𝒞 S0 j)| (λ (f) (∘𝒞 j f)))
  |(→𝒞 S0 j)|)

(: F (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (F X) (F Y)))))

(: s->σ (→ (F S0) (⇒ |(→𝒞 S0 _)| F)))
(define (s->σ s)
  (: σ (∀ ([X : 𝒞] [Y : 𝒞]) (→ (→𝒞 X Y) (→ (→𝒞 S0 X) (F Y)))))
  (define (σ j) (λ (f) ((F (∘𝒞 j f)) s)))
  σ)

(: σ->s (→ (⇒ |(→𝒞 S0 _)| F) (F S0)))
(define (σ->s σ)
  (: s (F S0))
  (define s ((σ S0) S0))
  s)

(module+ test
  (for ([s (in-list '(s0 a0 r0))])
    (define σ (s->σ s))

    (: j (→𝒞 S1 S2)) (define j (make-path "yyxq2xy" 'S1))
    (: f (→𝒞 S0 S1)) (define f (make-path "1xyxyxx" 'S0))

    (displayln s)
    (pretty-print j)
    (pretty-print f)
    (displayln ((σ j) f))
    (newline)))
