#lang typed/racket/base/no-check

(require "../functor/make-path.rkt"
         (only-in "../../code/functor/TDFA.rkt" 𝒢 [F𝒢 𝒞]))
(require/typed "../../code/functor/TDFA.rkt"
  [(φ* G) (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 B A) (→ (G A) (G B))))])

(module+ test (require rackunit))

(define-values (dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞) (𝒞))

(: T 𝒞) (define T (make-path 𝒢 'S0 ""))

(: |(→𝒞 _ T)| (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 B A) (→ (→𝒞 T A) (→𝒞 T B)))))
(define (|(→𝒞 _ T)| i)
  (define |(→𝒞 i T)| (λ (f) (∘𝒞 f i)))
  |(→𝒞 i T)|)

(: t->ρ (→ (G T) (⇒ |(→𝒞 _ T)| G)))
(define (t->ρ t)
  (: ρ (∀ ([A : 𝒞] [B : 𝒞]) (→ (→𝒞 B A) (→ (→𝒞 T A) (G B)))))
  (define (ρ i) (λ (f) ((G (∘𝒞 f i)) t)))
  ρ)

(: ρ->t (→ (⇒ |(→𝒞 _ T)| G) (G T)))
(define (ρ->t ρ)
  (define t ((ρ T) T))
  t)

(module+ test
  (for ([t (in-list '(s0 a0 r0))])
    (define ρ (t->ρ t))
    (check-eq? t (ρ->t ρ))
    (for ([w (in-list (list (make-path 𝒢 'S0 "1xyxyxx")))])
      (define S (dom𝒞 w))
      (check-eq? ((ρ S) w) ((G w) t)))))
