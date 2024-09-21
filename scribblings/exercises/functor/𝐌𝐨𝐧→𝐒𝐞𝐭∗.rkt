#lang typed/racket/base/no-check

#|
U((A, ∘, a)) = (A, a)

f: (A, ∘, a) → (B, ·, b)
U(f): (A, a) → (B, b)

U(b) = U(f(a)) = U(f)(a) = b
|#

(: U (∀ ([a : 𝐌𝐨𝐧] [b : 𝐌𝐨𝐧]) (→ (→𝐌𝐨𝐧 a b) (→𝐒𝐞𝐭∗ (U a) (U b)))))
(define ((U f) a) (f a))
