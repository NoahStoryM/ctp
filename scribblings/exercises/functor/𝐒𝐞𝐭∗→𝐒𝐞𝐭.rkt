#lang typed/racket/base/no-check

#|
U((A, a)) = A

f: (A, a) → (B, b)
U(f): A → B
|#

(: U (∀ ([a : 𝐒𝐞𝐭∗] [b : 𝐒𝐞𝐭∗]) (→ (→𝐒𝐞𝐭∗ a b) (→𝐒𝐞𝐭 (U a) (U b)))))
(define ((U f) a) (f a))
