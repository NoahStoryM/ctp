#lang typed/racket/base/no-check

(∀ (a b) (→ (→ b a) (→ b (→ a x) x)))
(define (cps i) (λ (n k) (k (i n))))
