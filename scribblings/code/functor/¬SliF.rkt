#lang typed/racket/base/no-check

(require racket/match)

(provide ¬SliF)
(define (¬SliF ∘𝒞)
  (: -/𝒞 (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝒞 a b) (→𝐂𝐚𝐭 b/𝒞 a/𝒞))))
  (define (-/𝒞 f)
    (: f/𝒞 (∀ ([x : b/𝒞] [y : b/𝒞]) (→ (→b/𝒞 x y) (→a/𝒞 (∘𝒞 x f) (∘𝒞 y f)))))
    (define f/𝒞
      (match-λ
        [`((,z ,x) (,y))
         `((,z ,(∘𝒞 x f)) (,(∘𝒞 y f)))]))
    f/𝒞)
  -/𝒞)
