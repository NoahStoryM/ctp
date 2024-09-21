#lang typed/racket/base/no-check

(require racket/match)

(provide SliF)
(define (SliF ∘𝒞)
  (: 𝒞/- (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝒞 b c) (→𝐂𝐚𝐭 𝒞/b 𝒞/c))))
  (define (𝒞/- g)
    (: 𝒞/g (∀ ([x : 𝒞/b] [y : 𝒞/b]) (→ (→𝒞/b x y) (→𝒞/c (∘𝒞 g x) (∘𝒞 g y)))))
    (define 𝒞/g
      (match-λ
        [`((,x) (,y ,z))
         `((,(∘𝒞 g x)) (,(∘𝒞 g y) ,z))]))
    𝒞/g)
  𝒞/-)
