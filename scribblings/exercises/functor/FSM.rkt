#lang typed/racket/base/no-check

(require racket/match racket/function)
(provide ℳ2 A2 S2 s2 φ2 φ2* ρ2 Fφ2*)

(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(define-type A2 (∪ #\x #\y))
(define-type S2 (∪ 's2 'b2 'o2))

(: ℳ2 (Listof (Immutable-Vector A2 S2 S2)))
(define ℳ2
  '(#[#\nul s2 s2]
    #[#\nul b2 b2]
    #[#\nul o2 o2]

    #[#\x s2 o2]
    #[#\x b2 b2]
    #[#\x o2 o2]

    #[#\y s2 b2]
    #[#\y b2 b2]
    #[#\y o2 o2]))

(: s2 S2) (define s2 's2)
(: φ2 (→ (× A2 S2) S2))
(define (φ2 a s)
  (or
   (for/or ([i : (Immutable-Vector A2 S2 S2) (in-list ℳ2)])
     (match i
       [`#[,(? (curry eq? a))
           ,(? (curry eq? s))
           ,s]
        s]
       [_ #f]))
   (raise-arguments-error
    'φ2 "invalid character or state"
    "character" a
    "state" s)))

(define-type A2* (Listof A2))
(: φ2* (→ (× A2* S2) S2))
(define (φ2* a* s)
  (match a*
    ['() s]
    [`(,a . ,w) (φ2 a (φ2* w s))]))

(: ρ2 (→ A2* S2))
(define (ρ2 a*) (φ2* a* s2))

(module+ test
  (check-eq? s2 (ρ2 '(#\nul)))

  (check-eq? 'b2 (ρ2 '(#\y #\y)))
  (check-eq? 'o2 (ρ2 '(#\y #\x)))
  (check-eq? 'b2 (ρ2 '(#\x #\y)))
  (check-eq? 'o2 (ρ2 '(#\x #\x))))

(define-type ∗ Null)
(: Fφ2* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S2 S2)))
(define Fφ2* (curry φ2*))

(module+ test
  (define str* '("x" "y" "xx" "xy" "yx" "yy"))
  (for* ([i (in-list str*)]
         [j (in-list str*)])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-eq?
     ((Fφ2* (∘ℒ n m)) s2)
     ((∘𝒮 (Fφ2* n) (Fφ2* m)) s2))))
