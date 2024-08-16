#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ2 (Listof (List A2 S2 S2)))
(define ℳ2
  '([#\x s o]
    [#\y s b]
    [#\x b b]
    [#\y b b]
    [#\x o o]
    [#\y o o]))

(: A2 𝒮) (define-type A2 (∪ #\x #\y))
(: S2 𝒮) (define-type S2 (∪ 's 'b 'o))
(: s2 S2) (define s2 's)
(: φ2 (→ (× A2 S2) S2))
(define (φ2 a s)
  (or
   (for/or ([i : (List A2 S2 S2) (in-list ℳ2)])
     (match i
       [`(,(? (curry eq? a))
          ,(? (curry eq? s))
          ,s)
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

(: run (→ String S2))
(define (run str) (φ2* (reverse (string->list str)) s2))

(module+ test
  (check-eq? 'b (run "yy"))
  (check-eq? 'o (run "xy"))
  (check-eq? 'b (run "yx"))
  (check-eq? 'o (run "xx")))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: Fφ2* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S2 S2)))
(define Fφ2* (curry φ2*))

(module+ test
  (define str* '("x" "y" "xx" "xy" "yx" "yy"))
  (for* ([i (in-list str*)]
         [j (in-list str*)])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-eq?
     ((Fφ2* (∘ℒ m n)) s2)
     ((∘𝒮 (Fφ2* m) (Fφ2* n)) s2))))
