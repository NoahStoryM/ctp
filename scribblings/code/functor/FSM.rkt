#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ (Listof (List A S S)))
(define ℳ
  '([#\x b s]
    [#\x s s]
    [#\y s b]
    [#\y b b]))

(: A 𝒮) (define-type A (∪ #\x #\y))
(: S 𝒮) (define-type S (∪ 's 'b))
(: s0 S) (define s0 's)
(: φ (→ (× A S) S))
(define (φ a s)
  (or
   (for/or ([i : (List A S S) (in-list ℳ)])
     (match i
       [`(,(? (curry eq? a))
          ,(? (curry eq? s))
          ,s)
        s]
       [_ #f]))
   (raise-arguments-error
    'φ "invalid token or state"
    "token" a
    "state" s)))

(define-type A* (Listof A))
(: φ* (→ (× A* S) S))
(define (φ* a* s)
  (match a*
    ['() s]
    [`(,a . ,w) (φ a (φ* w s))]))

(: run (→ String S))
(define (run str) (φ* (reverse (string->list str)) s0))

(module+ test
  (check-eq? 'b (run "yy"))
  (check-eq? 'b (run "xy"))
  (check-eq? 's (run "yx"))
  (check-eq? 's (run "xx")))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: Fφ* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S S)))
(define Fφ* (curry φ*))

(module+ test
  (for* ([i (in-list '("x" "y" "xx" "xy" "yx" "yy"))]
         [j (in-list '("x" "y" "xx" "xy" "yx" "yy"))])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-eq?
     ((Fφ* (∘ℒ m n)) s0)
     ((∘𝒮 (Fφ* m) (Fφ* n)) s0))))
