#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ2 (Listof (List A S S)))
(define ℳ2
  '([#\x s0 o]
    [#\y s0 b]
    [#\x b  b]
    [#\y b  b]
    [#\x o  o]
    [#\y o  o]))

(: A 𝒮) (define-type A (∪ #\x #\y))
(: S 𝒮) (define-type S (∪ 's0 'b 'o))
(: s0 S) (define s0 's0)
(: φ (→ (× A S) S))
(define (φ a s)
  (or
   (for/or ([i : (List A S S) (in-list ℳ2)])
     (match i
       [`(,(? (curry eq? a))
          ,(? (curry eq? s))
          ,s)
        s]
       [_ #f]))
   (raise-arguments-error
    'φ "invalid character or state"
    "character" a
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
  (check-eq? 'o (run "xy"))
  (check-eq? 'b (run "yx"))
  (check-eq? 'o (run "xx")))

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
