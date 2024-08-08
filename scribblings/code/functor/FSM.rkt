#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ1 (Listof (List A1 S1 S1)))
(define ℳ1
  '([#\x s s]
    [#\y s b]
    [#\y b b]
    [#\x b s]))

(: A1 𝒮) (define-type A1 (∪ #\x #\y))
(: S1 𝒮) (define-type S1 (∪ 's 'b))
(: s0 S1) (define s0 's)
(: φ1 (→ (× A1 S1) S1))
(define (φ1 a s)
  (or
   (for/or ([i : (List A1 S1 S1) (in-list ℳ1)])
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

(define-type A1* (Listof A1))
(: φ1* (→ (× A1* S1) S1))
(define (φ1* a* s)
  (match a*
    ['() s]
    [`(,a . ,w) (φ1 a (φ1* w s))]))

(: run (→ String S1))
(define (run str) (φ1* (reverse (string->list str)) s0))

(module+ test
  (check-eq? 'b (run "yy"))
  (check-eq? 'b (run "xy"))
  (check-eq? 's (run "yx"))
  (check-eq? 's (run "xx")))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: Fφ1* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S1 S1)))
(define Fφ1* (curry φ1*))

(module+ test
  (for* ([i (in-list '("x" "y" "xx" "xy" "yx" "yy"))]
         [j (in-list '("x" "y" "xx" "xy" "yx" "yy"))])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-eq?
     ((Fφ1* (∘ℒ m n)) s0)
     ((∘𝒮 (Fφ1* m) (Fφ1* n)) s0))))
