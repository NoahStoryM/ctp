#lang typed/racket/base/no-check

(require racket/match racket/function)
(provide ℳ1 A1 S1 s1 φ1 φ1* Fφ1*)

(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ1 (Listof (List A1 S1 S1)))
(define ℳ1
  '([#\x s1 s1]
    [#\y s1 b1]
    [#\y b1 b1]
    [#\x b1 s1]))

(: A1 𝒮) (define-type A1 (∪ #\x #\y))
(: S1 𝒮) (define-type S1 (∪ 's1 'b1))
(: s1 S1) (define s1 's1)
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
    'φ1 "invalid character or state"
    "character" a
    "state" s)))

(define-type A1* (Listof A1))
(: φ1* (→ (× A1* S1) S1))
(define (φ1* a* s)
  (match a*
    ['() s]
    [`(,a . ,w) (φ1 a (φ1* w s))]))

(: run (→ String S1))
(define (run str) (φ1* (reverse (string->list str)) s1))

(module+ test
  (check-eq? 'b1 (run "yy"))
  (check-eq? 'b1 (run "xy"))
  (check-eq? 's1 (run "yx"))
  (check-eq? 's1 (run "xx")))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: Fφ1* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S1 S1)))
(define Fφ1* (curry φ1*))

(module+ test
  (define str* '("x" "y" "xx" "xy" "yx" "yy"))
  (for* ([i (in-list str*)]
         [j (in-list str*)])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-eq?
     ((Fφ1* (∘ℒ m n)) s1)
     ((∘𝒮 (Fφ1* m) (Fφ1* n)) s1))))
