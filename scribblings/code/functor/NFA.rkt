#lang typed/racket/base/no-check

(require racket/function racket/match racket/set amb)
(provide ℳ3 A3 S3 s3 φ3 φ3* ρ3 Fφ3*)

(module+ test (require rackunit))

(: #;𝐑𝐞𝐥  ℛ 𝐂𝐚𝐭) (define (ℛ m) m) (define ∘ℛ compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(define-type A3 (∪ #\x #\y))
(define-type S3 (∪ 's3 'b3 'o3))

(: ℳ3 (Listof (Immutable-Vector A3 S3 S3)))
(define ℳ3
  '(#[#\nul s3 s3]
    #[#\nul b3 b3]
    #[#\nul o3 o3]

    #[#\x s3 s3]
    #[#\x s3 b3]

    #[#\y b3 o3]
    #[#\y o3 o3]))

(: s3 S3) (define s3 's3)
(: φ3 (→ (× A3 S3) S3))
(define (φ3 a s)
  (for/amb ([i (in-list ℳ3)])
    (match i
      [`#[,(? (curry eq? a))
          ,(? (curry eq? s))
          ,s]
       (amb s)]
      [_ (amb)])))

(define-type A3* (Listof A3))
(: φ3* (→ (× A3* S3) S3))
(define (φ3* a* s)
  (match a*
    ['() s]
    [`(,a . ,w) (φ3 a (φ3* w s))]))

(: ρ3 (→ A3* (𝒫 S3)))
(define (ρ3 a*) (for/set ([s (in-amb (φ3* a* s3))]) s))

(module+ test
  (check-equal? (ρ3 '())      (set s3))
  (check-equal? (ρ3 '(#\nul)) (set s3))

  (check-equal? (ρ3 '(#\x #\x #\y)) (set))
  (check-equal? (ρ3 '(#\y #\y #\x)) (set 'o3))
  (check-equal? (ρ3 '(#\y #\x #\x)) (set 'o3))
  (check-equal? (ρ3 '(#\x #\x #\x)) (set 's3 'b3)))

(define-type ∗ Null)
(: Fφ3* (→ #;A* (→ℒ ∗ ∗) (→ℛ S3 S3)))
(define Fφ3* (curry φ3*))

(module+ test
  (define str* '("x" "y" "xx" "xy" "yx" "yy"))
  (for* ([i (in-list str*)]
         [j (in-list str*)])
    (define m (reverse (string->list i)))
    (define n (reverse (string->list j)))
    (check-equal?
     (for/set ([s (in-amb ((Fφ3* (∘ℒ n m)) s3))]) s)
     (for/set ([s (in-amb ((∘ℛ (Fφ3* n) (Fφ3* m)) s3))]) s))))
