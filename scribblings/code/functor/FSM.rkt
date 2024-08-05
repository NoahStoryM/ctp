#lang typed/racket/base/no-check

(require racket/match racket/function)
(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: 𝒞 (Listof (List A S S)))
(define 𝒞
  '([∘𝒞   𝒞2 𝒞1]
    [dom𝒞 𝒞1 𝒞0]
    [cod𝒞 𝒞1 𝒞0]
    [id𝒞  𝒞0 𝒞1]))

(: A 𝒮) (define-type A (∪ '∘𝒞 'dom𝒞 'cod𝒞 'id𝒞))
(: S 𝒮) (define-type S (∪ '𝒞0 '𝒞1 '𝒞2))
(: s0 S) (define s0 '𝒞2)
(: φ (→ (× A S) S))
(define (φ a s)
  (or
   (for/or ([i : (List A S S) (in-list 𝒞)])
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

(module+ test
  (check-eq? '𝒞1 (φ* '(∘𝒞) s0))
  (check-eq? '𝒞0 (φ* '(cod𝒞 ∘𝒞) s0))
  (check-eq? '𝒞0 (φ* '(dom𝒞 ∘𝒞) s0))
  (check-eq? '𝒞1 (φ* '(id𝒞 cod𝒞 ∘𝒞) s0))
  (check-eq? '𝒞1 (φ* '(id𝒞 dom𝒞 ∘𝒞) s0)))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: Fφ* (→ #;A* (→ℒ ∗ ∗) (→𝒮 S S)))
(define Fφ* (curry φ*))

(module+ test
  (check-eq? '𝒞1 ((Fφ* '(∘𝒞)) s0))
  (check-eq? '𝒞0 ((Fφ* '(cod𝒞 ∘𝒞)) s0))
  (check-eq? '𝒞0 ((Fφ* '(dom𝒞 ∘𝒞)) s0))
  (check-eq? '𝒞1 ((Fφ* '(id𝒞 cod𝒞 ∘𝒞)) s0))
  (check-eq? '𝒞1 ((Fφ* '(id𝒞 dom𝒞 ∘𝒞)) s0)))

(module+ test
  (check-eq?
   ((Fφ* (∘ℒ '(dom𝒞) '(∘𝒞))) s0)
   ((∘𝒮 (Fφ* '(dom𝒞)) (Fφ* '(∘𝒞))) s0))
  (check-eq?
   ((Fφ* (∘ℒ '(id𝒞 dom𝒞) '(∘𝒞))) s0)
   ((∘𝒮 (Fφ* '(id𝒞 dom𝒞)) (Fφ* '(∘𝒞))) s0)))
