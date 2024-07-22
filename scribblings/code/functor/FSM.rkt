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

(: A 𝒮) (define-type A (∪ ∘𝒞 dom𝒞 cod𝒞 id𝒞))
(: S 𝒮) (define-type S (∪ 𝒞0 𝒞1 𝒞2))
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
  (define (recognizer a*) (φ* (reverse a*) s0))
  (check-eq? '𝒞1 (recognizer '(∘𝒞)))
  (check-eq? '𝒞0 (recognizer '(∘𝒞 cod𝒞)))
  (check-eq? '𝒞0 (recognizer '(∘𝒞 dom𝒞)))
  (check-eq? '𝒞1 (recognizer '(∘𝒞 cod𝒞 id𝒞)))
  (check-eq? '𝒞1 (recognizer '(∘𝒞 dom𝒞 id𝒞))))

(: ∗ ℒ) (define ∗ (∘ℒ))
(: F (→ #;A* (→ℒ ∗ ∗) (→𝒮 S S)))
(define F (curry φ*))

(module+ test
  (check-eq?
   ((F (∘ℒ '(dom𝒞) '(∘𝒞))) s0)
   ((∘𝒮 (F '(dom𝒞)) (F '(∘𝒞))) s0))
  (check-eq?
   ((F (∘ℒ '(id𝒞 dom𝒞) '(∘𝒞))) s0)
   ((∘𝒮 (F '(id𝒞 dom𝒞)) (F '(∘𝒞))) s0)))
