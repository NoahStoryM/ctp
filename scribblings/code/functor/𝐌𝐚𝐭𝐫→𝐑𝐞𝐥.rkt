#lang typed/racket/base/no-check

(require math/matrix)
(require (file "../category/𝐌𝐚𝐭𝐫.rkt")
         (file "../category/𝐑𝐞𝐥.rkt"))

(: rand (→ Index Index Any))
(define (rand m n) (random 1 9))

(: ℳ 𝐂𝐚𝐭)
(: domℳ (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) a)))
(: codℳ (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) b)))
(: ∘ℳ (∀ ([a : ℳ] [b : ℳ] [c : ℳ] ... [z : ℳ]) (→ (× (→ℳ a b) (→ℳ b c) ...) (→ℳ a z))))
(: ?ℳ (pred (∀ ([a : ℳ] [b : ℳ]) (→ℳ a b))))
(: =ℳ (∀ ([a : ℳ] [b : ℳ] [c : ℳ] [d : ℳ] ...) (→ (× (→ℳ a b) (→ℳ c d) ...) Boolean)))
(define (ℳ m) m)
(define-values (domℳ codℳ ∘ℳ ?ℳ =ℳ) (𝐌𝐚𝐭𝐫))

(: ℛ 𝐂𝐚𝐭)
(: domℛ (∀ ([a : ℛ] [b : ℛ]) (→ (→ℛ a b) a)))
(: codℛ (∀ ([a : ℛ] [b : ℛ]) (→ (→ℛ a b) b)))
(: ∘ℛ (∀ ([a : ℛ] [b : ℛ] [c : ℛ] ... [z : ℛ]) (→ (× (→ℛ a b) (→ℛ b c) ...) (→ℛ a z))))
(: ?ℛ (pred (∀ ([a : ℛ] [b : ℛ]) (→ℛ a b))))
(: =ℛ (∀ ([a : ℛ] [b : ℛ] [c : ℛ] [d : ℛ] ...) (→ (× (→ℛ a b) (→ℛ c d) ...) Boolean)))
(define (ℛ m) m)
(define-values (domℛ codℛ ∘ℛ ?ℛ =ℛ) (𝐑𝐞𝐥))

;; Functors from ℳ to ℛ
(: F (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (F a) (F b)))))
(define (F m) (cons (matrix-num-cols m) (matrix-num-rows m)))

(module+ test
  (require rackunit)

  ;; Objects in ℳ
  (: a ℳ) (define a (identity-matrix 1)) (check-pred ?ℳ a) (check-pred ?ℛ (F a))
  (: b ℳ) (define b (identity-matrix 2)) (check-pred ?ℳ b) (check-pred ?ℛ (F b))
  (: c ℳ) (define c (identity-matrix 3)) (check-pred ?ℳ c) (check-pred ?ℛ (F c))

  ;; Morphisms in ℳ
  (: f (→ℳ a b)) (define f (build-matrix 2 1 rand)) (check-pred ?ℳ f) (check-pred ?ℛ (F f))
  (: g (→ℳ b c)) (define g (build-matrix 3 2 rand)) (check-pred ?ℳ g) (check-pred ?ℛ (F g))

  ;; Preservation of domain and codomain
  (check-true (=ℛ (F a) (domℛ (F f)) (F (domℳ f))))
  (check-true (=ℛ (F b) (codℛ (F f)) (F (codℳ f))))

  ;; Preservation of identity morphisms
  (check-true (=ℳ    a  (domℳ    a)  (codℳ    a)))
  (check-true (=ℛ (F a) (domℛ (F a)) (codℛ (F a))))

  ;; Preservation of composable pairs
  (check-true (=ℛ (∘ℛ (F g) (F f)) (F (∘ℳ g f)))))
