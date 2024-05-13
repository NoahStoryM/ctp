#lang typed/racket/base/no-check

(require math/matrix)

(: rand (→ Index Index Any))
(define (rand m n) (random 1 9))

;; Category of Matrices ℳ
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))
(define (?ℳ m) (matrix? m))
(define =ℳ
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (=ℳ m1 m2) (apply =ℳ m*))]))

;; Category of Binary Relations ℛ
(define (domℛ r) (define o (car r)) (cons o o))
(define (codℛ r) (define o (cdr r)) (cons o o))
(define ∘ℛ
  (case-lambda
    [(r) r]
    [(r1 r2) (match* (r1 r2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(r1 r2 . r*) (apply ∘ℛ (∘ℛ r1 r2) r*)]))
(define (?ℛ r) (pair? r))
(define =ℛ
  (case-lambda
    [(_) #t]
    [(r1 r2) (equal? r1 r2)]
    [(r1 r2 . r*) (and (=ℛ r1 r2) (apply =ℛ r*))]))

;; Functors from ℳ to ℛ
(: F (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (F a) (F b))))))
(define (F m) (cons (matrix-num-cols m) (matrix-num-rows m)))

;; Objects in ℳ
(: a ℳ)
(: b ℳ)
(: c ℳ)
(define a (identity-matrix 1))
(define b (identity-matrix 2))
(define c (identity-matrix 3))

;; Morphisms in ℳ
(: f (→ℳ a b))
(: g (→ℳ b c))
(define f (build-matrix 2 1 rand))
(define g (build-matrix 3 2 rand))

;; Preservation of domain and codomain
(=ℛ (F a) (domℛ (F f)) (F (domℳ f)))
(=ℛ (F b) (codℛ (F f)) (F (codℳ f)))

;; Preservation of identity morphisms
(=ℳ    a  (domℳ    a)  (codℳ    a))
(=ℛ (F a) (domℛ (F a)) (codℛ (F a)))

;; Preservation of composition
(=ℛ (∘ℛ (F g) (F f)) (F (∘ℳ g f)))
