#lang typed/racket/base/no-check

(require math/matrix)

(: rand (→ Index Index Any))
(define (rand m n) (random 1 9))

(: number->symbol (→ Number Symbol))
(define (number->symbol z) (string->symbol (number->string z)))

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

;; Vertical Composition of Natural Transformations between ℳ and ℛ
(define ·ℛ
  (case-lambda
    [() values]
    [(α) α]
    [(α . α*)
     (define composed
       (λ (f)
         (define a (domℳ f))
         (define (α->αdom α) (α a))
         (define αdom* (apply ∘ℛ (map α->αdom α*)))
         (∘ℛ (α f) αdom*)))
     composed]))

;; Functors from ℳ to ℛ
(: F (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (F a) (F b))))))
(: G (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (G a) (G b))))))
(: H (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (H a) (H b))))))
(define (F m) (cons                 (matrix-num-cols m)                  (matrix-num-rows m)))
(define (G m) (cons (number->string (matrix-num-cols m)) (number->string (matrix-num-rows m))))
(define (H m) (cons (number->symbol (matrix-num-cols m)) (number->symbol (matrix-num-rows m))))

;; Natural Transformations from F to G
(: α (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (F a) (G b))))))
(: β (case→ (→ ℳ ℛ) (∀ ([a : ℳ] [b : ℳ]) (→ (→ℳ a b) (→ℛ (G a) (H b))))))
(define (α m) (cons                 (matrix-num-cols m)  (number->string (matrix-num-rows m))))
(define (β m) (cons (number->string (matrix-num-cols m)) (number->symbol (matrix-num-rows m))))

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

;;;
(=ℛ (domℛ (α a)) (F a))
(=ℛ (codℛ (α a)) (G a))

(=ℛ (α f) (∘ℛ (G f) (α a)) (∘ℛ (α b) (F f)))

(=ℛ (α (∘ℳ g f))
    (∘ℛ (α g) (F f))
    (∘ℛ (G g) (α f))
    (∘ℛ (α c) (F g) (F f))
    (∘ℛ (G g) (α b) (F f))
    (∘ℛ (G g) (G f) (α a)))

(=ℛ ((·ℛ β α) f)
    (∘ℛ (H f) (β a) (α a))
    (∘ℛ (β b) (G f) (α a))
    (∘ℛ (β b) (α b) (F f)))
