#lang typed/racket/base/no-check

(require math/array math/matrix)

(provide 𝐌𝐚𝐭𝐫)
(define (𝐌𝐚𝐭𝐫 . _) (values dom cod ∘ domʰ codʰ ⊗ ? =))

;; 0-cell
(define I #;(identity-matrix 0) (build-simple-array #(0 0) (λ (_) (error ""))))

(define (domʰ m) I)
(define (codʰ m) I)

(: ⊗ (∀ ([a : ℳ] [b : ℳ] [x : ℳ] [y : ℳ]) (→ (× (→ℳ a b) (→ℳ x y)) (→ℳ (⊗ a x) (⊗ b y)))))
(define (⊗ . m*)
  (let ([m* (remq* (list I) m*)])
    (if (null? m*) I (block-diagonal-matrix m*))))

(define (dom m) (define n (vector-ref (array-shape m) 1)) (if (zero? n) I (identity-matrix n)))
(define (cod m) (define n (vector-ref (array-shape m) 0)) (if (zero? n) I (identity-matrix n)))
(define (∘ m . m*) (apply matrix* m m*))
(define (? m) (and (array? m) (eqv? 2 (array-dims m))))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (and (array= m1 m2) #t)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt" rackunit)
  (define (rand m n) (random 1 9))

  ;; 1-cells
  (define F (identity-matrix 1))
  (define G (identity-matrix 2))
  (define H (identity-matrix 3))
  (define K (identity-matrix 4))
  (define L (identity-matrix 5))
  (define M (identity-matrix 6))

  ;; 2-cells
  (define α0 (build-matrix 2 1 rand))
  (define β0 (build-matrix 3 2 rand))
  (define α1 (build-matrix 5 4 rand))
  (define β1 (build-matrix 6 5 rand))

  (define check-𝐌𝐚𝐭𝐫 (check-oo2c 𝐌𝐚𝐭𝐫))
  (check-𝐌𝐚𝐭𝐫 F G H K L M α0 β0 α1 β1))
