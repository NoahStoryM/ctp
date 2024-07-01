#lang racket/base

(require math/matrix)

(define (rand m n) (random 1 9))

(provide 𝐌𝐚𝐭𝐫)
(define (𝐌𝐚𝐭𝐫 . _) (values dom cod ∘ ? =))

(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))
(define (? m) (matrix? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require rackunit)

  ;; Objects
  (define a (identity-matrix 1)) (check-pred ? a)
  (define b (identity-matrix 2)) (check-pred ? b)
  (define c (identity-matrix 3)) (check-pred ? c)
  (define d (identity-matrix 4)) (check-pred ? d)

  ;; Morphisms
  (define f (build-matrix 2 1 rand)) (check-pred ? f)
  (define g (build-matrix 3 2 rand)) (check-pred ? g)
  (define h (build-matrix 4 3 rand)) (check-pred ? h)


  ;; Existence of composition
  (check-true (= b (cod f) (dom g)))
  (check-true (= a (dom (∘ g f)) (dom f)))
  (check-true (= c (cod (∘ g f)) (cod g)))

  ;; Associativity of composition
  (check-true (= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f))))

  ;; Existence of identity morphisms
  (check-true (= a (dom a) (cod a)))

  ;; Composition and identity morphisms
  (check-true (= f (∘ f (dom f)) (∘ (cod f) f))))
