#lang racket/base

(provide |\|)
(define ((|\| ∼𝒞) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (values dom𝒞 cod𝒞 ∘𝒞 ?𝒞 ∼𝒞))

(module+ test
  (require rackunit)
  (require math/matrix)
  (require (file "../../code/category/𝐌𝐚𝐭𝐫.rkt"))

  (define-values (dom cod ∘ ? =)
    (let ()
      (define ∼
        (case-λ
          [(_) #t]
          [(m1 m2)
           (and (eq? (matrix-num-cols m1) (matrix-num-cols m2))
                (eq? (matrix-num-rows m1) (matrix-num-rows m2)))]
          [(m1 m2 . m*) (and (∼ m1 m2) (apply ∼ m2 m*))]))
      ((compose (|\| ∼) 𝐌𝐚𝐭𝐫))))

  (define (rand m n) (random 1 9))

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
