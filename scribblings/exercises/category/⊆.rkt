#lang racket/base

(provide ⊆)
(define ((⊆ ?𝒟) dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (values dom𝒞 cod𝒞 ∘𝒞 ?𝒟 =𝒞))

(module+ test
  (require rackunit)
  (require racket/match)
  (require (file "../../code/category/𝐏𝐚𝐢𝐫.rkt"))

  (define-values (dom cod ∘ ? =)
    (let ()
      (define ?
        (match-λ
          [`(,a . ,b)
           (and (real? a) (real? b))
           (<= a b)]
          [_ #f]))
      ((compose (⊆ ?) 𝐏𝐚𝐢𝐫))))

  ;; Objects
  (define a '(0 . 0)) (check-pred ? a)
  (define b '(1 . 1)) (check-pred ? b)
  (define c '(2 . 2)) (check-pred ? c)
  (define d '(3 . 3)) (check-pred ? d)

  ;; Morphisms
  (define f '(0 . 1)) (check-pred ? f)
  (define g '(1 . 2)) (check-pred ? g)
  (define h '(2 . 3)) (check-pred ? h)


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
