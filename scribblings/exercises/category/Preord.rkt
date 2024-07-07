#lang racket/base

(require racket/match)
(require (file "../../code/category/𝐏𝐚𝐢𝐫.rkt"))

(define-values (dom cod ∘ =)
  (call-with-values 𝐏𝐚𝐢𝐫 (λ (dom cod ∘ ? =) (values dom cod ∘ =))))

(provide Preord)
(define (Preord S? ≤)
  (define ?
    (match-λ
      [`(,a . ,b)
       (and (S? a) (S? b))
       (≤ a b)]
      [_ #f]))
  (values dom cod ∘ ? =))


(module+ test
  (require rackunit)

  (define ? (call-with-values 𝐏𝐚𝐢𝐫 (λ (dom cod ∘ ? =) ?)))

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
