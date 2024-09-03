#lang racket/base

(provide †)
(define († dom𝒞 cod𝒞 ∘𝒞 ?𝒞 =𝒞)
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values cod𝒞 dom𝒞 ∘ ?𝒞 =𝒞))

(module+ test
  (require rackunit)
  (require (file "../../code/category/𝐏𝐚𝐢𝐫.rkt"))

  (define-values (dom cod ∘ ? =) ((compose † 𝐏𝐚𝐢𝐫)))

  ;; Objects
  (define a '(a . a)) (check-pred ? a)
  (define b '(b . b)) (check-pred ? b)
  (define c '(c . c)) (check-pred ? c)
  (define d '(d . d)) (check-pred ? d)

  ;; Morphisms
  (define f '(a . b)) (check-pred ? f)
  (define g '(b . c)) (check-pred ? g)
  (define h '(c . d)) (check-pred ? h)


  ;; Existence of composition
  (check-true (= b (cod g) (dom f)))
  (check-true (= c (dom (∘ f g)) (dom g)))
  (check-true (= a (cod (∘ f g)) (cod f)))

  ;; Associativity of composition
  (check-true (= (∘ f g h) (∘ (∘ f g) h) (∘ f (∘ g h))))

  ;; Existence of identity morphisms
  (check-true (= a (dom a) (cod a)))

  ;; Composition and identity morphisms
  (check-true (= f (∘ f (dom f)) (∘ (cod f) f))))
