#lang racket/base

(require racket/match)

(provide 𝐑𝐞𝐥)
(define (𝐑𝐞𝐥 . _) (values dom cod ∘ ? =))

(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (pair? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require rackunit)

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
  (check-true (= b (cod f) (dom g)))
  (check-true (= a (dom (∘ g f)) (dom f)))
  (check-true (= c (cod (∘ g f)) (cod g)))

  ;; Associativity of composition
  (check-true (= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f))))

  ;; Existence of identity morphisms
  (check-true (= a (dom a) (cod a)))

  ;; Composition and identity morphisms
  (check-true (= f (∘ f (dom f)) (∘ (cod f) f))))
