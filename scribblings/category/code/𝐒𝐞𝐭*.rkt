#lang racket/base

;; Category of Pointed Sets
(define (dom m) (make-immutable-hashalw (map (λ (k) (cons k k)) (hash-keys   m))))
(define (cod m) (make-immutable-hashalw (map (λ (v) (cons v v)) (hash-values m))))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2)
     (for/hashalw ([(k2 v2) (in-hash m2)])
       (define v1 (hash-ref m1 v2))
       (values k2 v1))]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (and (hash-equal-always? m) (eq? '* (hash-ref m '* #f))))
(define =
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal-always? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a #hashalw([* . *] [  0  .   0 ] [  1  .   1 ] [  2  .   2 ]))
(define b #hashalw([* . *] [ |0| .  |0|] [ |1| .  |1|] [ |2| .  |2|]))
(define c #hashalw([* . *] [ "0" .  "0"] [ "1" .  "1"] [ "2" .  "2"]))
(define d #hashalw([* . *] [#"0" . #"0"] [#"1" . #"1"] [#"2" . #"2"]))

;; Morphisms
(define f #hashalw([* . *] [  0  .  |0|] [  1  .  |1|] [  2  .  |2|]))
(define g #hashalw([* . *] [ |0| .  "0"] [ |1| .  "1"] [ |2| .  "2"]))
(define h #hashalw([* . *] [ "0" . #"0"] [ "1" . #"1"] [ "2" . #"2"]))

;; Existence of composition
(= b (cod f) (dom g))
(= a (dom (∘ g f)) (dom f))
(= c (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= a (dom a) (cod a))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
