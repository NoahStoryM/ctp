#lang racket/base

(define (Δ a) (cons a a))

;; Category of Pointed Sets
(define (dom m) (make-immutable-hash (map Δ (hash-keys   m))))
(define (cod m) (make-immutable-hash (map Δ (hash-values m))))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2)
     (for/hash ([(k2 v2) (in-hash m2)])
       (define v1 (hash-ref m1 v2))
       (values k2 v1))]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (and (hash? m) (hash-equal? m)))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a #hash([a . a] [  0  .   0 ] [  1  .   1 ] [  2  .   2 ]))
(define b #hash([b . b] [ |0| .  |0|] [ |1| .  |1|] [ |2| .  |2|]))
(define c #hash([c . c] [ "0" .  "0"] [ "1" .  "1"] [ "2" .  "2"]))
(define d #hash([d . d] [#"0" . #"0"] [#"1" . #"1"] [#"2" . #"2"]))

;; Morphisms
(define f #hash([a . b] [  0  .  |0|] [  1  .  |1|] [  2  .  |2|]))
(define g #hash([b . c] [ |0| .  "0"] [ |1| .  "1"] [ |2| .  "2"]))
(define h #hash([c . d] [ "0" . #"0"] [ "1" . #"1"] [ "2" . #"2"]))

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
