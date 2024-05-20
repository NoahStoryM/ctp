#lang racket/base

(define (kv->e kv) (cons kv kv))

;; Category of Pointed Sets
(define (dom m) (make-immutable-hash (map kv->e (hash-keys   m))))
(define (cod m) (make-immutable-hash (map kv->e (hash-values m))))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2)
     (for/hash ([(k2 v2) (in-hash m2)])
       (define v1 (hash-ref m1 v2))
       (values k2 v1))]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (and (hash? m) (hash-equal? m) (eq? '* (hash-ref m '* #f))))
(define =
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a #hash([* . *] [  0  .   0 ] [  1  .   1 ] [  2  .   2 ]))
(define b #hash([* . *] [ |0| .  |0|] [ |1| .  |1|] [ |2| .  |2|]))
(define c #hash([* . *] [ "0" .  "0"] [ "1" .  "1"] [ "2" .  "2"]))
(define d #hash([* . *] [#"0" . #"0"] [#"1" . #"1"] [#"2" . #"2"]))

;; Morphisms
(define f #hash([* . *] [  0  .  |0|] [  1  .  |1|] [  2  .  |2|]))
(define g #hash([* . *] [ |0| .  "0"] [ |1| .  "1"] [ |2| .  "2"]))
(define h #hash([* . *] [ "0" . #"0"] [ "1" . #"1"] [ "2" . #"2"]))

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
