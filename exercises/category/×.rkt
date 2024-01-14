#lang racket/base

(require "../../private/utils.rkt")

;; Product Category
(define (dom× . dom*) (define (dom m*) (map call dom* m*)) dom)
(define (cod× . cod*) (define (cod m*) (map call cod* m*)) cod)
(define (∘× . ∘*)
  (define (∘ . m**)
    (for/list ([∘ (in-list ∘*)]
               [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply ∘ m*)))
  ∘)

(define (morphism× . m*) m*)
(define (morphism×? . morphism?*)
  (define n (length morphism?*))
  (define (morphism? . m*)
    (and (list? m*) (= n (length m*))
         (andmap call morphism?* m*)))
  morphism?)
(define (morphism×=? . morphism=?*)
  (define (morphism=? . m**)
    (for/and ([morphism=? (in-list morphism=?*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply morphism=? m*)))
  morphism=?)
