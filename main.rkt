#lang racket/base

(require "private/utils.rkt")

(provide (all-defined-out))


;; Procedure Category
(define ∘ (procedure-rename compose '∘))
(define ·
  (let ([α->αid (λ (α) (α values))])
    (case-lambda
      [() values]
      [(α) α]
      [(α . α*)
       (define αid (apply ∘ (map α->αid α*)))
       (define composed (λ (f) (∘ (α f) αid)))
       composed])))


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
  (define (morphism? . m*)
    (andmap call morphism?* m*))
  morphism?)
(define (morphism×=? . morphism=?*)
  (define (morphism=? . m**)
    (for/and ([morphism=? (in-list morphism=?*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply morphism=? m*)))
  morphism=?)
