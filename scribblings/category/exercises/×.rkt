#lang racket/base

(require "../../../private/utils.rkt")

;; Product Category
(define (× . m*) m*)
(define (dom× . dom*) (define (dom m*) (map call dom* m*)) dom)
(define (cod× . cod*) (define (cod m*) (map call cod* m*)) cod)
(define (∘× . ∘*)
  (define (∘ . m**)
    (for/list ([∘ (in-list ∘*)]
               [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply ∘ m*)))
  ∘)
(define (?× . ?*)
  (define n (length ?*))
  (define (? . m*)
    (and (list? m*) (= n (length m*))
         (andmap call ?* m*)))
  ?)
(define (=× . =*)
  (define (= . m**)
    (for/and ([= (in-list =*)]
              [i (in-naturals)])
      (define m* (map (λ (m*) (list-ref m* i)) m**))
      (apply = m*)))
  =)
