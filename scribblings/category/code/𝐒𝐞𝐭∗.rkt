#lang racket/base

(require racket/hash racket/set)

;; Category of Pointed Sets
(define (dom m)
  (for/hash ([(a b) (in-hash m)])
    (values a (if (eq? a '_) (set) a))))
(define (cod m)
  (hash-union
   (for/hash ([b (in-set (hash-ref m '_))]) (values b b))
   (for/hash ([(a b) (in-hash (hash-remove m '_))]) (values b b))
   (hash '_ (set))))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2)
     (define m
       (for/hash ([(k2 v2) (in-hash (hash-remove m2 '_))])
         (define v1 (hash-ref m1 v2))
         (values k2 v1)))
     (define v*
       (let ([v1* (list->set (hash-values m))])
         (for/fold ([v* (set)])
                   ([v2 (in-set (hash-ref m2 '_))])
           (define v1 (hash-ref m1 v2))
           (if (set-member? v1* v1) v* (set-add v* v1)))))
     (hash-set m '_ (set-union v* (hash-ref m1 '_)))]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (and (hash? m) (set? (hash-ref m '_ #f))))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define a (hash 'a 'a   0    0    1    1    2    2  '_ (set))) (? a)
(define b (hash 'b 'b '|0| '|0| '|1| '|1| '|2| '|2| '_ (set))) (? b)
(define c (hash 'c 'c  "0"  "0"  "1"  "1"  "2"  "2" '_ (set))) (? c)
(define d (hash 'd 'd #"0" #"0" #"1" #"1" #"2" #"2" '_ (set))) (? d)

;; Morphisms
(define f (hash 'a 'b   0  'b   1  'b   2  '|2| '_ (set '|0| '|1|))) (? f)
(define g (hash 'b 'c '|0| 'c '|1| 'c '|2|  "2" '_ (set  "0"  "1"))) (? g)
(define h (hash 'c 'd  "0" 'd  "1" 'd  "2" #"2" '_ (set #"0" #"1"))) (? h)

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
