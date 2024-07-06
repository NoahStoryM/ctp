#lang racket/base

(require racket/hash racket/promise)

(provide (struct-out function))
(struct function (dom cod map))

(provide 𝐒𝐞𝐭)
(define (𝐒𝐞𝐭 . _) (values dom cod ∘ ? =))

(define (dom m) (force (function-dom m)))
(define (cod m) (force (function-cod m)))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2)
     (define ht1 (function-map m1))
     (define ht2 (function-map m2))
     (define ht
       (for/hash ([(k2 v2) (in-hash ht2)])
         (define v1 (hash-ref ht1 v2))
         (values k2 v1)))
     (function (function-dom m2) (function-cod m1) ht)]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (function? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2)
     (and
      (equal? (function-map      m1)
              (function-map      m2))
      (equal? (function-map (dom m1))
              (function-map (dom m2)))
      (equal? (function-map (cod m1))
              (function-map (cod m2))))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require rackunit)

  ;; Objects
  (define a (function (lazy a) (lazy a) #hash([a . a] [  0  .   0 ] [  1  .   1 ] [  2  .   2 ]))) (check-pred ? a)
  (define b (function (lazy b) (lazy b) #hash([b . b] [ |0| .  |0|] [ |1| .  |1|] [ |2| .  |2|]))) (check-pred ? b)
  (define c (function (lazy c) (lazy c) #hash([c . c] [ "0" .  "0"] [ "1" .  "1"] [ "2" .  "2"]))) (check-pred ? c)
  (define d (function (lazy d) (lazy d) #hash([d . d] [#"0" . #"0"] [#"1" . #"1"] [#"2" . #"2"]))) (check-pred ? d)

  ;; Morphisms
  (define f (function (lazy a) (lazy b) #hash([a . b] [  0  .  |0|] [  1  .  |0|] [  2  .  |1|]))) (check-pred ? f)
  (define g (function (lazy b) (lazy c) #hash([b . c] [ |0| .  "0"] [ |1| .  "0"] [ |2| .  "1"]))) (check-pred ? g)
  (define h (function (lazy c) (lazy d) #hash([c . d] [ "0" . #"0"] [ "1" . #"0"] [ "2" . #"1"]))) (check-pred ? h)


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
