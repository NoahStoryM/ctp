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
     (define m1.map (function-map m1))
     (define m2.map (function-map m2))
     (define m.map
       (for/hash ([(k2 v2) (in-hash m2.map)])
         (define v1 (hash-ref m1.map v2))
         (values k2 v1)))
     (define m (function (function-dom m2) (function-cod m1) m.map))
     m]
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
  (require "check.rkt")

  ;; Objects
  (define a (function (lazy a) (lazy a) #hash([a . a] [  0  .   0 ] [  1  .   1 ])))
  (define b (function (lazy b) (lazy b) #hash([b . b] [ |0| .  |0|] [ |1| .  |1|])))
  (define c (function (lazy c) (lazy c) #hash([c . c] [ "0" .  "0"] [ "1" .  "1"])))
  (define d (function (lazy d) (lazy d) #hash([d . d] [#"0" . #"0"] [#"1" . #"1"])))

  ;; Morphisms
  (define f (function (lazy a) (lazy b) #hash([a . b] [  0  .  |0|] [  1  .  |0|])))
  (define g (function (lazy b) (lazy c) #hash([b . c] [ |0| .  "0"] [ |1| .  "0"])))
  (define h (function (lazy c) (lazy d) #hash([c . d] [ "0" . #"0"] [ "1" . #"0"])))

  (define check-𝐒𝐞𝐭 (check-cat 𝐒𝐞𝐭))
  (check-𝐒𝐞𝐭 a b c d f g h))
