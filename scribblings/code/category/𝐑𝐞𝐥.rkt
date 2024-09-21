#lang racket/base

(require racket/set racket/match racket/promise)

(provide (struct-out relation))
(struct relation (dom cod rel*))

(provide 𝐑𝐞𝐥)
(define (𝐑𝐞𝐥 . _) (values dom cod ∘ ? =))

(define (dom m) (force (relation-dom m)))
(define (cod m) (force (relation-cod m)))
(define ∘
  (case-λ
    [(m) m]
    [(m1 m2)
     (define r1* (relation-rel* m1))
     (define r2* (relation-rel* m2))
     (define r*
       (set-remove
        (for*/set ([r2 (in-set r2*)]
                   [r1 (in-set r1*)])
          (match* (r2 r1)
            [(`(,a . ,b) `(,b . ,c)) `(,a . ,c)]
            [(_ _) #f]))
        #f))
     (relation (relation-dom m2) (relation-cod m1) r*)]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))
(define (? m) (relation? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2)
     (and
      (equal? (relation-rel*      m1)
              (relation-rel*      m2))
      (equal? (relation-rel* (dom m1))
              (relation-rel* (dom m2)))
      (equal? (relation-rel* (cod m1))
              (relation-rel* (cod m2))))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt")

  ;; Objects
  (define a (relation (lazy a) (lazy a) (set '(a0 . a0) '(a1 . a1) '(a2 . a2))))
  (define b (relation (lazy b) (lazy b) (set '(b0 . b0) '(b1 . b1) '(b2 . b2))))
  (define c (relation (lazy c) (lazy c) (set '(c0 . c0) '(c1 . c1) '(c2 . c2))))
  (define d (relation (lazy d) (lazy d) (set '(d0 . d0) '(d1 . d1) '(d2 . d2))))

  ;; Morphisms
  (define f (relation (lazy a) (lazy b) (set '(a0 . b0) '(a0 . b1) '(a2 . b0))))
  (define g (relation (lazy b) (lazy c) (set '(b0 . c0) '(b0 . c1) '(b2 . c0))))
  (define h (relation (lazy c) (lazy d) (set '(c0 . d0) '(c0 . d1) '(c2 . d0))))

  (define check-𝐑𝐞𝐥 (check-cat 𝐑𝐞𝐥))
  (check-𝐑𝐞𝐥 a b c d f g h))
