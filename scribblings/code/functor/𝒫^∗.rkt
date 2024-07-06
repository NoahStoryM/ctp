#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require (file "../category/𝐒𝐞𝐭.rkt")
         (file "../function/𝒫.rkt"))

(: 𝒫^∗ (∀ ([b : 𝒮] [a : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 b) (𝒫 a)))))
(provide 𝒫^∗)
(define (𝒫^∗ f)
  (define m (function-map f))
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define ht
    (for/hash ([(b0 _) (in-hash (function-map 𝒫b))])
      (define a0
        (for/fold ([a0 #hash()])
                  ([(x _) (in-hash (function-map a))])
          (if (and (hash-has-key? m x)
                   (let ([y (hash-ref m x)])
                     (hash-has-key? b0 y)))
              (hash-set a0 x x) a0)))
      (values b0 a0)))
  (define f^∗ (function (lazy 𝒫b) (lazy 𝒫a) ht))
  f^∗)

(module+ test
  (require rackunit)

  ;; Objects
  (: a 𝒮) (define a (function (lazy a) (lazy a) #hash([x0 . x0] [x1 . x1]))) (check-pred ?𝒮 a)
  (: b 𝒮) (define b (function (lazy b) (lazy b) #hash([y0 . y0] [y1 . y1]))) (check-pred ?𝒮 b)
  (: c 𝒮) (define c (function (lazy c) (lazy c) #hash([z0 . z0] [z1 . z1]))) (check-pred ?𝒮 c)

  ;; Morphisms
  (: f (→𝒮 a b)) (define f (function (lazy a) (lazy b) #hash([x0 . y0] [x1 . y0]))) (check-pred ?𝒮 f)
  (: g (→𝒮 b c)) (define g (function (lazy b) (lazy c) #hash([y0 . z0] [y1 . z0]))) (check-pred ?𝒮 g)

  ;; Preservation of domain and codomain
  (check-true (=𝒮 (𝒫 b) (𝒫^∗ b) (dom𝒮 (𝒫^∗ f)) (𝒫^∗ (cod𝒮 f))))
  (check-true (=𝒮 (𝒫 a) (𝒫^∗ a) (cod𝒮 (𝒫^∗ f)) (𝒫^∗ (dom𝒮 f))))

  ;; Preservation of identity morphisms
  (check-true (=𝒮      a  (dom𝒮      a)  (cod𝒮      a)))
  (check-true (=𝒮 (𝒫^∗ a) (dom𝒮 (𝒫^∗ a)) (cod𝒮 (𝒫^∗ a))))

  ;; Preservation of composable pairs
  (check-true (=𝒮 (∘𝒮 (𝒫^∗ f) (𝒫^∗ g)) (𝒫^∗ (∘𝒮 g f)))))
