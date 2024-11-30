#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require "../category/Set.rkt"
         "../function/P.rkt")

(: 𝒫^∗ (∀ ([b : 𝒮] [a : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 b) (𝒫 a)))))
(provide 𝒫^∗)
(define (𝒫^∗ f)
  (define f.map (function-map f))
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define f^∗.map
    (for/hash ([(b0.map _) (in-hash (function-map 𝒫b))])
      (define a0.map
        (for/fold ([a0.map #hash()])
                  ([(x _) (in-hash (function-map a))])
          (if (and (hash-has-key? f.map x)
                   (let ([y (hash-ref f.map x)])
                     (hash-has-key? b0.map y)))
              (hash-set a0.map x x) a0.map)))
      (values b0.map a0.map)))
  (define f^∗ (function (lazy 𝒫b) (lazy 𝒫a) f^∗.map))
  f^∗)

(module+ test
  (require "check.rkt" "../category/dual.rkt")
  (define ∘ compose)

  ;; Objects
  (: a 𝒮) (define a (function (lazy a) (lazy a) #hash([x0 . x0] [x1 . x1])))
  (: b 𝒮) (define b (function (lazy b) (lazy b) #hash([y0 . y0] [y1 . y1])))
  (: c 𝒮) (define c (function (lazy c) (lazy c) #hash([z0 . z0] [z1 . z1])))

  ;; Morphisms
  (: f (→𝒮 a b)) (define f (function (lazy a) (lazy b) #hash([x0 . y0] [x1 . y0])))
  (: g (→𝒮 b c)) (define g (function (lazy b) (lazy c) #hash([y0 . z0] [y1 . z0])))

  (define check-𝐒𝐞𝐭†→𝐒𝐞𝐭 (check-ftr (∘ † 𝐒𝐞𝐭) 𝐒𝐞𝐭))
  (define check-𝒫^∗ (check-𝐒𝐞𝐭†→𝐒𝐞𝐭 𝒫^∗))
  (check-𝒫^∗ c b a g f))
