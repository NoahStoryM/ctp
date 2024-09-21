#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require (file "../category/𝐒𝐞𝐭.rkt")
         (file "../function/𝒫.rkt"))

(: 𝒫_∗ (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 a) (𝒫 b)))))
(provide 𝒫_∗)
(define (𝒫_∗ f)
  (define f.map (function-map f))
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define f_∗.map
    (for/hash ([(a0.map _) (in-hash (function-map 𝒫a))])
      (define b0.map
        (for/fold ([b0.map #hash()])
                  ([(x _) (in-hash a0.map)])
          (define y (hash-ref f.map x))
          (hash-set b0.map y y)))
      (values a0.map b0.map)))
  (define f_∗ (function (lazy 𝒫a) (lazy 𝒫b) f_∗.map))
  f_∗)

(module+ test
  (require "check.rkt")

  ;; Objects
  (: a 𝒮) (define a (function (lazy a) (lazy a) #hash([x0 . x0] [x1 . x1])))
  (: b 𝒮) (define b (function (lazy b) (lazy b) #hash([y0 . y0] [y1 . y1])))
  (: c 𝒮) (define c (function (lazy c) (lazy c) #hash([z0 . z0] [z1 . z1])))

  ;; Morphisms
  (: f (→𝒮 a b)) (define f (function (lazy a) (lazy b) #hash([x0 . y0] [x1 . y0])))
  (: g (→𝒮 b c)) (define g (function (lazy b) (lazy c) #hash([y0 . z0] [y1 . z0])))

  (define check-𝐒𝐞𝐭→𝐒𝐞𝐭 (check-ftr 𝐒𝐞𝐭 𝐒𝐞𝐭))
  (define check-𝒫_∗ (check-𝐒𝐞𝐭→𝐒𝐞𝐭 𝒫_∗))
  (check-𝒫_∗ a b c f g))
