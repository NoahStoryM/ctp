#lang typed/racket/base/no-check

(require racket/hash racket/set)
(require (file "../function/𝒫.rkt"))

(: 𝒫_∗ (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 a) (𝒫 b)))))
(provide 𝒫_∗)
(define (𝒫_∗ f)
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define m
    (for/hash ([(a0 _) (in-hash (function->mapping (𝒫 a)))])
      (define b0
        (for/fold ([b0 e])
                  ([(x _) (in-hash (function->mapping a0))])
          (define y (hash-ref f x))
          (hash-set b0 y y)))
      (values a0 b0)))
  (define f_∗ (mapping->function m (𝒫 b)))
  f_∗)

(module+ test
  (require rackunit)

  ;; Objects
  (: a 𝒮) (define a (hash 'x0 'x0 'x1 'x1 '_ (set))) (check-pred ?𝒮 a)
  (: b 𝒮) (define b (hash 'y0 'y0 'y1 'y1 '_ (set))) (check-pred ?𝒮 b)
  (: c 𝒮) (define c (hash 'z0 'z0 'z1 'z1 '_ (set))) (check-pred ?𝒮 c)

  ;; Morphisms
  (: f (→𝒮 a b)) (define f (hash 'x0 'y0 'x1 'y0 '_ (set 'y1))) (check-pred ?𝒮 f)
  (: g (→𝒮 b c)) (define g (hash 'y0 'z0 'y1 'z0 '_ (set 'z1))) (check-pred ?𝒮 g)

  ;; Preservation of domain and codomain
  (check-true (=𝒮 (𝒫 a) (𝒫_∗ a) (dom𝒮 (𝒫_∗ f)) (𝒫_∗ (dom𝒮 f))))
  (check-true (=𝒮 (𝒫 b) (𝒫_∗ b) (cod𝒮 (𝒫_∗ f)) (𝒫_∗ (cod𝒮 f))))

  ;; Preservation of identity morphisms
  (check-true (=𝒮      a  (dom𝒮      a)  (cod𝒮      a)))
  (check-true (=𝒮 (𝒫_∗ a) (dom𝒮 (𝒫_∗ a)) (cod𝒮 (𝒫_∗ a))))

  ;; Preservation of composable pairs
  (check-true (=𝒮 (∘𝒮 (𝒫_∗ g) (𝒫_∗ f)) (𝒫_∗ (∘𝒮 g f)))))
