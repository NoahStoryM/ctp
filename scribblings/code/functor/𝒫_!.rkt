#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require (file "𝒫^∗.rkt")
         (file "../category/𝐒𝐞𝐭.rkt")
         (file "../function/𝒫.rkt"))

(: 𝒫_! (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 a) (𝒫 b)))))
(provide 𝒫_!)
(define (𝒫_! f)
  (define m (function-map f))
  (define f^∗ (𝒫^∗ f))
  (define m^∗ (function-map f^∗))
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define f_!.map
    (for/hash ([(a0 _) (in-hash (function-map 𝒫a))])
      (define b0
        (for/fold ([b0 #hash()])
                  ([(y _) (in-hash (function-map b))])
          (define a1 (hash-ref m^∗ (hash y y)))
          (if (equal? a0 (hash-union a0 a1 #:combine/key combine/key))
              (hash-set b0 y y) b0)))
      (values a0 b0)))
  (define f_! (function (lazy 𝒫a) (lazy 𝒫b) f_!.map))
  f_!)

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
  (check-true (=𝒮 (𝒫 a) (𝒫_! a) (dom𝒮 (𝒫_! f)) (𝒫_! (dom𝒮 f))))
  (check-true (=𝒮 (𝒫 b) (𝒫_! b) (cod𝒮 (𝒫_! f)) (𝒫_! (cod𝒮 f))))

  ;; Preservation of identity morphisms
  (check-true (=𝒮      a  (dom𝒮      a)  (cod𝒮      a)))
  (check-true (=𝒮 (𝒫_! a) (dom𝒮 (𝒫_! a)) (cod𝒮 (𝒫_! a))))

  ;; Preservation of composable pairs
  (check-true (=𝒮 (∘𝒮 (𝒫_! g) (𝒫_! f)) (𝒫_! (∘𝒮 g f)))))
