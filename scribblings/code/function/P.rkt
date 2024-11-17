#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require "../category/Set.rkt")

(provide (all-defined-out))

(: 𝒮 𝐂𝐚𝐭)
(: dom𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) a)))
(: cod𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) b)))
(: ∘𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] ... [z : 𝒮]) (→ (× (→𝒮 a b) (→𝒮 b c) ...) (→𝒮 a z))))
(: ?𝒮 (pred (∀ ([a : 𝒮] [b : 𝒮]) (→𝒮 a b))))
(: =𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] [d : 𝒮] ...) (→ (× (→𝒮 a b) (→𝒮 c d) ...) Boolean)))
(define (𝒮 m) m)
(define-values (dom𝒮 cod𝒮 ∘𝒮 ?𝒮 =𝒮) (𝐒𝐞𝐭))

(: combine/key (→ Any Any Any Any))
(define (combine/key k v1 v2) v1)

(define 𝒫
  (let ()
    (define (𝒫.map s.map)
      (for/fold ([𝒫s.map (hash s.map s.map #hash() #hash())])
                ([(v _) (in-hash s.map)])
        (define s0.map (hash-remove s.map v))
        (define 𝒫s0.map (𝒫.map s0.map))
        (hash-union 𝒫s.map 𝒫s0.map #:combine/key combine/key)))
    (λ (s)
      (define 𝒫s.map (𝒫.map (function-map s)))
      (define 𝒫s (function (lazy 𝒫s) (lazy 𝒫s) 𝒫s.map))
      𝒫s)))
