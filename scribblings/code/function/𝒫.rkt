#lang typed/racket/base/no-check

(require racket/hash racket/set racket/promise)
(require (file "../category/𝐒𝐞𝐭.rkt"))

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
    (define (𝒫 s)
      (for/fold ([𝒫s (hash s s #hash() #hash())])
                ([(v _) (in-hash s)])
        (define s0 (hash-remove s v))
        (define 𝒫s0 (𝒫 s0))
        (hash-union 𝒫s 𝒫s0 #:combine/key combine/key)))
    (λ (s)
      (define 𝒫s.map (𝒫 (function-map s)))
      (define 𝒫s (function (lazy 𝒫s) (lazy 𝒫s) 𝒫s.map))
      𝒫s)))
