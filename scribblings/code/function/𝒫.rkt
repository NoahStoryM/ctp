#lang typed/racket/base/no-check

(require racket/hash racket/set)
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

(:  e 𝒮) (define  e (hash '_ (set)))
(: 𝒫e 𝒮) (define 𝒫e (hash-set e e e))

(define (function->mapping f) (hash-remove f '_))
(define (mapping->function m s)
  (define v*
    (let ([v1* (list->set (hash-values m))])
      (for/fold ([v* (set)])
                ([(v _) (in-hash (function->mapping s))])
        (if (set-member? v1* v) v* (set-add v* v)))))
  (define f (hash-set m '_ v*))
  f)

(: combine/key (→ Any Any Any Any))
(define (combine/key k v1 v2) (if (eq? k '_) (set-union v1 v2) v1))

(define (𝒫 s)
  (if (=𝒮 s e)
      𝒫e
      (for/fold ([𝒫s (hash-set 𝒫e s s)])
                ([(v _) (in-hash (function->mapping s))])
        (define s0 (hash-remove s v))
        (define 𝒫s0 (𝒫 s0))
        (hash-union 𝒫s 𝒫s0 #:combine/key combine/key))))
