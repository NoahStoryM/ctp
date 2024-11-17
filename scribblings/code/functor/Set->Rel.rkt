#lang typed/racket/base/no-check

(require "../category/Set.rkt"
         "../category/Rel.rkt")

(: 𝒮 𝐂𝐚𝐭)
(: dom𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) a)))
(: cod𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) b)))
(: ∘𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] ... [z : 𝒮]) (→ (× (→𝒮 a b) (→𝒮 b c) ...) (→𝒮 a z))))
(: ?𝒮 (pred (∀ ([a : 𝒮] [b : 𝒮]) (→𝒮 a b))))
(: =𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] [d : 𝒮] ...) (→ (× (→𝒮 a b) (→𝒮 c d) ...) Boolean)))
(define (𝒮 m) m)
(define-values (dom𝒮 cod𝒮 ∘𝒮 ?𝒮 =𝒮) (𝐒𝐞𝐭))

(: ℛ 𝐂𝐚𝐭)
(: domℛ (∀ ([a : ℛ] [b : ℛ]) (→ (→ℛ a b) a)))
(: codℛ (∀ ([a : ℛ] [b : ℛ]) (→ (→ℛ a b) b)))
(: ∘ℛ (∀ ([a : ℛ] [b : ℛ] [c : ℛ] ... [z : ℛ]) (→ (× (→ℛ a b) (→ℛ b c) ...) (→ℛ a z))))
(: ?ℛ (pred (∀ ([a : ℛ] [b : ℛ]) (→ℛ a b))))
(: =ℛ (∀ ([a : ℛ] [b : ℛ] [c : ℛ] [d : ℛ] ...) (→ (× (→ℛ a b) (→ℛ c d) ...) Boolean)))
(define (ℛ m) m)
(define-values (domℛ codℛ ∘ℛ ?ℛ =ℛ) (𝐑𝐞𝐥))

;; Functors from 𝒮 to ℛ
(: F (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→ℛ (F a) (F b)))))
(define F
  (let ()
    (define (F.map m) (for/set ([(x y) (in-hash m)]) (cons x y)))
    (λ (f)
      (define a (dom𝒮 f))
      (define b (cod𝒮 f))
      (define a.map (function-map a))
      (define b.map (function-map b))
      (define f.map (function-map f))
      (define Fa (relation (lazy Fa) (lazy Fa) (F.map a.map)))
      (define Fb (relation (lazy Fb) (lazy Fb) (F.map b.map)))
      (define Ff (relation (lazy Fa) (lazy Fb) (F.map f.map)))
      Ff)))

(module+ test
  (require "check.rkt")

  ;; Objects in 𝒮
  (: a 𝒮) (define a (function (lazy a) (lazy a) #hash([x0 . x0] [x1 . x1])))
  (: b 𝒮) (define b (function (lazy b) (lazy b) #hash([y0 . y0] [y1 . y1])))
  (: c 𝒮) (define c (function (lazy c) (lazy c) #hash([z0 . z0] [z1 . z1])))

  ;; Morphisms in 𝒮
  (: f (→𝒮 a b)) (define f (function (lazy a) (lazy b) #hash([x0 . y0] [x1 . y0])))
  (: g (→𝒮 b c)) (define g (function (lazy b) (lazy c) #hash([y0 . z0] [y1 . z0])))

  (define check-𝐒𝐞𝐭→𝐑𝐞𝐥 (check-ftr 𝐒𝐞𝐭 𝐑𝐞𝐥))
  (define check-F (check-𝐒𝐞𝐭→𝐑𝐞𝐥 F))
  (check-F a b c f g))
