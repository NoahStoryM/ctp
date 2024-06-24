#lang typed/racket/base/no-check

(require racket/hash)

(: combine (→ Any Any Any))
(define combine (λ (v _) v))

(: 𝒫 (→ 𝒮 𝒮))
(define (𝒫 s)
  (for/fold ([𝒫s (hash s s)])
            ([(v _) (in-hash s)])
    (define s0 (hash-remove s v))
    (define 𝒫s0 (𝒫 s0))
    (hash-union 𝒫s 𝒫s0 #:combine combine)))

(: Δ (∀ (a) (→ a (Pair a a))))
(define (Δ a) (cons a a))

;; Category of Pointed Sets 𝒮
(: 𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 a b))))
(: dom𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) a)))
(: cod𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) b)))
(: ∘𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] ... [z : 𝒮]) (→ (× (→𝒮 a b) (→𝒮 b c) ...) (→𝒮 a z))))
(: ?𝒮 (pred (∀ ([a : 𝒮] [b : 𝒮]) (→𝒮 a b))))
(: =𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] [d : 𝒮] ...) (→ (× (→𝒮 a b) (→𝒮 c d) ...) Boolean)))
(define (𝒮 m) m)
(define (dom𝒮 m) (make-immutable-hash (map Δ (hash-keys   m))))
(define (cod𝒮 m) (make-immutable-hash (map Δ (hash-values m))))
(define ∘𝒮
  (case-λ
    [(m) m]
    [(m1 m2)
     (for/hash ([(k2 v2) (in-hash m2)])
       (define v1 (hash-ref m1 v2))
       (values k2 v1))]
    [(m1 m2 . m*) (apply ∘𝒮 (∘𝒮 m1 m2) m*)]))
(define (?𝒮 m) (hash? m))
(define =𝒮
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (=𝒮 m1 m2) (apply =𝒮 m2 m*))]))

;; Powerset Functors
(: 𝒫^∗ (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 b a) (→𝒮 (𝒫^∗ a) (𝒫^∗ b)))))
(define (𝒫^∗ f)
  (define b (dom𝒮 f))
  (define a (cod𝒮 f))
  (define 𝒫b (𝒫 b))
  (define 𝒫a (𝒫 a))
  (define f^∗
    (for/hash ([(a0 _) (in-hash 𝒫a)])
      (define b0
        (for/fold ([res #hash()])
                  ([(eb _) (in-hash b)])
          (if (and (hash-has-key? f eb)
                   (let ([ea (hash-ref f eb)])
                     (hash-has-key? a0 ea)))
              (hash-set res eb eb)
              res)))
      (values a0 b0)))
  f^∗)

(: 𝒫_∗ (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫_∗ a) (𝒫_∗ b)))))
(define (𝒫_∗ f)
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define f_∗
    (for/hash ([(a0 _) (in-hash 𝒫a)])
      (define b0
        (for/fold ([res #hash()])
                  ([(ea _) (in-hash a0)])
          (define eb (hash-ref f ea))
          (hash-set res eb eb)))
      (values a0 b0)))
  f_∗)

(: 𝒫_! (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫_! a) (𝒫_! b)))))
(define (𝒫_! f)
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define 𝒫a (𝒫 a))
  (define 𝒫b (𝒫 b))
  (define f_!
    (for/hash ([(a0 _) (in-hash 𝒫a)])
      (define b0
        (for/fold ([res #hash()])
                  ([(ea _) (in-hash a0)])
          (define eb (hash-ref f ea))
          (for/fold ([res (hash-set res eb eb)])
                    ([(ea _) (in-hash a)])
            (define eb (hash-ref f ea))
            (if (or (hash-has-key? a0 ea)
                    (not (hash-has-key? res eb)))
                res (hash-remove res eb)))))
      (values a0 b0)))
  f_!)

;; Objects
(: a 𝒮) (define a #hash([a . a] [  0  .   0 ] [  1  .   1 ] [  2  .   2 ])) (?𝒮 a)
(: b 𝒮) (define b #hash([b . b] [ |0| .  |0|] [ |1| .  |1|] [ |2| .  |2|])) (?𝒮 b)
(: c 𝒮) (define c #hash([c . c] [ "0" .  "0"] [ "1" .  "1"] [ "2" .  "2"])) (?𝒮 c)
(: d 𝒮) (define d #hash([d . d] [#"0" . #"0"] [#"1" . #"1"] [#"2" . #"2"])) (?𝒮 d)

;; Morphisms
(: f (→𝒮 a b)) (define f #hash([a . b] [  0  .  |0|] [  1  .  |1|] [  2  .  |2|])) (?𝒮 f)
(: g (→𝒮 b c)) (define g #hash([b . c] [ |0| .  "0"] [ |1| .  "1"] [ |2| .  "2"])) (?𝒮 g)
(: h (→𝒮 c d)) (define h #hash([c . d] [ "0" . #"0"] [ "1" . #"1"] [ "2" . #"2"])) (?𝒮 h)

;; Preservation of domain and codomain
(=𝒮 (𝒫^∗ b) (dom𝒮 (𝒫^∗ f)) (𝒫^∗ (cod𝒮 f)))
(=𝒮 (𝒫^∗ a) (cod𝒮 (𝒫^∗ f)) (𝒫^∗ (dom𝒮 f)))

(=𝒮 (𝒫_∗ a) (dom𝒮 (𝒫_∗ f)) (𝒫_∗ (dom𝒮 f)))
(=𝒮 (𝒫_∗ b) (cod𝒮 (𝒫_∗ f)) (𝒫_∗ (cod𝒮 f)))

(=𝒮 (𝒫_! a) (dom𝒮 (𝒫_! f)) (𝒫_! (dom𝒮 f)))
(=𝒮 (𝒫_! b) (cod𝒮 (𝒫_! f)) (𝒫_! (cod𝒮 f)))

;; Preservation of identity morphisms
(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫^∗ a) (dom𝒮 (𝒫^∗ a)) (cod𝒮 (𝒫^∗ a)))

(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫_∗ a) (dom𝒮 (𝒫_∗ a)) (cod𝒮 (𝒫_∗ a)))

(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫_! a) (dom𝒮 (𝒫_! a)) (cod𝒮 (𝒫_! a)))

;; Preservation of composable pairs
(=𝒮 (∘𝒮 (𝒫^∗ f) (𝒫^∗ g)) (𝒫^∗ (∘𝒮 g f)))

(=𝒮 (∘𝒮 (𝒫_∗ g) (𝒫_∗ f)) (𝒫_∗ (∘𝒮 g f)))

(=𝒮 (∘𝒮 (𝒫_! g) (𝒫_! f)) (𝒫_! (∘𝒮 g f)))
