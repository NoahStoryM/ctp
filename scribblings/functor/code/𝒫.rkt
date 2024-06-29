#lang typed/racket/base/no-check

(require racket/hash racket/set)

(: combine/key (→ Any Any Any Any))
(define (combine/key k v1 v2) (if (eq? k '_) (set-union v1 v2) v1))

(:  e 𝒮) (define  e (hash '_ (set)))
(: 𝒫e 𝒮) (define 𝒫e (hash-set e e e))

(: 𝒫 (→ 𝒮 𝒮))
(define (𝒫 s)
  (if (=𝒮 s e)
      𝒫e
      (for/fold ([𝒫s (hash-set 𝒫e s s)])
                ([(v _) (in-hash (function->mapping s))])
        (define s0 (hash-remove s v))
        (define 𝒫s0 (𝒫 s0))
        (hash-union 𝒫s 𝒫s0 #:combine/key combine/key))))

(define (function->mapping f) (hash-remove f '_))
(define (mapping->function m s)
  (define v*
    (let ([v1* (list->set (hash-values m))])
      (for/fold ([v* (set)])
                ([(v _) (in-hash (function->mapping s))])
        (if (set-member? v1* v) v* (set-add v* v)))))
  (define f (hash-set m '_ v*))
  f)

;; Category of Sets 𝒮
(: 𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 a b))))
(: dom𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) a)))
(: cod𝒮 (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) b)))
(: ∘𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] ... [z : 𝒮]) (→ (× (→𝒮 a b) (→𝒮 b c) ...) (→𝒮 a z))))
(: ?𝒮 (pred (∀ ([a : 𝒮] [b : 𝒮]) (→𝒮 a b))))
(: =𝒮 (∀ ([a : 𝒮] [b : 𝒮] [c : 𝒮] [d : 𝒮] ...) (→ (× (→𝒮 a b) (→𝒮 c d) ...) Boolean)))
(define (𝒮 m) m)
(define (dom𝒮 m)
  (for/hash ([(a b) (in-hash m)])
    (values a (if (eq? a '_) (set) a))))
(define (cod𝒮 m)
  (hash-union
   (for/hash ([b (in-set (hash-ref m '_))]) (values b b))
   (for/hash ([(a b) (in-hash (function->mapping m))]) (values b b))
   e))
(define ∘𝒮
  (case-λ
    [(m) m]
    [(m1 m2)
     (define m
       (for/hash ([(k2 v2) (in-hash (function->mapping m2))])
         (define v1 (hash-ref m1 v2))
         (values k2 v1)))
     (define v*
       (let ([v1* (list->set (hash-values m))])
         (for/fold ([v* (set)])
                   ([v2 (in-set (hash-ref m2 '_))])
           (define v1 (hash-ref m1 v2))
           (if (set-member? v1* v1) v* (set-add v* v1)))))
     (hash-set m '_ (set-union v* (hash-ref m1 '_)))]
    [(m1 m2 . m*) (apply ∘𝒮 (∘𝒮 m1 m2) m*)]))
(define (?𝒮 m) (and (hash? m) (set? (hash-ref m '_ #f))))
(define =𝒮
  (case-λ
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (=𝒮 m1 m2) (apply =𝒮 m2 m*))]))

;; Powerset Functors
(: 𝒫_∗ (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 a) (𝒫 b)))))
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

(: 𝒫^∗ (∀ ([b : 𝒮] [a : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 b) (𝒫 a)))))
(define (𝒫^∗ f)
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define m
    (for/hash ([(b0 _) (in-hash (function->mapping (𝒫 b)))])
      (define a0
        (for/fold ([a0 e])
                  ([(x _) (in-hash (function->mapping a))])
          (if (and (hash-has-key? f x)
                   (let ([y (hash-ref f x)])
                     (hash-has-key? b0 y)))
              (hash-set a0 x x) a0)))
      (values b0 a0)))
  (define f^∗ (mapping->function m (𝒫 a)))
  f^∗)

(: 𝒫_! (∀ ([a : 𝒮] [b : 𝒮]) (→ (→𝒮 a b) (→𝒮 (𝒫 a) (𝒫 b)))))
(define (𝒫_! f)
  (define a (dom𝒮 f))
  (define b (cod𝒮 f))
  (define f^∗ (𝒫^∗ f))
  (define m
    (for/hash ([(a0 _) (in-hash (function->mapping (𝒫 a)))])
      (define b0
        (for/fold ([b0 e])
                  ([(y _) (in-hash (function->mapping b))])
          (define a1 (hash-ref f^∗ (hash y y '_ (set))))
          (if (equal? a0 (hash-union a0 a1 #:combine/key combine/key))
              (hash-set b0 y y) b0)))
      (values a0 b0)))
  (define f_! (mapping->function m (𝒫 b)))
  f_!)

;; Objects
(: a 𝒮) (define a (hash 'x0 'x0 'x1 'x1 '_ (set))) (?𝒮 a)
(: b 𝒮) (define b (hash 'y0 'y0 'y1 'y1 '_ (set))) (?𝒮 b)
(: c 𝒮) (define c (hash 'z0 'z0 'z1 'z1 '_ (set))) (?𝒮 c)

;; Morphisms
(: f (→𝒮 a b)) (define f (hash 'x0 'y0 'x1 'y0 '_ (set 'y1))) (?𝒮 f)
(: g (→𝒮 b c)) (define g (hash 'y0 'z0 'y1 'z0 '_ (set 'z1))) (?𝒮 g)

;; Preservation of domain and codomain
(=𝒮 (𝒫 a) (𝒫_∗ a) (dom𝒮 (𝒫_∗ f)) (𝒫_∗ (dom𝒮 f)))
(=𝒮 (𝒫 b) (𝒫_∗ b) (cod𝒮 (𝒫_∗ f)) (𝒫_∗ (cod𝒮 f)))

(=𝒮 (𝒫 b) (𝒫^∗ b) (dom𝒮 (𝒫^∗ f)) (𝒫^∗ (cod𝒮 f)))
(=𝒮 (𝒫 a) (𝒫^∗ a) (cod𝒮 (𝒫^∗ f)) (𝒫^∗ (dom𝒮 f)))

(=𝒮 (𝒫 a) (𝒫_! a) (dom𝒮 (𝒫_! f)) (𝒫_! (dom𝒮 f)))
(=𝒮 (𝒫 b) (𝒫_! b) (cod𝒮 (𝒫_! f)) (𝒫_! (cod𝒮 f)))

;; Preservation of identity morphisms
(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫_∗ a) (dom𝒮 (𝒫_∗ a)) (cod𝒮 (𝒫_∗ a)))

(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫^∗ a) (dom𝒮 (𝒫^∗ a)) (cod𝒮 (𝒫^∗ a)))

(=𝒮      a  (dom𝒮      a)  (cod𝒮      a))
(=𝒮 (𝒫_! a) (dom𝒮 (𝒫_! a)) (cod𝒮 (𝒫_! a)))

;; Preservation of composable pairs
(=𝒮 (∘𝒮 (𝒫_∗ g) (𝒫_∗ f)) (𝒫_∗ (∘𝒮 g f)))

(=𝒮 (∘𝒮 (𝒫^∗ f) (𝒫^∗ g)) (𝒫^∗ (∘𝒮 g f)))

(=𝒮 (∘𝒮 (𝒫_! g) (𝒫_! f)) (𝒫_! (∘𝒮 g f)))
