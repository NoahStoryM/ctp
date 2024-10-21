#lang typed/racket/base/no-check

(require racket/match racket/function racket/set)
(require "FSM.rkt"
         "../../exercises/functor/FSM.rkt"
         "../../exercises/functor/F.rkt"
         "../../exercises/functor/make-path.rkt")
(provide ℳ 𝒢0 𝒢1 𝒢 F𝒢 S s φ φ* ρ)

(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(define-type S0 (∪ 's0 'a0 'r0))
(define-type S  (∪ S0 S1 S2))
(define-type 𝒢0 S)
(define-type 𝒢1 (Immutable-Vector Char 𝒢0 𝒢0))

(: ℳ (Listof 𝒢1))
(define ℳ
  (append
   '(#[#\nul s0 s0]
     #[#\nul a0 a0]
     #[#\nul r0 r0]

     #[#\1 s0 s1]
     #[#\1 a0 s1]
     #[#\1 r0 s1]

     #[#\q s1 a0]
     #[#\q b1 r0]

     #[#\2 s0 s2]
     #[#\2 a0 s2]
     #[#\2 r0 s2]

     #[#\q s2 a0]
     #[#\q b2 r0]
     #[#\q o2 a0])
   ℳ1 ℳ2))

(: 𝒢 (Immutable-Vectorof 𝒢1))
(define 𝒢
  '#(#[#\nul S0 S0]
     #[#\nul S1 S1]
     #[#\nul S2 S2]

     #[#\1 S0 S1]
     #[#\2 S0 S2]

     #[#\q S1 S0]
     #[#\q S2 S0]

     #[#\x S1 S1]
     #[#\x S2 S2]

     #[#\y S1 S1]
     #[#\y S2 S2]))

(: s 𝒢0) (define s 's0)
(: φ (case→ (→ 𝒢0 𝐒𝐞𝐭) (→ 𝒢1 →𝐒𝐞𝐭)))
(define (φ g)
  (match g
    [(? symbol?) (∘𝒮)]
    [`#[,a ,n0 ,n1]
     #:when
     (and (char? a)
          (symbol? n0)
          (symbol? n1))
     (λ (s)
       (or
        (for/or ([i : 𝒢1 (in-list ℳ)])
          (match i
            [`#[,(? (curry eq? a))
                ,(? (curry eq? s))
                ,t]
             t]
            [_ #f]))
        (raise-arguments-error
         'φ "invalid character or state"
         "character" a
         "state" s)))]))

(define F𝒢 (F 𝒢))
(module+ test
  (require "../category/check.rkt")

  ;; Objects
  (define S0 (make-path 𝒢 'S0 ""))
  (define S1 (make-path 𝒢 'S1 ""))
  (define S2 (make-path 𝒢 'S2 ""))
  (define S3 (make-path 𝒢 'S3 ""))

  ;; Morphisms
  (define f (make-path 𝒢 'S0 "1yxy"))
  (define g (make-path 𝒢 'S1 "xq2y"))
  (define h (make-path 𝒢 'S2 "xxyq"))

  (define check-F𝒢 (check-cat F𝒢))
  (check-F𝒢 S0 S1 S2 S3 f g h))

(: φ* (∀ ([a : F𝒢] [b : F𝒢]) (→ (→F𝒢 a b) (→𝐒𝐞𝐭 (φ* a) (φ* b)))))
(define (φ* g*) (apply ∘𝒮 (map φ g*)))

(: ρ (→ →F𝒢 S))
(define (ρ g*) ((φ* g*) s))

(module+ test
  (check-eq? s (ρ S0))
  (check-eq? ((φ* (∘ℒ g f)) s)
             ((∘𝒮 (φ* g) (φ* f)) s)))
