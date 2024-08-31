#lang typed/racket/base/no-check

(require racket/match racket/function racket/set)
(require "FSM.rkt" "../../exercises/functor/FSM.rkt")
(provide ℳ 𝒢0 𝒢1 S s φ φ*)

(module+ test (require rackunit))

(: #;𝐒𝐞𝐭  𝒮 𝐂𝐚𝐭) (define (𝒮 m) m) (define ∘𝒮 compose)
(: #;𝐋𝐢𝐬𝐭 ℒ 𝐂𝐚𝐭) (define (ℒ m) m) (define ∘ℒ append)

(: ℳ (Listof (List Char Symbol Symbol)))
(define ℳ
  (append
   '([#\1 a0 s1]
     [#\1 s0 s1]
     [#\1 r0 s1]

     [#\q s1 a0]
     [#\q b1 r0]

     [#\2 a0 s2]
     [#\2 s0 s2]
     [#\2 r0 s2]

     [#\q s2 a0]
     [#\q b2 r0]
     [#\q o2 a0])
   ℳ1 ℳ2))

(define S0 (set 's0 'a0 'r0))
(define S1 (set 's1 'b1))
(define S2 (set 's2 'b2 'o2))
(define S  (set-union S0 S1 S2))

(: s 𝒢0) (define s 's0)

(define-type 𝒢0 (∪ S0 S1 S2))
(define-type 𝒢1 (List Char 𝒢0 𝒢0))

(: φ (case→ (→ 𝒢0 𝐒𝐞𝐭) (→ 𝒢1 →𝐒𝐞𝐭)))
(define (φ g)
  (match g
    [(? symbol?) (∘𝒮)]
    [`(,a ,n0 ,n1)
     #:when
     (and (char? a)
          (symbol? n0)
          (symbol? n1))
     (λ (s)
       (or
        (for/or ([i : 𝒢1 (in-list ℳ)])
          (match i
            [`(,(? (curry eq? a))
               ,(? (curry eq? s))
               ,t)
             t]
            [_ #f]))
        (raise-arguments-error
         'φ "invalid character or state"
         "character" a
         "state" s)))]))

(: φ* (∀ ([a : F𝒢] [b : F𝒢]) (→ (→F𝒢 a b) (→𝐒𝐞𝐭 (φ* a) (φ* b)))))
(define (φ* g*) (apply ∘𝒮 (map φ g*)))

(: make-path (→ (× String Symbol) (Listof 𝒢1)))
(define (make-path str s)
  (for/fold ([g* '()] [n0 s] #:result g*)
            ([a (in-list (string->list str))])
    (let/cc return
      (for ([g : 𝒢1 (in-list ℳ)])
        (match g
          [`(,(? (curry eq? a))
             ,(? (curry eq? n0))
             ,n1)
           (return (cons g g*) n1)]
          [_ (void)]))
      (raise-arguments-error
       'make-path "invalid string or state"
       "string" str
       "state" s))))

(module+ test
  (define m (make-path "1yyq"  's0))
  (define n (make-path "2xyyq" 'r0))
  (check-eq?
   ((φ* (∘ℒ n m)) s)
   ((∘𝒮 (φ* n) (φ* m)) s)))
