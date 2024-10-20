#lang racket/base

(require racket/match racket/set)
(provide F)

(define (F 𝒢)
  (define name* (for/set ([g (in-vector 𝒢)]) (vector-ref g 0)))
  (define (name? ch) (set-member? name* ch))
  (define id?
    (match-λ
      [`#[#\nul ,n ,n] #t]
      [`#[,(? name?) ,_ ,_] #f]))

  (define dom
    (match-λ
      #;['() '()]
      [`(#[,(? name?) ,_ ,_] ... #[,(? name?) ,n0 ,n1])
       (list (vector-immutable #\nul n0 n0))]))
  (define cod
    (match-λ
      #;['() '()]
      [`(#[,(? name?) ,n0 ,n1] #[,(? name?) ,_ ,_] ...)
       (list (vector-immutable #\nul n1 n1))]))
  (define (∘ . p*) (apply append p*))
  (define (? p)
    (match p
      [`(#[,(? name?) ,_ ,_]) #t]
      [`(#[,(? name?) ,n1 ,_] #[,(? name?) ,_ ,n1] . _) (? (cdr p))]
      [_ #f]))
  (define =
    (case-λ
      [(_) #t]
      [(p1 p2) (equal? (filter id? p1) (filter id? p1))]
      [(p1 p2 . p*) (and (= p1 p2) (apply = p2 p*))]))

  (λ _ (values dom cod ∘ ? =)))
