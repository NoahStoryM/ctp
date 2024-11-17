#lang racket/base

(provide (struct-out composition))
(struct composition (procedure* body)
  #:constructor-name make-composition
  #:property prop:procedure
  (struct-field-index body))

(provide 𝐏𝐫𝐨𝐜)
(define (𝐏𝐫𝐨𝐜 . _) (values dom cod ∘ ? =))

(define (dom _) values)
(define (cod _) values)
(define ∘
  (case-λ
    [() values]
    [(m) m]
    [m*
     (define procedure**
       (for/list ([m (in-list m*)])
         (cond
           [(eq? values m) '()]
           [(composition? m) (composition-procedure* m)]
           [else (list m)])))
     (define procedure* (apply append procedure**))
     (define body (apply compose procedure*))
     (case (length procedure*)
       [(0 1) body]
       [else (make-composition procedure* body)])]))
(define (? m) (procedure? m))
(define =
  (case-λ
    [(_) #t]
    [(m1 m2)
     (or (eq? m1 m2)
         (and (composition? m1)
              (composition? m2)
              (equal? (composition-procedure* m1)
                      (composition-procedure* m2))))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

(module+ test
  (require "check.rkt")

  ;; morphism
  (define f number->string)
  (define g string->list)
  (define h list->vector)

  (define check-𝐏𝐫𝐨𝐜 (check-ooc 𝐏𝐫𝐨𝐜))
  (check-𝐏𝐫𝐨𝐜 f g h))
