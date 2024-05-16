#lang racket/base

(struct composition (procedure* body)
  #:constructor-name make-composition
  #:property prop:procedure
  (struct-field-index body))

;; Category of Procedures
(define (dom _) *)
(define (cod _) *)
(define ∘
  (case-lambda
    [() *]
    [(m) m]
    [m*
     (define procedure**
       (for/list ([m (in-list m*)])
         (cond
           [(eq? * m) '()]
           [(composition? m) (composition-procedure* m)]
           [else (list m)])))
     (define procedure* (apply append procedure**))
     (define body (apply compose procedure*))
     (case (length procedure*)
       [(0 1) body]
       [else (make-composition procedure* body)])]))
(define (? m) (procedure? m))
(define =
  (case-lambda
    [(_) #t]
    [(m1 m2)
     (or (eq? m1 m2)
         (and (composition? m1)
              (composition? m2)
              (equal? (composition-procedure* m1)
                      (composition-procedure* m2))))]
    [(m1 m2 . m*) (and (= m1 m2) (apply = m2 m*))]))

;; Objects
(define * values)

;; Morphisms
(define f +)
(define g list)
(define h string)

;; Existence of composition
(= * (cod f) (dom g))
(= * (dom (∘ g f)) (dom f))
(= * (cod (∘ g f)) (cod g))

;; Associativity of composition
(= (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

;; Existence of identity morphisms
(= * (dom *) (cod *))

;; Composition and identity morphisms
(= f (∘ f (dom f)) (∘ (cod f) f))
