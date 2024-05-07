#lang racket/base

(require racket/match "private/utils.rkt"
         (file "scribblings/category/exercises/¬.rkt")
         (file "scribblings/category/exercises/×.rkt")
         (file "scribblings/category/exercises/Arr.rkt")
         (file "scribblings/category/exercises/Sli.rkt")
         (file "scribblings/category/exercises/¬Sli.rkt"))

(provide (all-defined-out)
         (all-from-out (file "scribblings/category/exercises/¬.rkt"))
         (all-from-out (file "scribblings/category/exercises/×.rkt"))
         (all-from-out (file "scribblings/category/exercises/Arr.rkt"))
         (all-from-out (file "scribblings/category/exercises/Sli.rkt"))
         (all-from-out (file "scribblings/category/exercises/¬Sli.rkt")))

;; Procedure Category
(define (dom _) (∘))
(define (cod _) (∘))
(define ∘ (procedure-rename compose '∘))
(define ? (procedure-rename procedure? '?))
(define ·
  (let ([α->αid (λ (α) (α values))])
    (case-lambda
      [() values]
      [(α) α]
      [(α . α*)
       (define αid (apply ∘ (map α->αid α*)))
       (define composed (λ (f) (∘ (α f) αid)))
       composed])))
