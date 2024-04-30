#lang racket/base

;; Category of Strings
(define (dom _) (∘))
(define (cod _) (∘))
(define (∘ . m*) (apply string-append m*))

(define (morphism? m) (string? m))
(define (morphism=? m . m*) (apply string=? m m*))
