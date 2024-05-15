#lang racket/base

;; Category of Strings
(define (dom _) (∘))
(define (cod _) (∘))
(define (∘ . m*) (apply string-append m*))
(define (? m) (string? m))
(define (= m . m*) (apply string=? m m*))
