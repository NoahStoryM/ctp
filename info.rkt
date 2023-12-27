#lang info

(define license 'MIT)
(define version "0.0")
(define collection "ctp")
(define deps '("base"))
(define build-deps
  '("scribble-lib"
    "racket-doc"
    "math-lib"
    "math-doc"))
(define clean '("compiled"))
(define scribblings '(("scribblings/ctp.scrbl" (multi-page) (getting-started))))
