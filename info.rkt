#lang info

(define license 'MIT)
(define version "0.0")
(define collection "ctp")
(define deps '("base" "typed-racket-lib"))
(define build-deps
  '("scribble-lib"
    "racket-doc"
    "typed-racket-doc"
    "math-lib"
    "math-doc"))
(define clean '("compiled" "private/compiled"))
(define scribblings '(("scribblings/ctp.scrbl" (multi-page) (getting-started))))
