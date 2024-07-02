#lang info

(define license 'MIT)
(define collection "ctp")

(define version "0.0")

(define deps '("base" "typed-racket-lib"))
(define build-deps
  '("scribble-lib"
    "rackunit-lib"
    "racket-doc"
    "typed-racket-doc"
    "math-lib"
    "math-doc"))

(define scribblings '(("scribblings/ctp.scrbl" (multi-page) (getting-started))))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/exercises/).)*$"))
