#lang info

(define license 'MIT)
(define collection "ctp")
(define version "0.2")

(define pkg-desc "Category Theory in Programming")

(define deps '("base" "typed-racket-lib"))
(define build-deps
  '("scribble-lib"
    "rackunit-lib"
    "rackunit-doc"
    "racket-doc"
    "typed-racket-doc"
    "math-lib"
    "math-doc"))

(define scribblings '(("scribblings/ctp.scrbl" (multi-page) (teaching -30))))

(define clean '("compiled" "private/compiled"))
(define test-omit-paths '(#px"^((?!/code/|/exercises/).)*$"))
