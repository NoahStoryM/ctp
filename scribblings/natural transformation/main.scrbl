#lang scribble/manual

@(require (for-label ctp (only-meta-in 0 (except-in typed/racket/base/no-check =)))
          "../ctp-utils.rkt")

@title[#:tag "_Natural_Transformation_"]{Natural Transformation}

In this @seclink["_Natural_Transformation_"]{chapter}, we extend our exploration
of @tech{category theory} by introducing the concept of @tech{natural transformation}.
@tech{Natural transformations} provide a structured way to understand how
@tech{functors} between two @tech{categories} relate to each other.

Building on our foundation of @tech{categories} and @tech{functors}, this
@seclink["_Natural_Transformation_"]{chapter} presents @tech{natural transformations}
in a unique way: they are defined as @tech{functions} that map @tech{morphisms}
in the @tech{domain category} to corresponding @tech{morphisms} in the
@tech{codomain category}, similar to @tech{functors}, while ensuring certain
@tech{commutative} properties hold. This approach highlights that @tech{functors}
themselves can be viewed as special @tech{natural transformations}, much like
@tech{objects} can be viewed as special @tech{morphisms}, specifically as
@tech{identity morphisms}.

As in the previous @seclink["_Functor_"]{chapter}, we'll leverage Typed Racket
to illustrate the core principles, allowing us to express these abstract
mathematical concepts through practical programming constructs.

@local-table-of-contents[]

@section{Natural Transformation}
