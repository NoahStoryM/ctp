#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0 (except-in typed/racket/base/no-check =))
                     rackunit)
          "../ctp-utils.rkt")

@title[#:tag "_Natural_Transformation_"]{Natural Transformation}

In this @seclink["_Natural_Transformation_"]{chapter}, we extend our exploration
of @tech{category theory} by introducing the concept of @tech{natural transformation}.
@tech{Natural transformations} provide a structured way to understand how
@tech{functors} between two @tech{categories} relate to each other.

Building on our foundation of @tech{categories} and @tech{functors}, this
@seclink["_Natural_Transformation_"]{chapter} presents @tech{natural transformations}
in a unique way: they are defined as @tech{functions} that map @tech{morphisms}
in the @tech{domain} @tech{category} to corresponding @tech{morphisms} in the
@tech{codomain} @tech{category}, similar to @tech{functors}, while ensuring
certain @tech{commutative} properties hold. This approach highlights that
@tech{functors} themselves can be viewed as special @tech{natural transformations},
much like @tech{objects} can be viewed as special @tech{morphisms}, specifically
as @tech{identity morphisms}.

As in the previous @seclink["_Functor_"]{chapter}, we'll leverage Typed Racket
to illustrate the core principles, allowing us to express these abstract
mathematical concepts through practical programming constructs.

@local-table-of-contents[]

@section{Natural Transformation}

A @deftech{natural transformation} @math{α} between @tech{parallel}
@tech{functors} @math{F, G: 𝒞 → 𝒟}, denoted as @math{α: F @deftech{⇒} G}, is a
way to map each @tech{morphism} @math{f: a → b} in @math{𝒞} to a corresponding
@tech{morphism} @math{α(f): F(a) → G(b)} in @math{𝒟}. This mapping must ensure
that the following @tech{diagram} is @tech{commutative}:

@image["scribblings/natural transformation/images/N-1.svg"]{[picture] N-1.svg}

To verify the properties of @tech{natural transformations}, we'll @racket[define]
some @tech{check} @tech{procedures} to automate the testing of the
@deftech{naturality} a @tech{natural transformation} has:

@racketfile{code/natural transformation/check.rkt}

Just as @tech{functors} can be @tech[#:key "compose"]{composed}, so can
@tech{natural transformations}. In fact, a key insight is that @tech{functors}
themselves can be viewed as special types of @tech{natural transformations}.
Given that @tech{natural transformations} are defined as mappings of
@tech{morphisms}, it is natural to consider whether and how they can be
@tech[#:key "compose"]{composed}, similar to the @tech{composition} of
@tech{functions} or @tech{functors}. This leads us to @racket[define] a type of
@tech{composition} for @tech{natural transformations}, known as
@tech{horizontal composition}.

Consider two @tech{natural transformations} @math{α: F ⇒ G} and @math{β: H ⇒ K},
where @math{F, G: 𝒞 → 𝒟} and @math{H, K: 𝒟 → ℰ}. The
@deftech{horizontal composition} @math{β∘α: H∘F ⇒ K∘G} is a new
@tech{natural transformation} that, for each @tech{morphism} @math{f: a → b} in
@math{𝒞}, maps it to @math{β∘α(f) = β(α(f)): H∘F(a) → K∘G(b)} in @math{ℰ}.

@bold{Exercise}: Prove @math{α = α∘id@_{𝒞} = id@_{𝒟}∘α}.

@bold{Exercise}: Prove that @tech{horizontal composition} is @tech{associative}.

We often omit the @tech{composition} symbol @tech{∘} when dealing with
@tech{functors} and @tech{natural transformations}. For instance, expressions
like @math{K∘F}, @math{β∘F}, @math{K∘α}, and @math{β∘α} are typically simplified
to @math{KF}, @math{βF}, @math{Kα}, and @math{βα}. This simplification makes it
easier to reason about complex structures involving multiple @tech{functors} and
@tech{natural transformations}, reducing visual clutter and improving readability.

@image["scribblings/natural transformation/images/N-2.svg"]{[picture] N-2.svg}

@bold{Exercise}: Prove that the @tech{horizontal composition} of
@tech{natural transformations} ensures that the resulting @tech{diagram} is
@tech{commutative}.

@;; @image["scribblings/natural transformation/images/N-3.svg"]{[picture] N-3.svg}
