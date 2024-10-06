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

@margin-note{
In a sense, @math{α(f)} can be considered a @tech{commutative square}.
}

@image["scribblings/natural transformation/images/N-1.svg"]{[picture] N-1.svg}

To verify the properties of @tech{natural transformations}, we'll @racket[define]
some @tech{check} @tech{procedures} to automate the testing of the
@deftech{naturality} a @tech{natural transformation} has:

@racketfile{code/natural transformation/check.rkt}

@subsection{Godement Calculus}

In this @seclink["Godement_Calculus"]{section}, we explore two types of
@tech{composition} for @tech{natural transformations}: @tech{horizontal composition}
and @tech{vertical composition}. These forms of @tech{composition} are fundamental
to understanding how @tech{natural transformations} interact and provide a deeper
insight into their algebraic properties.

@subsubsection{Horizontal Composition}

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

@bold{Exercise}: Show the types of @math{H∘α}, @math{K∘α}, @math{β∘F}, and @math{β∘G}.

We often omit the @tech{composition} symbol @tech{∘} when dealing with
@tech{functors} and @tech{natural transformations}. For instance, expressions
like @math{K∘F}, @math{K∘α}, @math{β∘F}, and @math{β∘α} are typically simplified
to @math{KF}, @math{Kα}, @math{βF}, and @math{βα}. This simplification makes it
easier to reason about complex structures involving multiple @tech{functors} and
@tech{natural transformations}, reducing visual clutter and improving readability.

@image["scribblings/natural transformation/images/N-2.svg"]{[picture] N-2.svg}

@bold{Exercise}: Prove that the @tech{horizontal composition} of
@tech{natural transformations} ensures that the resulting @tech{diagram} is
@tech{commutative}.

The @tech{horizontal composition} involves more than just combining
@tech{natural transformations} at a high level; it reveals the structure within
the @tech[#:key "compose"]{composed} @tech{natural transformation}. For @math{βα},
its type is @math{HF ⇒ KG}, where @math{HF} and @math{KG} are @tech{functors}
from @math{𝒞} to @math{ℰ}. Additionally, there are three important
@tech{commutative squares} associated with @math{βα}:

@itemlist[
  #:style 'ordered
  @item{@math{βα(f): HF(f) ⇒ KG(f)} @image["scribblings/natural transformation/images/N-2_1.svg"]{[picture] N-2_1.svg}}
  @item{@math{βα(f): Hα(f) ⇒ Kα(f)} @image["scribblings/natural transformation/images/N-2_2.svg"]{[picture] N-2_2.svg}}
  @item{@math{βα(f): βF(f) ⇒ βG(f)} @image["scribblings/natural transformation/images/N-2_3.svg"]{[picture] N-2_3.svg}}
  ]

Note that @math{Hα}, @math{Kα}, @math{βF}, and @math{βG} are all
@tech{natural transformations}, not @tech{functors}. These three
@tech{commutative squares} arise naturally because @math{βα(f)} is the body
diagonal of a @tech{commutative cube}, and there are precisely three faces of
this cube whose face diagonals coincide with this body diagonal. Each of these
faces also has a cube edge that starts from @math{HF(a)}, which uniquely
identifies them among all the cube's faces:

@image["scribblings/natural transformation/images/N-2_4.svg"]{[picture] N-2_4.svg}

@bold{Exercise}: Prove that @tech{horizontal composition} is @tech{associative}.

@subsubsection{Vertical Composition}

Since two adjacent @tech{commutative squares} can themselves be
@tech[#:key "compose"]{composed} to form a larger @tech{commutative square},
it naturally suggests that two @tech{natural transformations} that share a common
@tech{functor} can also be @tech[#:key "compose"]{composed} to form a new
@tech{natural transformation}. This leads us to @racket[define] a type of
@tech{composition} for @tech{natural transformations}, known as
@tech{vertical composition}.

Consider two @tech{natural transformations} @math{α: F ⇒ G} and @math{β: G ⇒ H},
where @math{F, G, H: 𝒞 → 𝒟}. The @deftech{vertical composition} @math{β·α: F ⇒ H},
is a new natural transformation that, for each @tech{morphism} @math{f: a → b}
in @math{𝒞}, maps it to @math{β·α(f) : F(a) → H(b)} in @math{𝒟}.

@bold{Exercise}: Prove @math{α = α·F = G·α}.

@image["scribblings/natural transformation/images/N-3.svg"]{[picture] N-3.svg}
@image["scribblings/natural transformation/images/N-3_1.svg"]{[picture] N-3_1.svg}
@image["scribblings/natural transformation/images/N-3_2.svg"]{[picture] N-3_2.svg}

@bold{Exercise}: Prove that the @tech{vertical composition} of
@tech{natural transformations} ensures that the resulting @tech{diagram} is
@tech{commutative}.

@bold{Exercise}: Prove that @tech{vertical composition} is @tech{associative}.

@subsubsection{Godements's Five Rules}

@deftech{Godements's five rules} provide powerful tools for reasoning about the
relationships between @tech{functors} and the @tech{natural transformations}
between them. By organizing @tech{natural transformations} into structures that
can themselves be @tech[#:key "compose"]{composed}, we reveal a higher level of
abstraction within @tech{category theory}.

@deftech{Interchange Law}: @math{(δ·γ)∘(β·α) = (δ∘β)·(γ∘α)}.

@image["scribblings/natural transformation/images/N-4.svg"]{[picture] N-4.svg}

@itemlist[
  #:style 'ordered
  @item{@math{(δ·γ)(β·α)(f): KF(f) ⇒ MH(f)} @image["scribblings/natural transformation/images/N-4_1.svg"]{[picture] N-4_1.svg}}
  @item{@math{(δ·γ)(β·α)(f): K(β·α)(f) ⇒ M(β·α)(f)} @image["scribblings/natural transformation/images/N-4_2.svg"]{[picture] N-4_2.svg}}
  @item{@math{(δ·γ)(β·α)(f): (δ·γ)F(f) ⇒ (δ·γ)H(f)} @image["scribblings/natural transformation/images/N-4_3.svg"]{[picture] N-4_3.svg}}
  ]

@image["scribblings/natural transformation/images/N-4_4.svg"]{[picture] N-4_4.svg}

@math{(δβ)·(γα)(f): KF(f) ⇒ MH(f)} @image["scribblings/natural transformation/images/N-4_5.svg"]{[picture] N-4_5.svg}
