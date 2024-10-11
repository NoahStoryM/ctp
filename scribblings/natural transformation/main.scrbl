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
@tech{morphism} @math{α(f): F(a) → G(b)} in @math{𝒟}. This mapping must adhere
@deftech{@deftech{naturality} condition} that the following @tech{diagram} is
@tech{commutative}:

@margin-note{
In a sense, @math{α(f)} can be considered a @tech{commutative square}.
}

@image["scribblings/natural transformation/images/N-1.svg"]{[picture] N-1.svg}

The @tech{morphism} @math{α(a)} in @math{𝒟} for an @tech{object} @math{a} in
@math{𝒞} is the @deftech{component} of @math{α} at @math{a}.

To verify the properties of @tech{natural transformations}, we'll @racket[define]
some @tech{check} @tech{procedures} to automate the testing of the
@tech{naturality} a @tech{natural transformation} has:

@racketfile{code/natural transformation/check.rkt}

The following example illustrates how to implement @tech{natural transformations}
in Racket:

@racketfile{code/natural transformation/𝐒𝐞𝐭⇒𝐑𝐞𝐥.rkt}

@subsection{Composition}

In this @seclink["Composition"]{section}, we explore two types of
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
@tech{functors} and @tech{natural transformations}. For instance, @math{β∘α(f)}
are typically simplified to @math{βα(f)} or @math{βαf}. This simplification makes
it easier to reason about complex structures involving multiple @tech{functors}
and @tech{natural transformations}, reducing visual clutter and improving
readability.

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

@subsubsection{Interchange Law}

The @deftech{interchange law} (@deftech{IL}) explains how @tech{horizontal composition}
and @tech{vertical composition} of @tech{natural transformations} interact with
each other.

To understand how the @tech{IL} works, recall that applying a @tech{natural transformation}
to a @tech{morphism} often results in a @tech{morphism} which is the diagonal of
a @tech{commutative square}. When dealing with adjacent @tech{natural transformations},
there are multiple ways to @tech{compose} them, ultimately produce the same
@tech{commutative diagram}. This is precisely what the @tech{IL} states.

Consider the @tech{natural transformations} @math{α: F ⇒ G}, @math{β: G ⇒ H},
@math{γ: K ⇒ L}, and @math{δ: L ⇒ M}, where @math{F, G, H: 𝒞 → 𝒟} and
@math{K, L, M: 𝒟 → ℰ}. The @tech{commutative diagram} below illustrates the
relationships between them:

@image["scribblings/natural transformation/images/N-4.svg"]{[picture] N-4.svg}

@margin-note{
In some @tech{category theory} texts, @math{∘} denotes @tech{vertical composition}
and @math{∗} denotes @tech{horizontal composition}: @math{(δ∘γ)∗(β∘α) = (δ∗β)∘(γ∗α)}.
}

We can @tech[#:key "vertical composition"]{vertically compose}
@math{α} with @math{β}, and @math{γ} with @math{δ}, as well as
@tech[#:key "horizontal composition"]{horizontally compose} @math{α} with @math{γ},
and @math{β} with @math{δ}. The @tech{IL} states that the @tech{horizontal composition}
of two @tech{vertical compositions} is equal to the @tech{vertical composition}
of two @tech{horizontal compositions}. More precisely, the @tech{IL} can be
written as: @math{(δ·γ)∘(β·α) = (δ∘β)·(γ∘α)}.

@image["scribblings/natural transformation/images/IL.svg"]{[picture] IL.svg}

Here are some important @tech{commutative squares} that arise:

@itemlist[
  #:style 'ordered
  @item{@math{(δβ)·(γα)(f): KF(f) ⇒ MH(f)}
        @image["scribblings/natural transformation/images/N-4_0.svg"]{[picture] N-4_0.svg}}
  @item{@math{(δ·γ)(β·α)(f): KF(f) ⇒ MH(f)}
        @image["scribblings/natural transformation/images/N-4_1.svg"]{[picture] N-4_1.svg}}
  @item{@math{(δ·γ)(β·α)(f): K(β·α)(f) ⇒ M(β·α)(f)}
        @image["scribblings/natural transformation/images/N-4_2.svg"]{[picture] N-4_2.svg}}
  @item{@math{(δ·γ)(β·α)(f): (δ·γ)F(f) ⇒ (δ·γ)H(f)}
        @image["scribblings/natural transformation/images/N-4_3.svg"]{[picture] N-4_3.svg}}
  ]

@image["scribblings/natural transformation/images/N-4_4.svg"]{[picture] N-4_4.svg}

@subsection{Structure of 𝐂𝐚𝐭}

In the previous @seclink["_Functor_"]{chapter}, we introduced what we referred to
as @tech{𝐂𝐚𝐭}, which consists of @tech{categories} as @tech{objects} and
@tech{functors} as @tech{morphisms}. Strictly speaking, this was actually the
@deftech{base category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{b}}.

@image["scribblings/natural transformation/images/𝐂𝐚𝐭^b.svg"]{[picture] 𝐂𝐚𝐭^b.svg}

With the introduction of @tech{natural transformations}, we can now see that
@tech{functors} are actually a special case of @tech{natural transformations}.
This observation reveals that the full structure of @tech{𝐂𝐚𝐭} is richer and more
complex compared to other @tech{categories}, as it includes
@tech{natural transformations} in addition to @tech{functors}.

The complete @deftech{𝐂𝐚𝐭} can be understood as being composed of two additional
interrelated @tech{categories}: the @tech{horizontal category} and the
@tech{vertical category}. These @tech{categories} give us deeper insights into
the complex structure of @tech{𝐂𝐚𝐭} and how the different components
(@tech{categories}, @tech{functors}, and @tech{natural transformations}) interact
with each other.

@subsubsection{Horizontal Category}

The @deftech{horizontal category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{h}},
can be viewed as an extension of @tech{𝐂𝐚𝐭@^{b}}. In @tech{𝐂𝐚𝐭@^{b}},
@tech{objects} are @tech{categories} and @tech{morphisms} are @tech{functors}
between @tech{categories}. In @tech{𝐂𝐚𝐭@^{h}}, the @tech{objects} remain the same
but the @tech{morphisms} are generalized to include all @tech{natural transformations}
between @tech{functors}.

@image["scribblings/natural transformation/images/𝐂𝐚𝐭^h.svg"]{[picture] 𝐂𝐚𝐭^h.svg}

In @tech{𝐂𝐚𝐭@^{h}}, @tech{horizontal composition} serves as the @tech{composition}
operation for @tech{morphisms}. This perspective allows us to see that
@tech{horizontal composition} essentially works like the @tech{composition} of
@tech{functions}: both @tech{functors} and @tech{natural transformations} are
kinds of @tech{functions} between @tech{categories}.

@subsubsection{Vertical Category}

The @deftech{vertical category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{v}},
provides a perspective that focuses on the relationships between @tech{functors}
through @tech{natural transformations}. In @tech{𝐂𝐚𝐭@^{v}}, @tech{objects} are
@tech{functors} between @tech{categories} and @tech{morphisms} are
@tech{natural transformations} between @tech{functors}.

@image["scribblings/natural transformation/images/𝐂𝐚𝐭^v.svg"]{[picture] 𝐂𝐚𝐭^v.svg}

In @tech{𝐂𝐚𝐭@^{v}}, @tech{vertical composition} serves as the @tech{composition}
operation for @tech{morphisms}. This perspective helps us understand why
@tech{functors} can be viewed as a special case of @tech{natural transformations}.
Consider a @tech{category} @math{𝐂(𝒞, 𝒟)}, which has all the @tech{functors}
from @math{𝒞} to @math{𝒟} as @tech{objects}, and all the
@tech{natural transformations} between those @tech{functors} as @tech{morphisms}.
In this @tech{category}, every @tech{functor} @math{F: 𝒞 → 𝒟} can be viewed as
the @tech{identity} @tech{natural transformation} @math{id_F: F ⇒ F}, which acts
as the @tech{identity morphism}.

In Racket, to distinguish between operations in the @tech{horizontal category}
and @tech{vertical category}, we introduce the notions of @deftech{src},
@deftech{tgt}, and @deftech{·} to denote the @tech{domain}, @tech{codomain}, and
@tech{compose} operators in @math{𝐂(𝒞, 𝒟)}. Additionally, we stipulate that
@racket[(∘)] and @racket[(·)] must return the same value.

@subsection{String Diagram}

Traditional @tech{diagrams} represent @tech{categories} as @tech{nodes},
@tech{functors} as single @tech{arrows} between these @tech{nodes}, and
@tech{natural transformations} as double @tech{arrows} between these single
@tech{arrows}. In contrast, @deftech{string diagrams} provide a more intuitive
and geometrical representation:

@itemlist[
  @item{@tech{Category} is represented as a plane.}
  @item{@tech{Functor} is represented as a line segment that splits a plane.}
  @item{@tech{Natural transformation} is represented as a point that splits a line segment.}
]

@tech{String diagrams} are a powerful tool for visualizing relationships between
@tech{categories}, @tech{functors}, and @tech{natural transformations} within
@tech{𝐂𝐚𝐭}. Below, we illustrate a @tech{natural transformation} @math{α: F ⇒ G},
where @math{F, G: 𝒞 → 𝒟}, using both a traditional @tech{diagram} and a
corresponding @tech{string diagram}:

@margin-note{
By default, @tech{string diagrams} are read from right to left and from bottom
to top.
}

@image["scribblings/natural transformation/images/α.svg"]{[picture] α.svg}

We also use special notations in @tech{string diagrams}:

@itemlist[
  @item{The @tech{identity} @tech{functor} is represented by a dashed line or may
        be omitted entirely for simplicity.}
  @item{Curved lines or arcs can also be used to represent @tech{functors}, and
        their intersection represents a @tech{natural transformation}.}
  ]

Below is a traditional @tech{diagram} representing a @tech{natural transformation}
@math{α: G∘F ⇒ id@_{𝒞}}, where @math{F: 𝒞 → 𝒟} and @math{G: 𝒟 → 𝒞}, along with
its @tech{equivalent} representations using @tech{string diagrams}:

@image["scribblings/natural transformation/images/α_0.svg"]{[picture] α_0.svg}

The following @tech{string diagrams} show different ways of representing the same
structure:

@image["scribblings/natural transformation/images/α_1.svg"]{[picture] α_1.svg}
@image["scribblings/natural transformation/images/α_2.svg"]{[picture] α_2.svg}
@image["scribblings/natural transformation/images/α_3.svg"]{[picture] α_3.svg}

The advantage of using @tech{string diagrams} lies in their simplicity when
representing complex structures in @tech{𝐂𝐚𝐭}. Instead of working with layers of
@tech{nodes} and @tech{arrows}, @tech{string diagrams} allow us to represent
these relationships in a clear, visual manner that highlights how each part of
the structure interacts with the others.

@bold{Exercise}: The following is a @tech{string diagram}, try to draw the
corresponding @tech{diagram}.

@image["scribblings/natural transformation/images/str-diag.svg"]{[picture] str-diag.svg}

If there are no @tech{natural transformations} in a @tech{string diagram}, we can
further compress it for simplicity. Specifically:

@itemlist[
  @item{@tech{Category} is represented as a line segment.}
  @item{@tech{Functor} is represented as a point that splits a line segment.}
]

This compressed representation is not limited to @tech{𝐂𝐚𝐭} but can also be
extended to other @tech{categories}:

@itemlist[
  @item{@tech{0-cell} is represented as a line segment.}
  @item{@tech{1-cell} is represented as a point that splits a line segment.}
]
