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
@tech{functors} @math{F} and @math{G} from @math{𝒞} to @math{𝒟}, denoted by
@math{α : F @deftech{⇒} G : 𝒞 → 𝒟}, is a way to map each @tech{morphism}
@math{f : a → b : 𝒞} to a corresponding @tech{morphism}
@math{α(f) : F(a) → G(b) : 𝒟}. This mapping must adhere
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

Consider two @tech{natural transformations} @math{α : F ⇒ G : 𝒞 → 𝒟} and
@math{β : H ⇒ K : 𝒟 → ℰ}. The @deftech{horizontal composition}
@math{β∘α : H∘F ⇒ K∘G : 𝒞 → ℰ} is a new @tech{natural transformation} that, for
each @tech{morphism} @math{f : a → b : 𝒞}, maps it to
@math{β∘α(f) = β(α(f)) : H∘F(a) → K∘G(b) : ℰ}.

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
  @item{@math{βα(f) : HF(f) ⇒ KG(f)} @image["scribblings/natural transformation/images/N-2_1.svg"]{[picture] N-2_1.svg}}
  @item{@math{βα(f) : Hα(f) ⇒ Kα(f)} @image["scribblings/natural transformation/images/N-2_2.svg"]{[picture] N-2_2.svg}}
  @item{@math{βα(f) : βF(f) ⇒ βG(f)} @image["scribblings/natural transformation/images/N-2_3.svg"]{[picture] N-2_3.svg}}
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

Consider two @tech{natural transformations} @math{α : F ⇒ G : 𝒞 → 𝒟} and
@math{β : G ⇒ H : 𝒞 → 𝒟}. The @deftech{vertical composition}
@math{β∙α : F ⇒ H : 𝒞 → 𝒟} is a new @tech{natural transformation} that, for each
@tech{morphism} @math{f : a → b : 𝒞}, maps it to @math{β∙α(f) : F(a) → H(b) : 𝒟}.

@bold{Exercise}: Prove @math{α = α∙F = G∙α}.

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

Consider the @tech{natural transformations} @math{α : F ⇒ G : 𝒞 → 𝒟},
@math{β : G ⇒ H : 𝒞 → 𝒟}, @math{γ : K ⇒ L : 𝒟 → ℰ}, and @math{δ : L ⇒ M : 𝒟 → ℰ}.
The @tech{commutative diagram} below illustrates the relationships between them:

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
written as: @math{(δ∙γ)∘(β∙α) = (δ∘β)∙(γ∘α)}.

@image["scribblings/natural transformation/images/IL.svg"]{[picture] IL.svg}

Here are some important @tech{commutative squares} that arise:

@itemlist[
  #:style 'ordered
  @item{@math{(δβ)∙(γα)(f) : KF(f) ⇒ MH(f)}
        @image["scribblings/natural transformation/images/N-4_0.svg"]{[picture] N-4_0.svg}}
  @item{@math{(δ∙γ)(β∙α)(f) : KF(f) ⇒ MH(f)}
        @image["scribblings/natural transformation/images/N-4_1.svg"]{[picture] N-4_1.svg}}
  @item{@math{(δ∙γ)(β∙α)(f) : K(β∙α)(f) ⇒ M(β∙α)(f)}
        @image["scribblings/natural transformation/images/N-4_2.svg"]{[picture] N-4_2.svg}}
  @item{@math{(δ∙γ)(β∙α)(f) : (δ∙γ)F(f) ⇒ (δ∙γ)H(f)}
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
Consider a @deftech{functor category} @math{[𝒞, 𝒟]}
(@deftech{exponential category} @math{𝒟@^{𝒞}}), which has all the @tech{functors}
from @math{𝒞} to @math{𝒟} as @tech{objects}, and all the
@tech{natural transformations} between those @tech{functors} as @tech{morphisms}.
In this @tech{category}, every @tech{functor} @math{F : 𝒞 → 𝒟} can be viewed as
the @tech{identity} @tech{natural transformation} @math{id_F : F ⇒ F}, which acts
as the @tech{identity morphism}.

@bold{Exercise}: Prove the @tech{exponential laws}:

@itemlist[
  @item{@math{𝒜^0 ≅ 1@^{𝒜} ≅ 1}}
  @item{@math{𝒜^1 ≅ 𝒜}}
  @item{@math{𝒜@^{𝒞}×ℬ@^{𝒞} ≅ (𝒜×ℬ)@^{𝒞}}}
  @item{@math{𝒞@^{𝒜×ℬ} ≅ (𝒞@^{ℬ})@^{𝒜}}}
  @item{@math{𝒞@^{𝒜+ℬ} ≅ 𝒞@^{𝒜}×𝒞@^{ℬ}}}
]

@bold{Exercise}: Think about what structure an @deftech{endofunctor category}
@math{[𝒞, 𝒞]} exhibits when considering @tech{horizontal composition}.

In Racket, to distinguish between operations in the @tech{horizontal category}
and @tech{vertical category}, we introduce the notions of @deftech{src},
@deftech{tgt}, and @deftech{∙} to denote the @tech{domain}, @tech{codomain}, and
@tech{compose} operators in @math{𝒟@^{𝒞}}. Additionally, we stipulate that
@racket[(∘)] and @racket[(∙)] must return the same value.

@section{2-Category}

A @tech{set} (@deftech{0-category}) is defined by a @tech{collection} of
@tech{elements} (@deftech{0-cell}s). Extending this idea, a @tech{category}
(@deftech{1-category}) is defined by two @tech{collections}: @tech{objects}
(@tech{0-cells}) and @tech{morphisms} (@deftech{1-cell}s). Importantly, in a
@tech{category}, @tech{objects} can be seen as @tech{identity morphisms}, a
special case of @tech{morphisms}.

This natural progression leads us to consider whether we can extend our
abstraction to include @deftech{2-morphism}s (@deftech{2-cell}s). In other words,
we can think about constructing a @deftech{2-category}, which is defined by not
only @tech{0-cells} and @tech{1-cells} but also @tech{2-cells}. Just as in a
@tech{1-category}, @tech{0-cells} are a special case of @tech{1-cells}, in a
@tech{2-category}, @tech{1-cells} are a special case of @tech{2-cells}. This
layered structure allows us to study the relationships between @tech{morphisms}
in a deeper and more systematic way, paving the way for powerful abstractions in
@tech{category theory}.

We already have an example of such a structure: @tech{𝐂𝐚𝐭}. In @tech{𝐂𝐚𝐭},
@tech{categories} serve as @tech{0-cells}, @tech{functors} act as @tech{1-cells},
and @tech{natural transformations} provide the additional layer of abstraction
as @tech{2-cells}. This makes @tech{𝐂𝐚𝐭} a natural reference for understanding
the concept of a @tech{2-category}.

To formalize this idea, we look at how @tech{𝐂𝐚𝐭} operates. There are two distinct
@tech{composition} operations for @tech{natural transformations} within @tech{𝐂𝐚𝐭}:
@tech{horizontal composition} and @tech{vertical composition}. These two forms of
@tech{composition} interact systematically, governed by the @tech{interchange law}.
We can describe @tech{𝐂𝐚𝐭} in terms of three interrelated @tech{categories}: the
@tech{base category} @tech{𝐂𝐚𝐭@^{b}}, the @tech{horizontal category} @tech{𝐂𝐚𝐭@^{h}},
and the @tech{vertical category} @tech{𝐂𝐚𝐭@^{v}}.

Using these properties, we @racket[define] a @tech{2-category} @math{𝐂} as a
structure consisting of three @tech{collections}: @math{𝐂_n} of @deftech{n-cells}
for @math{n = 0, 1, 2}. In @math{𝐂}, there are two ways to @tech{compose}
@tech{2-cells}: @tech{horizontal composition} and @tech{vertical composition},
which satisfy the @tech{interchange law}. Additionally, @math{𝐂} can be described
in terms of three @tech{1-categories}: the @tech{base category} @math{𝐂^b}, the
@tech{horizontal category} @math{𝐂^h}, and the @tech{vertical category} @math{𝐂^v}.

In a general @tech{2-category}, we may not know the specific internal structure
of the @tech{1-cells}. However, we can draw inspiration from the concept of
@tech{global elements}. In @tech{𝐂𝐚𝐭}, any @tech{category} @math{𝒞} is
@tech{isomorphic} to @math{𝒞^1}. This observation motivates us to @racket[define]
a similar concept in any @tech{2-category} @math{𝐂} that contains a
@tech{terminal object} @tech{1}. Specifically, for any @tech{0-cell} @math{𝒞 : 𝐂},
we @racket[define] the @tech{1-cells} @math{1 → 𝒞} as the @deftech{global object}s
of @math{𝒞}, and the @tech{2-cells} between them as the @deftech{global morphism}s
of @math{𝒞}.

@subsection{String Diagram}

Traditional @tech{diagrams} represent @tech{0-cells} as @tech{nodes},
@tech{1-cells} as single @tech{arrows} between these @tech{nodes}, and
@tech{2-cells} as double @tech{arrows} between these single @tech{arrows}.
In contrast, @deftech{string diagrams} provide a more intuitive and geometrical
representation:

@itemlist[
  @item{@tech{0-cell} is represented as a plane.}
  @item{@tech{1-cell} is represented as a line segment that splits a plane.}
  @item{@tech{2-cell} is represented as a point that splits a line segment.}
]

@tech{String diagrams} are a powerful tool for visualizing relationships between
@tech{n-cells} within a @tech{2-category} @math{𝐂}. Below, we illustrate a
@tech{2-cell} @math{α : F ⇒ G : 𝒞 → 𝒟 : 𝐂}, using both a traditional
@tech{diagram} and a corresponding @tech{string diagram}:

@margin-note{
By default, @tech{string diagrams} are read from right to left and from bottom
to top.
}

@image["scribblings/natural transformation/images/α.svg"]{[picture] α.svg}

We also use special notations in @tech{string diagrams}:

@itemlist[
  @item{The @tech{identity} @tech{1-cell} is represented by a dashed line or may
        be omitted entirely for simplicity.}
  @item{Curved lines or arcs can also be used to represent @tech{1-cells}, and
        their intersection represents a @tech{2-cell}.}
  ]

The following two examples illustrate the special notations used in
@tech{string diagrams}. These @tech{string diagrams} show @tech{equivalent} but
visually distinct representations of the same structures.

The first one shows a @tech{2-cell} @math{α : G∘F ⇒ id@_{𝒞}}, where
@math{F : 𝒞 → 𝒟 : 𝐂} and @math{G : 𝒟 → 𝒞 : 𝐂}:

@image["scribblings/natural transformation/images/α_0.svg"]{[picture] α_0.svg}
@image["scribblings/natural transformation/images/α_1.svg"]{[picture] α_1.svg}
@image["scribblings/natural transformation/images/α_2.svg"]{[picture] α_2.svg}
@image["scribblings/natural transformation/images/α_3.svg"]{[picture] α_3.svg}

The second one shows two @tech{2-cells} @math{α : G∘F ⇒ id@_{𝒞}} and
@math{β : id@_{𝒟} ⇒ H∘G}, where @math{F : 𝒞 → 𝒟 : 𝐂}, @math{G : 𝒟 → 𝒞 : 𝐂},
and @math{H : 𝒞 → 𝒟 : 𝐂}:

@image["scribblings/natural transformation/images/β&α_0.svg"]{[picture] β&α_0.svg}
@image["scribblings/natural transformation/images/β&α_1.svg"]{[picture] β&α_1.svg}
@image["scribblings/natural transformation/images/β&α_2.svg"]{[picture] β&α_2.svg}
@image["scribblings/natural transformation/images/β&α_3.svg"]{[picture] β&α_3.svg}

The advantage of using @tech{string diagrams} lies in their simplicity when
representing complex structures in a @tech{2-category}. Instead of working with
layers of @tech{nodes} and @tech{arrows}, @tech{string diagrams} allow us to
represent these relationships in a clear, visual manner that highlights how each
part of the structure interacts with the others.

@bold{Exercise}: The following is a @tech{string diagram}, try to draw the
corresponding @tech{diagram}.

@image["scribblings/natural transformation/images/str-diag.svg"]{[picture] str-diag.svg}

If there are no @tech{2-cells} in a @tech{string diagram}, we can further
compress it for simplicity. Specifically:

@itemlist[
  @item{@tech{0-cell} is represented as a line segment.}
  @item{@tech{1-cell} is represented as a point that splits a line segment.}
]

This compressed representation is not limited to @math{𝐂^b} but can also be
extended to other @tech{1-categories}.

@subsection{Strict Monoidal Category}

A @deftech{strict monoidal category} @math{(𝒞, ⊗, I)} is a @tech{category}
@math{𝒞} equipped with a @deftech{tensor product} @deftech{⊗} and a
@deftech{tensor unit} @math{I}. The @tech{tensor product} is a @tech{functor}
@math{⊗ : 𝒞×𝒞 → 𝒞}, and the @tech{tensor unit} is a @deftech{unit object}
@math{I : 𝒞}, such that for any @tech{objects} @math{A, B, C : 𝒞},
@math{(A⊗B)⊗C = A⊗(B⊗C)} and @math{A = A⊗I = I⊗A}.

Just as a @tech{one-object category} @math{𝒞} can be viewed as a @tech{monoid}
(@tech{monoidal set}) @math{(𝒞_1, ∘, id@_{∗})}, we extend this idea to view a
@deftech{one-object 2-category} @math{𝐂} as a @tech{strict monoidal category}
@math{(𝐂^v, ∘, id^b@_{∗})}. In this context, the @tech{vertical category}
@math{𝐂^v} is equipped with the @tech{horizontal composition} @math{∘}, which
acts as the @tech{tensor product}, and the @tech{identity} @tech{1-cell}
@math{id^b@_{∗}}, which serves as the @tech{tensor unit}.

@subsection{Equivalence}

In a @tech{2-category} @math{𝐂}, @tech{equivalence} is a weaker version of
@tech{isomorphism}. For @tech{1-cells} @math{F: 𝒞 → 𝒟 : 𝐂} and @math{G: 𝒟 → 𝒞 : 𝐂},
if @math{id@_{𝒞} ≅ G∘F} and @math{F∘G ≅ id@_{𝒟}}, then @math{F} and @math{G} are
both @deftech{equivalence}s.

@image["scribblings/natural transformation/images/eqv.svg"]{[picture] eqv.svg}

In this case, @math{G} is a @deftech{pseudo-inverse} of @math{F} and @math{F} is
a @tech{pseudo-inverse} of @math{G}. @math{𝒞} and @math{𝒟} are
@deftech{equivalent} to each other (@math{𝒞 @deftech{≃} 𝒟}) if there exists an
@tech{equivalence} between them.

@bold{Exercise}: Prove that @tech{≃} is an @tech{equivalence relation} over
@math{𝐂_0}.

@bold{Exercise}: Prove that every @tech{0-cell} is @tech{equivalent} to itself.

@bold{Exercise}: Prove that the @tech{pseudo-inverse} of an @tech{equivalence} is
not unique.
