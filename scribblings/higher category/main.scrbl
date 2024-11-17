#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0 (except-in typed/racket/base/no-check =))
                     rackunit)
          "../ctp-utils.rkt")

@title[#:tag "_Higher_Category_"]{Higher Category}

@local-table-of-contents[]

@section{2-Category}

A @tech{set} (@deftech{0-category}) is defined by a @tech{collection} of
@tech{elements} (@deftech{0-cell}s). Extending this idea, a @tech{category}
(@deftech{1-category}) is defined by two @tech{collections}: @tech{objects}
(@tech{0-cells}) and @tech{morphisms} (@deftech{1-cell}s). Importantly, in a
@tech{category}, @tech{objects} can be seen as @tech{identity morphisms}, a
special case of @tech{morphisms}.

This natural progression leads us to consider whether we can extend our
abstraction to include @deftech{2-morphism}s (@deftech{2-cell}s). In other words,
we can think about constructing a @tech{2-category}, which is defined by not
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

@margin-note{
Note that @math{𝒞_2} of a @tech{1-category} @math{𝒞} is the @tech{collection} of
@tech{composable pairs}.
}

Using these properties, we @racket[define] a @deftech{2-category} @math{𝐂} as a
structure consisting of three @tech{collections}: @math{𝐂_i} of
@deftech{i-morphism}s (@deftech{i-cell}s) for @math{i = 0, 1, 2}. In @math{𝐂},
there are two ways to @tech{compose} @tech{2-cells}: @tech{horizontal composition}
and @tech{vertical composition}, which satisfy the @tech{interchange law}.
Additionally, @math{𝐂} can be described in terms of three @tech{1-categories}:

@itemlist[
  @item{The @deftech{base category} @math{𝐂^b}:
        @math{𝐂^b_0 = 𝐂_0} and @math{𝐂^b_1 = 𝐂_1}.}
  @item{The @deftech{horizontal category} @math{𝐂^h}:
        @math{𝐂^h_0 = 𝐂_0} and @math{𝐂^h_1 = 𝐂_2}.}
  @item{The @deftech{vertical category} @math{𝐂^v}:
        @math{𝐂^v_0 = 𝐂_1} and @math{𝐂^v_1 = 𝐂_2}.}
  ]

An @tech{isomorphism} @math{α : F ⇒ G} in @math{𝐂^v} is called a
@deftech{2-isomorphism}, and @math{F} and @math{G} are @deftech{2-isomorphic} to
each other.

In a general @tech{2-category}, we may not know the specific internal structure
of the @tech{1-cells}. However, we can draw inspiration from the concept of
@tech{global elements}. In @tech{𝐂𝐚𝐭}, any @tech{category} @math{𝒞} is
@tech{isomorphic} to @math{𝒞^1}. This observation motivates us to @racket[define]
a similar concept in any @tech{2-category} @math{𝐂} that contains a
@tech{terminal object} @tech{1}. Specifically, for any @tech{0-cell} @math{𝒞 : 𝐂},
we @racket[define] the @tech{1-cells} from @tech{1} to @math{𝒞} as the
@deftech{global object}s of @math{𝒞}, and the @tech{2-cells} between them as the
@deftech{global morphism}s of @math{𝒞}.

@subsection{String Diagram}

Traditional @tech{diagrams} represent @tech{0-cells} as @tech{nodes},
@tech{1-cells} as single @tech{arrows} between these @tech{nodes}, and
@tech{2-cells} as double @tech{arrows} between these single @tech{arrows}.
In contrast, @deftech{string diagrams} provide a more intuitive and geometrical
representation:

@itemlist[
  @item{@tech{0-cell} is represented by a portion of a plane.}
  @item{@tech{1-cell} is represented by a @deftech{string} separating the plane in two.}
  @item{@tech{2-cell} is represented by an intersection of @tech{strings}.}
]

@tech{String diagrams} are a powerful tool for visualizing relationships between
@tech{i-cells} within a @tech{2-category} @math{𝐂}. Below, we illustrate a
@tech{2-cell} @math{α : F ⇒ G : 𝒞 → 𝒟 : 𝐂}, using both a traditional
@tech{diagram} and a corresponding @tech{string diagram}:

@margin-note{
By default, @tech{string diagrams} are read from right to left and from bottom
to top.
}

@image["scribblings/higher category/images/alpha.svg"]{[picture] alpha.svg}

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

@image["scribblings/higher category/images/alpha_0.svg"]{[picture] alpha_0.svg}
@image["scribblings/higher category/images/alpha_1.svg"]{[picture] alpha_1.svg}
@image["scribblings/higher category/images/alpha_2.svg"]{[picture] alpha_2.svg}
@image["scribblings/higher category/images/alpha_3.svg"]{[picture] alpha_3.svg}

The second one shows two @tech{2-cells} @math{α : G∘F ⇒ id@_{𝒞}} and
@math{β : id@_{𝒟} ⇒ H∘G}, where @math{F : 𝒞 → 𝒟 : 𝐂}, @math{G : 𝒟 → 𝒞 : 𝐂},
and @math{H : 𝒞 → 𝒟 : 𝐂}:

@image["scribblings/higher category/images/beta&alpha_0.svg"]{[picture] beta&alpha_0.svg}
@image["scribblings/higher category/images/beta&alpha_1.svg"]{[picture] beta&alpha_1.svg}
@image["scribblings/higher category/images/beta&alpha_2.svg"]{[picture] beta&alpha_2.svg}
@image["scribblings/higher category/images/beta&alpha_3.svg"]{[picture] beta&alpha_3.svg}

The advantage of using @tech{string diagrams} lies in their simplicity when
representing complex structures in a @tech{2-category}. Instead of working with
layers of @tech{nodes} and @tech{arrows}, @tech{string diagrams} allow us to
represent these relationships in a clear, visual manner that highlights how each
part of the structure interacts with the others.

@bold{Exercise}: The following is a @tech{string diagram}, try to draw the
corresponding @tech{diagram}.

@image["scribblings/higher category/images/str-diag.svg"]{[picture] str-diag.svg}

If there are no @tech{2-cells} in a @tech{string diagram}, we can further
compress it for simplicity. Specifically:

@itemlist[
  @item{@tech{0-cell} is represented by a line segment.}
  @item{@tech{1-cell} is represented by a point separating the line in two.}
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

@subsubsection{Strict Symmetric Monoidal Category}

A @deftech{strict symmetric monoidal category} @math{(𝒞, ⊗, I)} is a
@tech{strict monoidal category} that @tech{⊗} is @tech{symmetric}: for any
@tech{objects} @math{A, B : 𝒞}, @math{A⊗B = B⊗A}.

@subsection{Equivalence}

In a @tech{2-category} @math{𝐂}, @tech{equivalence} is a weaker version of
@tech{isomorphism}. For @tech{1-cells} @math{F: 𝒞 → 𝒟 : 𝐂} and @math{G: 𝒟 → 𝒞 : 𝐂},
if @math{id@_{𝒞} ≅ G∘F} and @math{F∘G ≅ id@_{𝒟}}, then @math{F} and @math{G} are
both @deftech{equivalence}s (often called be @deftech{weakly invertible}).

@image["scribblings/higher category/images/eqv.svg"]{[picture] eqv.svg}

In this case, both @math{F} and @math{G} are @tech{inverses} up to
@tech{2-isomorphisms} @math{η : id@_{𝒞} ⇒ G∘F} and @math{ϵ : F∘G ⇒ id@_{𝒟}}.
@math{G} is a @deftech{pseudo-inverse} of @math{F}, and @math{F} is a
@tech{pseudo-inverse} of @math{G}. @math{𝒞} and @math{𝒟} are @deftech{equivalent}
to each other (@math{𝒞 @deftech{≃} 𝒟}) if there exists an @tech{equivalence}
between them.

@bold{Exercise}: Prove that @tech{≃} is an @tech{equivalence relation} over
@math{𝐂_0}.

@bold{Exercise}: Prove that every @tech{0-cell} is @tech{equivalent} to itself.

@bold{Exercise}: Prove that the @tech{pseudo-inverse} of an @tech{equivalence} is
not unique.

@bold{Exercise}: Prove that @tech{𝐓𝐫𝐞𝐞} is @tech{equivalent} to @tech{𝐅𝐬𝐭}.

@subsection{Bicategory}

@subsection{Monoidal Category}
