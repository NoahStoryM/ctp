#lang scribble/manual

@(require (for-label racket/base
                     racket/match
                     math/matrix)
          "../ctp-utils.rkt")

@title[#:tag "_Category_"]{Category}

Welcome to the first chapter of our @secref{_CTP_} tutorial! Here, we delve into
the foundational concepts of @tech{category theory}, focusing on @tech{morphisms}
as the central entities of study. This chapter sets the stage for understanding
how these entities interact within the structured universe of @tech{categories},
using Racket programming language as our exploration tool.

@local-table-of-contents[]

@section{Basic Definitions}

In the abstract world of mathematics, @tech{category theory} offers a unified
framework for analyzing and synthesizing concepts across different fields. Central
to our exploration are @tech{morphisms}, which we consider not merely as
connections or processes but as entities in their own right.

@subsection{Category}

@margin-note{
This tutorial focuses on @deftech{@deftech{small} categories} and does not cover
@deftech{@deftech{large} categories}. For more information on @tech{small} and
@tech{large} @tech{categories}, please refer to
@hyperlink["https://ncatlab.org/nlab/show/small+category"]{Small Category}
and
@hyperlink["https://ncatlab.org/nlab/show/large+category"]{Large Category}.
}

A @deftech{category} @math{𝒞} is defined by 2 collections: @math{𝒞_0} of
@deftech{objects} and @math{𝒞_1} of @deftech{morphisms}. Think of @math{𝒞} as a
directed graph, where @tech{objects} are nodes, and @tech{morphisms} are arrows
connecting these nodes.

For a @tech{morphism} @math{f: a → b} in @math{𝒞}, its starting points (@deftech{domain})
is @math{a}, and its ending point (@deftech{codomain}) is @math{b}:
@math{dom(f) = a} and @math{cod(f) = b}.

@image["scribblings/category/images/cat.svg"]{[picture] cat.svg}

Our approach to @tech{category theory} places @tech{morphisms} at the core,
viewing a @tech{category} not just as a network of @tech{objects} linked by
@tech{morphisms}, but as a universe where @tech{morphisms} themselves are the
primary focus. In this universe, @tech{objects} serve more as structural markers
than active participants, and @tech{morphisms} are entities that can represent
transformations, operations, or even concrete entities like @tech/refer{numbers},
@tech/refer{lists}, and @tech/refer{strings}, as long as they adhere to the
@deftech{composition rules}:

@itemlist[
  #:style 'ordered
  @item{Existence of @deftech{composition}

        For @tech{morphisms} @math{f} and @math{g} in @math{𝒞}, a @tech{composition}
        @math{g∘f} is defined iff @math{cod(f) = dom(g)}. When @math{g∘f}
        is defined, @math{dom(g∘f) = dom(f)} and @math{cod(g∘f) = cod(g)}.

        @image["scribblings/category/images/C-1.svg"]{[picture] C-1.svg}}
  @item{Associativity of @tech{composition}

        @margin-note{
        Note that a @deftech{composable pair} consists of not only a pair of
        @tech{morphisms}, but also the @tech{domains} and @tech{codomains} of them.
        See more in @hyperlink["https://ncatlab.org/nlab/show/composable%20pair"]{nLab}.
        }

        For @tech{composable pairs} @math{(f, g)} and @math{(g, h)} in @math{𝒞},
        @math{(h∘g)∘f = h∘(g∘f)}, denoted as @math{h∘g∘f}.

        @image["scribblings/category/images/C-2.svg"]{[picture] C-2.svg}}
  @item{Existence of @deftech{identity morphisms}

        Every @tech{object} has an associated @tech{identity morphism}.
        For an @tech{object} @math{a} in @math{𝒞}, its @tech{identity morphism}
        is denoted as @math{id_a} or @math{1_a}, and @math{a = dom(id_a) = cod(id_a)}.

        @image["scribblings/category/images/C-3.svg"]{[picture] C-3.svg}}
  @item{@tech{Composition} and @tech{identity morphisms}

        For a @tech{morphism} @math{f: a → b} in @math{𝒞}, @math{f = f∘id_a = id_b∘f}.

        @image["scribblings/category/images/C-4.svg"]{[picture] C-4.svg}}
  ]

@margin-note{
In traditional @tech{category theory}, @tech{categories} are often named after
their @tech{objects}.

In this tutorial, we intentionally use @tech{morphisms} to label @tech{categories}.
This approach emphasizes the role of @tech{morphisms} as the central focus.

Nevertheless, classic @tech{category} names will still be given precedence and
used whenever applicable in this tutorial.
}

In traditional @tech{category theory}, a distinction is often made between
@tech{objects} and @tech{identity morphisms}. However, in this tutorial, we adopt
a unique perspective by using @tech{identity morphisms} to directly represent
@tech{objects}. There is no strict demarcation between @tech{objects} and their
associated @tech{identity morphisms}; they are treated interchangeably (@math{a = id_a}).

Remember, while it may seem that @tech{objects} take a secondary role, their
presence as @tech{identity morphisms} is essential for facilitating the dynamic
interplay of @tech{morphisms}, which are the focal point of our study.

We've laid the groundwork for understanding the fundamental principles of
@tech{category theory}. If you're eager to see how these abstract concepts
translate into practical programming examples, feel free to jump to
@secref{Mapping_Category_to_Programming}. There, we'll explore how the ideas of
@tech{objects}, @tech{morphisms}, and @tech{compositions} come to life in the
Racket programming environment, providing concrete examples.

@subsection{Commutative Diagram}

Informally, a @deftech{diagram} comprises various @tech{objects} connected by
various @tech{morphisms}. When the @tech{morphisms} with the same @tech{domain}
and @tech{codomain} are the same one, the @tech{diagram} is a
@deftech{@deftech{commutative} diagram}.

@tech{Commutative diagrams} serve as a powerful language for expressing equations.

@subsubsection{Commutative Triangle}

A @deftech{commutative triangle} is a @tech{commutative diagram} that has the shape
of a triangle.

The equation @math{h = g∘f} can be pictured as a @tech{commutative triangle}
like this:

@image["scribblings/category/images/comm-tri.svg"]{[picture] comm-tri.svg}

@subsubsection{Commutative Square}

A @deftech{commutative square} is a @tech{commutative diagram} that has the shape
of a square.

The equation @math{k∘f = g∘h} can be pictured as a @tech{commutative square}
like this:

@image["scribblings/category/images/comm-sqr.svg"]{[picture] comm-sqr.svg}

If there is a @tech{morphism} @math{l} making @math{f = l∘h} and @math{g = k∘l},
then @math{l} is a @deftech{lift} (@deftech{diagonal fill-in} or @deftech{filler})
in the @tech{commutative square}:

@image["scribblings/category/images/lift_1.svg"]{[picture] lift_1.svg}
@image["scribblings/category/images/lift_2.svg"]{[picture] lift_2.svg}

@subsection{One-Object Category}

@margin-note{
A @deftech{monoid} (@deftech{monoidal set}) @math{(S, *, e)} is a @tech{set}
@math{S} equipped with an associative binary operation @math{*} and an
@deftech{identity element} @math{e}.
}

A @deftech{one-object category} (@deftech{OOC}) can be viewed as a @tech{monoid}.
In @tech{OOC}, there is only a single @tech{object}, usually denoted as @math{*},
and @tech{morphisms} are defined within the context of @math{*}.

@image["scribblings/category/images/ooc.svg"]{[picture] ooc.svg}

The @tech{monoid} structure becomes evident when we consider the @tech{identity morphism}
as the @tech{monoid} @tech{identity element} and the @tech{composition} operation
as the @tech{monoid} operation. Thus, @tech{OOCs} provide a categorical perspective
on @tech{monoids}.

@subsection{Subcategory}

Given @tech{categories} @math{𝒞} and @math{𝒟}, @math{𝒟} is a @deftech{subcategory}
of @math{𝒞} iff:

@itemlist[
  #:style 'ordered
  @item{@math{𝒟_0 ⊆ 𝒞_0} and @math{𝒟_1 ⊆ 𝒞_1}.}
  @item{If the @tech{object} @math{a} is in @math{𝒟}, then so is @math{id_a}.}
  @item{If the @tech{morphism} @math{f} is in @math{𝒟}, then so are @math{dom(f)} and @math{cod(f)}.}
  @item{If the @tech{composable pair} @math{(f, g)} is in @math{𝒟}, then so is @math{g∘f}.}
  ]

@subsubsection{Full Subcategory}

A @deftech{@deftech{full} subcategory} arises when we selectively remove certain
@tech{objects} from a @tech{category} @math{𝒞} along with the @tech{morphisms}
whose @tech{domains} or @tech{codomains} involve these @tech{objects}. The
resulting @tech{subcategory} @math{𝒟}, retains all the @tech{morphisms} from
@math{𝒞} that have not been affected by the removal of @tech{objects}.

@subsubsection{Wide Subcategory}

A @deftech{@deftech{wide} subcategory} is a @tech{subcategory} that includes all
@tech{objects} from the original @tech{category}. Formally, if @math{𝒟} is a
@tech{wide subcategory} of @math{𝒞}, then every @tech{object} in @math{𝒞} is also
an @tech{object} in @math{𝒟}.

@section{Mapping Category to Programming}

In this section, we'll explore how @tech{category theory} concepts can be mapped
to practical programming constructs.

Just as @racket[car], @racket[cdr], and @racket[cons] provide an abstraction
for @tech/refer{pairs} in Racket, we'll introduce the notions of
@deftech{dom}, @deftech{cod}, and @deftech{∘}
(representing @tech{domain}, @tech{codomain}, and @deftech{compose})
to abstract over @tech{categories}.

We stipulate that @code{(∘)} returns @math{*}, @code{(∘ m)} returns @code{m},
and @code{(morphism=? m)} returns @code{#t} in Racket.

@subsection{Category Examples}

Let's see how these abstractions can be applied to create and manipulate
@tech{categories} in the context of programming.

@subsubsection{Category of Natural Numbers}

The @tech{category} of natural @tech/refer{numbers} is an example of @tech{OOC}.
In this case, @tech{morphisms} are natural @tech/refer{numbers}, and the single
@tech{object} @math{*} is @code{0} (as the @tech{identity morphism}):

@racketfile{category/code/cat-of-nn.rkt}

@subsubsection{Category of Lists}

The @tech{category} of @tech/refer{lists} is also an @tech{OOC}, where @math{*}
is @racket[null] and morphisms are @tech/refer{lists}:

@racketfile{category/code/cat-of-ls.rkt}

@subsubsection{Category of Strings}

@bold{Exercise}: referencing the example code above, implement the @tech{category}
of @tech/refer{strings}, which is also an @tech{OOC}.

@subsubsection{Category of Matrices}

The @tech{category} of @tech/math[#:key "matrix"]{matrices} is a fascinating
example that brings together linear algebra and @tech{category theory}. In this
@tech{category}, each @math{m × n} @tech/math{matrix} is considered a @tech{morphism},
its @tech{domain} is the n-order identity @tech/math{matrix}, and its @tech{codomain}
is the m-order identity @tech/math{matrix}:

@racketfile{category/code/cat-of-mat.rkt}

@subsubsection{Category of Binary Relations}

@margin-note{
A @deftech{preordered set} @math{(S, ≤)} is a @tech{set} @math{S} equipped with
a binary relation @math{≤} that is reflexive and transitive.
}

A @tech{preordered set} @math{(S, ≤)} can be viewed as a @tech{category} where
@tech{morphisms} are binary relations on its underlying @tech{set} @math{S}:

@racketfile{category/code/cat-of-br.rkt}

@margin-note{
A @deftech{partially ordered set} (@deftech{poset}) is a @tech{preordered set}
@math{(S, ≤)} for which @math{≤} is antisymmetric.
}

@bold{Exercise}: view a @tech{poset} as a @tech{category} and implement it.

@subsubsection{Category of Procedures}

The @tech{category} of procedures is perhaps the most important @tech{category}
in programming. As the name suggests, this @tech{category} has procedures
(also known as functions in functional programming) as its @tech{morphisms},
so it resembles the @tech{category} of @tech{sets}
(with mathematical @tech{functions} as @tech{morphisms}) in @tech{category theory}.

However, this @tech{category} is not a strict @tech{category} that follows the
@tech{composition rules}, unlike the examples we introduced above. It has some defects.

From the computing science point of view, @tech{category theory} is a strongly
typed language, stronger than any programming language. This is because of the
@tech{composition rule}: @math{g∘f} exists iff @math{cod(f) = dom(g)}.
Racket is an untyped language, it allows any procedure to be composed, such as
@code{(compose car +)}, but such a procedure will only @racket[raise] an @racket[exn]
when applied. Therefore, this @tech{category} can be regarded as an @tech{OOC}:

@racketblock[
(define (dom _) (∘))
(define (cod _) (∘))
(define ∘ compose)
]

Another defect is that we cannot compare whether two procedures have the same
functionality, which means we cannot @racket[define] @code{morphism=?}, and have
to rely on the programmer to judge whether the behavior of two procedures is same.
For Racket, it cannot even be sure that @math{g∘f = g∘f} !

@subsection{Constructions on Categories}

This section involves the creation of new @tech{categories} using existing ones.
These constructions provide a way to extend our understanding of @tech{categories}
and explore various relationships between them.

@subsubsection{Opposite Category}

The @deftech{dual} of a @tech{category} @math{𝒞} is the reverse version of @math{𝒞},
denoted as @deftech{opposite category} @math{𝒞^op}.

@image["scribblings/category/images/¬cat.svg"]{[picture] ¬cat.svg}

A @tech{category} @math{𝒞} can be viewed as a directed graph that adheres to the
@tech{composition rules}. If we reverse all the arrows in the directed graph,
the resulting new directed graph still adheres to the @tech{composition rules},
so this new directed graph is also a @tech{category} @math{𝒞^op}.

@bold{Exercise}: prove that @math{(𝒞^op)^op = 𝒞}.

@racketblock[
(define (¬ dom𝒞 cod𝒞 ∘𝒞)
  (define (dom m) (cod𝒞 m))
  (define (cod m) (dom𝒞 m))
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values dom cod ∘))
]

@subsubsection{Product Category}

@margin-note{
In this context, @tech[#:key "cartesian product"]{products} refer to
@deftech{cartesian products}, which are @tech{product objects} in the
@tech{category} of @tech{sets}.
}

A @deftech{product category} @math{𝒞 × 𝒟} combines the given @tech{categories}
@math{𝒞} and @math{𝒟} to form a new @tech{category}.

@image["scribblings/category/images/prod-cat.svg"]{[picture] prod-cat.svg}

@bold{Exercise}: prove that @math{(g_0∘f_0, g_1∘f_1) = (g_0, g_1)∘(f_0, f_1)}.

Let's illustrate this concept with a Racket code example
(@racket[list] is used here as @tech{cartesian product}). In the following code,
we create a @tech{product category} by taking the @tech[#:key "cartesian product"]{product}
of the @secref["Category_of_Matrices"] and the @secref["Category_of_Binary_Relations"].

@racketfile{category/code/prod-cat.rkt}

@bold{Exercise}: @racket[define] @code{dom×}, @code{cod×}, @code{∘×},
@code{morphism×?} and @code{morphism×=?} so that we can @racket[define]
@tech{category} @math{ℳ × ℛ} in this way:

@racketblock[
(define dom (dom× domℳ domℛ))
(define cod (cod× codℳ codℛ))
(define ∘ (∘× ∘ℳ ∘ℛ))
(define morphism? (morphism×? morphismℳ? morphismℛ?))
(define morphism=? (morphism×=? morphismℳ=? morphismℛ=?))
]

@subsubsection{Arrow Category}

Given a @tech{category} @math{𝒞}, the @deftech{arrow category} @math{Arr(𝒞)} is
constructed by takeing its @tech{morphisms} as @tech{objects} and @tech{commutative squares}
as @tech{morphisms}.

For example, here are @code{3} @tech{commutative squares} in @math{𝒞}:

@image["scribblings/category/images/arr-cat_1.svg"]{[picture] arr-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative squares} by @tech{composition}:

@image["scribblings/category/images/arr-cat_2.svg"]{[picture] arr-cat_2.svg}

Finally, using nodes to represent @tech{morphisms}, and using arrows to represent
@tech{commutative squares}, we get a directed graph that obeys the @tech{composition rules},
which is the @tech{arrow category} @math{Arr(𝒞)}:

@margin-note{
Although we name arrows using pairs here, note that they are not pairs, but
@tech{commutative squares}.
}

@image["scribblings/category/images/arr-cat_3.svg"]{[picture] arr-cat_3.svg}

@bold{Exercise}: prove that @math{(k∘i, l∘j) = (k, l)∘(i, j)}.

In the following code, we create an @tech{arrow category} to which the
@secref["Category_of_Matrices"] gives rise:

@racketfile{category/code/arr-cat.rkt}

@bold{Exercise}: @racket[define] @code{Arr} so that we can @racket[define] the
@tech{arrow category} @math{Arr(ℳ)} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  (Arr domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?))
]

@subsubsection{(Co)Slice Category}

A @deftech{slice category} (@deftech{over category}) @math{𝒞/c} is a construction
that allows us to study a @tech{category} @math{𝒞} through the lens of a fixed
@tech{object} @math{c} in @math{𝒞}. Intuitively, @math{𝒞/c} consists of all
the @tech{objects} and @tech{morphisms} in @math{𝒞} that are "over" @math{c}.

@math{𝒞/c} is constructed by takeing @math{𝒞}'s @tech{morphisms} end to @math{c}
as @tech{objects}, and @tech{commutative triangles} end to @math{c} as @tech{morphisms}.

For example, here are @code{3} @tech{commutative triangles} end to @math{c_1}
in @math{𝒞}:

@image["scribblings/category/images/over-cat_1.svg"]{[picture] over-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["scribblings/category/images/over-cat_2.svg"]{[picture] over-cat_2.svg}

Finally, using nodes to represent @tech{morphisms} end to @math{c_1}, and using
arrows to represent @tech{commutative triangles} end to @math{c_1}, we get a
directed graph that obeys the @tech{composition rules}, which is the
@tech{slice category} @math{𝒞/c_1}:

@margin-note{
Although we name arrows using @tech{morphisms} here, note that they are not
@tech{morphisms}, but @tech{commutative triangles} end to @math{c_1}.
}

@image["scribblings/category/images/over-cat_3.svg"]{[picture] over-cat_3.svg}

@bold{Exercise}: referencing the example code of the @tech{arrow category}
@math{Arr(ℳ)}, implement the @tech{slice category} @math{ℳ/m}.

@bold{Exercise}: @racket[define] @code{Sli} so that we can @racket[define]
the @tech{slice category} @math{ℳ/m} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  ((Sli domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?) m))
]

The @tech{dual} notion of a @tech{slice category} @math{𝒞/c} is a @deftech{coslice category}
(@deftech{under category}) @math{c/𝒞}, which consists of all the @tech{objects}
and @tech{morphisms} in @math{𝒞} that are "under" @math{c}.

@math{c/𝒞} is constructed by takeing @math{𝒞}'s @tech{morphisms} start from @math{c}
as @tech{objects}, and @tech{commutative triangles} start from @math{c} as @tech{morphisms}.

For example, here are @code{3} @tech{commutative triangles} start from @math{c_0}
in @math{𝒞}:

@image["scribblings/category/images/under-cat_1.svg"]{[picture] under-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["scribblings/category/images/under-cat_2.svg"]{[picture] under-cat_2.svg}

Finally, using nodes to represent @tech{morphisms} start from @math{c_0}, and using
arrows to represent @tech{commutative triangles} start from @math{c_0}, we get a
directed graph that obeys the @tech{composition rules}, which is the
@tech{coslice category} @math{c_0/𝒞}:

@margin-note{
Although we name arrows using @tech{morphisms} here, note that they are not
@tech{morphisms}, but @tech{commutative triangles} start from @math{c_0}.
}

@image["scribblings/category/images/under-cat_3.svg"]{[picture] under-cat_3.svg}

@bold{Exercise}: referencing the example code of the @tech{arrow category}
@math{Arr(ℳ)}, implement the @tech{coslice category} @math{m/ℳ}.

@bold{Exercise}: @racket[define] @code{¬Sli} so that we can @racket[define]
the @tech{coslice category} @math{m/ℳ} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  ((¬Sli domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?) m))
]

@bold{Exercise}: prove that @math{ℳ^op/m = (m/ℳ)^op}.

@bold{Exercise}: @racket[define] @code{¬Sli} by using @code{¬} and @code{Sli}.

@section{Categorical Definitions}

In this section, we explore the fundamental idea of defining properties a
@tech{category} may have solely through @tech{objects} and @tech{morphisms} in it.
This approach, known as the @deftech{categorical definition}, allows us to capture
and express important concepts using the language of @tech{category theory}.

@subsection{Endomorphism}

For a @tech{morphism} @math{f}, it is an @deftech{endomorphism} iff @math{dom(f) = cod(f)}.

@image["scribblings/category/images/endo.svg"]{[picture] endo.svg}

@subsubsection{Idempotent}

For an @tech{endomorphism} @math{f}, it is an @deftech{idempotent} iff @math{f = f∘f}.

@image["scribblings/category/images/idem.svg"]{[picture] idem.svg}

@subsection{Split Morphism}

@margin-note{
We can see from the names that:

@itemlist[
  @item{a @tech{split monomorphism} is a @tech{monomorphism};}
  @item{a @tech{split epimorphism} is an @tech{epimorphism};}
  @item{a @tech{split idempotent} is an @tech{idempotent}.}
  ]

The proof is left as an exercise.
}

For @tech{morphisms} @math{f: a → b} and @math{g: b → a} in @math{𝒞}, if
@math{g∘f = id_a}, then @math{f} is a @deftech{split monomorphism}
(often abbreviated as @deftech{split mono}, or called be @deftech{split monic}),
@math{g} is a @deftech{split epimorphism}
(often abbreviated as @deftech{split epi}, or called be @deftech{split epic}),
and @math{f∘g} is a @deftech{split idempotent}.

@image["scribblings/category/images/split.svg"]{[picture] split.svg}

In this case, @math{f} is a @deftech{right inverse} of @math{g}, and @math{g} is
a @deftech{left inverse} of @math{f}. @math{a} is called a @deftech{retract} of
@math{b}, @math{f} is called a @deftech{section} of @math{g}, @math{g} is called
a @deftech{cosection} (@deftech{retraction}) of @math{f}, or a @tech{retraction}
of @math{b} onto @math{a}.

Examples in the @secref["Category_of_Matrices"]:

@racketblock[
(code:comment2 "Objects")
(define a (identity-matrix 2))
(define b (identity-matrix 3))

(code:comment2 "Morphisms")
(define f (matrix [[1 -2] [0 1] [0 0]])) (code:comment "split monomorphism")
(define g (matrix [[1 2 0] [0 1 0]]))    (code:comment "split epimorphism")
(define f∘g (∘ f g))                     (code:comment "split idempotent")

(code:comment2 "g∘f is the identity morphism of a")
(morphism=? a (∘ g f))

(code:comment2 "f∘g is an endomorphism of b")
(morphism=? b (dom f∘g) (cod f∘g))

(code:comment2 "f∘g is an idempotent")
(morphism=? f∘g (∘ f∘g f∘g))
]

@subsection{Isomorphism}

For @tech{morphisms} @math{f: a → b} and @math{g: b → a} in @math{𝒞}, if @math{g∘f = id_a}
and @math{f∘g = id_b}, then @math{f} and @math{g} are both @deftech{isomorphisms}
(often abbreviated as @deftech{iso}, or called be @deftech{isic} or @deftech{invertible}).

@image["scribblings/category/images/iso.svg"]{[picture] iso.svg}

In this case, @math{g} is the @deftech{inverse} of @math{f}, denoted as @math{f^{–1}},
and @math{f} is the @tech{inverse} of @math{g}, denoted as @math{g^{–1}}.
@math{a} and @math{b} are @deftech{isomorphic} to each other (@math{a ≅ b}) iff
there exists an @tech{isomorphism} between them.

Examples in the @secref["Category_of_Binary_Relations"]:

@racketblock[
(code:comment2 "Objects")
(define a '(a . a))
(define b '(b . b))

(code:comment2 "Morphisms")
(define f '(a . b))
(define g '(b . a))

(code:comment2 "a ≅ b")
(morphism=? a (∘ g f))
(morphism=? b (∘ f g))
]

@bold{Exercise}: prove that every @tech{object} is @tech{isomorphic} to itself.

@bold{Exercise}: prove that for a @tech{isomorphism} @math{f}, @math{f = (f^{–1})^{–1}}.

@bold{Exercise}: prove that for @tech{isomorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{(g∘f)^{–1} = f^{–1}∘g^{–1}}.

@subsubsection{Automorphism}

An @deftech{automorphism} is an @tech{invertible} @tech{endomorphism}.

@image["scribblings/category/images/auto.svg"]{[picture] auto.svg}

@subsubsection{Representative Subcategory}

A @deftech{@deftech{representative} subcategory} is a @tech{subcategory} @math{𝒟}
of a @tech{category} @math{𝒞} that every @tech{object} of @math{𝒞} is @tech{isomorphic}
to some @tech{object} of @math{𝒟}.

@subsection{Groupoid}

@margin-note{
@tech{Categories} are sometimes called @deftech{monoidoids}.
}

A @deftech{groupoid} is equivalently a @tech{category} in which all @tech{morphisms}
are @tech{isomorphisms}.

@subsubsection{One-Object Groupoid}

@margin-note{
A @deftech{group} is a @tech{monoid} in which every @tech{element} has a unique
@tech{inverse}.
}

A @deftech{one-object groupoid} (@deftech{OOG}) can be viewed as a @tech{group}.

@subsection{Monomorphism and Epimorphism}

A @deftech{monomorphism} (often abbreviated as @deftech{mono}, or called be @deftech{monic})
@math{m} is defined as a @deftech{left cancellable} @tech{morphism}. This property
means that for all @tech{composable pairs} @math{(a, m)} and @math{(b, m)}, if
@math{m∘a = m∘b}, then it must follow that @math{a = b}. Such a condition ensures
that no two different @tech{morphisms}, when composed with @math{m} on the right,
result in the same @tech{morphism}, thereby establishing the @tech{injective}
nature of @math{m}.

Conversely, an @deftech{epimorphism} (often referred to as @deftech{epi}, or called be @deftech{epic})
@math{e} is defined as a @deftech{right cancellable} @tech{morphism}. This property
means that for all @tech{composable pairs} @math{(e, x)} and @math{(e, y)}, if
@math{x∘e = y∘e}, then it must follow that @math{x = y}. Such a condition ensures
that @math{e} reaches all possible endpoints in the target @tech{object} without
duplication, thereby establishing the @tech{surjective} nature of @math{e}.

For a @tech{morphism} @math{f: a → b}, if @math{f} is @tech{monic}, it's
conventionally denoted as @math{f: a ↣ b}; if @math{f} is @tech{epic}, it's
conventionally denoted as @math{f: a ↠ b}; if @math{f} is both @tech{monic} and
@tech{epic}, it's conventionally denoted as @math{f: a ⤖ b}.

@bold{Exercise}: Prove that for @tech{monomorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{g∘f} is also a
@tech{monomorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is a @tech{monomorphism}, then @math{f}
is also a @tech{monomorphism}.

@bold{Exercise}: Prove that a @tech{monomorphism} in @math{𝒞} is an
@tech{epimorphism} in @math{𝒞^op}.

@bold{Exercise}: Prove that for @tech{epimorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{g∘f} is also an
@tech{epimorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is an @tech{epimorphism}, then @math{g}
is also an @tech{epimorphism}.

@bold{Exercise}: Prove that a @tech{morphism} is @tech{invertible} iff it is both
@tech{monic} and @tech{split epic} (or is both @tech{split monic} and @tech{epic}).

@subsection{Initial Object and Terminal Object}

An @deftech{@deftech{initial} object} @math{0} in a @tech{category} @math{𝒞}
is an @tech{object} from which there exists exactly one @tech{morphism} to every
other @tech{object} @math{a} in @math{𝒞}, usually denoted as @math{!_a: 0 → a}.

@bold{Exercise}: Prove that the empty @tech{set} @math{{}} is the unique
@tech{initial object} in the @tech{category} of @tech{sets}.

Conversely, a @deftech{@deftech{terminal} object} @math{1} in a @tech{category}
@math{𝒞} is an @tech{object} to which there exists exactly one @tech{morphism} from
every other @tech{object} @math{a} in @math{𝒞}, usually denoted as @math{⟨⟩_a: a → 1}.

@bold{Exercise}: Prove that any singleton @tech{set} @math{{*}} is a
@tech{terminal object} in the @tech{category} of @tech{sets}.

@image["scribblings/category/images/0→1_1.svg"]{[picture] 0→1_1.svg}
@image["scribblings/category/images/0→1_2.svg"]{[picture] 0→1_2.svg}

@bold{Exercise}: Prove that @math{!_1=⟨⟩_0}.

@bold{Exercise}: Prove that an @tech{initial object} in @math{𝒞} is also a
@tech{terminal object} in @math{𝒞^op}.

@bold{Exercise}: Prove that @tech{initial objects} (or @tech{terminal objects})
are @tech{isomorphic} to each other.

@bold{Exercise}: Think about the relationships between @math{0/𝒞}, @math{𝒞/1},
and @math{𝒞}.

If an @tech{object} is both an @tech{initial object} and a @tech{terminal object},
it is called a @deftech{zero object} (@deftech{null object} or @deftech{biterminator}).
A @tech{category} with a @tech{zero object} is called a @deftech{pointed category}.

@subsubsection{Global Element}

If a @tech{terminal object} @math{1} exists within a @tech{category} @math{𝒞}, a
@deftech{global element} of another @tech{object} @math{a} in @math{𝒞} is defined
as a @tech{morphism} @math{1 → a}.

Although a @tech{category} is fundamentally composed of @tech{objects} and
@tech{morphisms}, @tech{objects} within some @tech{categories} possess some
inherent structures. The beauty of @tech{category theory} lies in our ability to
consistently identify special @tech{morphisms} that reveal and represent these
structures. This perspective not only deepens our understanding of tangible
structures but also inspires us to perceive connections and patterns beyond the
obvious structural definitions.

Consider the @tech{category} of @tech{sets} as an example. We understand that
@tech{sets} contain @tech{elements}. However, the definition of @tech{categories}
does not talk about the internal structure of their @tech{objects}. Therefore, we
must adopt a different viewpoint, characterizing @tech{elements} through
@tech{morphisms} instead.

@image["scribblings/category/images/global-elem_1.svg"]{[picture] global-elem_1.svg}
@image["scribblings/category/images/global-elem_2.svg"]{[picture] global-elem_2.svg}
@image["scribblings/category/images/global-elem_3.svg"]{[picture] global-elem_3.svg}

This approach to viewing @tech{elements} requires only the presence of a
@tech{terminal object} in the @tech{category}. In this context, a @tech{morphisms}
@math{1 → a} can be seen as an @tech{elements} of the @tech{object} @math{a},
thereby generalizing the concept of @tech{elements} of @tech{sets} in a broader
and more abstract manner.

In further exploring the @tech{category} of @tech{sets}, let's consider a
@tech{function} @math{f: A → B}. Traditionally, we apply @math{f} to an @tech{element}
@math{x} in @math{A}, denoted as @math{f(x)}. In @tech{category theory}, we can
express this application using @tech{morphisms}. Let @math{x} be a @tech{global element}
of @math{A}, then the application of @math{f} to @math{x} is represented by the
@tech{composition} of @math{f} with @math{x}, written as @math{f∘x}.

Thus, the notation @math{f(x)} is sometimes used in @tech{category theory} to
denote the @tech{composition} @math{f∘x}, where @math{x} is interpreted as a
@tech{morphism} rather than an @tech{element}. In this context, an arbitrary
@tech{morphism} @math{x: a → b} is called a @deftech{variable element} of @math{b},
parametrized by @math{a}. This viewpoint aligns with the idea that @tech{morphisms}
in a @tech{category} can be thought of as @tech{elements}, and an application is
a special case of @tech{morphism} @tech{composition}.
