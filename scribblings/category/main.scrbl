#lang scribble/manual

@(require (for-label ctp
                     (except-in racket/base =)
                     racket/hash
                     racket/match
                     racket/promise
                     racket/set
                     rackunit
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
framework for analyzing and synthesizing concepts across different fields.
Central to our exploration are @tech{morphisms}, which we consider not merely as
connections or processes but as entities in their own right.

@subsection{Category}

@margin-note{
This tutorial does not distinguish @deftech{@deftech{small} category} and
@deftech{@deftech{large} category}. For more information on @tech{small} and
@tech{large} @tech{category}, please refer to
@hyperlink["https://ncatlab.org/nlab/show/small+category"]{Small Category}
and
@hyperlink["https://ncatlab.org/nlab/show/large+category"]{Large Category}.
}

A @deftech{category} (@deftech{cat}) @math{𝒞} is defined by two
@deftech{collections}: @math{𝒞_0} of @deftech{object}s (@deftech{0-cell}s)
and @math{𝒞_1} of @deftech{morphism}s (@deftech{1-cell}s). Think of @math{𝒞} as
a @deftech{directed graph}, where @tech{objects} are @deftech{node}s, and
@tech{morphisms} are @deftech{arrow}s connecting these @tech{nodes}.

For a @tech{morphism} @math{f: a @deftech{→} b} in @math{𝒞}, its @deftech{domain}
(@deftech{source}) is @math{a}, and its @deftech{codomain} (@deftech{target}) is
@math{b}: @math{dom(f) = a} and @math{cod(f) = b}.

@image["scribblings/category/images/cat.svg"]{[picture] cat.svg}

Our approach to @tech{category theory} places @tech{morphisms} at the core,
viewing a @tech{category} not just as a network of @tech{objects} linked by
@tech{morphisms}, but as a universe where @tech{morphisms} themselves are the
primary focus. In this universe, @tech{objects} serve more as structural markers
than active participants, and @tech{morphisms} are entities that can represent
transformations, operations, or even concrete entities like @tech/refer{numbers},
@tech/refer{lists}, @tech/refer{strings},  and @tech/refer{pairs}, as long as
they adhere to the @deftech{composition rules}:

@itemlist[
  #:style 'ordered
  @item{Existence of @deftech{composition}

        For @tech{morphisms} @math{f} and @math{g} in @math{𝒞}, the @deftech{composite}
        @math{g∘f} is defined if @math{cod(f) = dom(g)}. When @math{g∘f}
        is defined, @math{dom(g∘f) = dom(f)} and @math{cod(g∘f) = cod(g)}.

        @image["scribblings/category/images/C-1.svg"]{[picture] C-1.svg}}
  @item{@tech[#:key "associative"]{Associativity} of @tech{composition}

        @margin-note{
        Note that a @deftech{composable pair} consists of not only a pair of
        @tech{morphisms}, but also the @tech{domains} and @tech{codomains} of them.
        See more in @hyperlink["https://ncatlab.org/nlab/show/composable+pair"]{nLab}.
        }

        For @tech{composable pairs} @math{(f, g)} and @math{(g, h)} in @math{𝒞},
        @tech{composition} is @deftech{associative}: @math{(h∘g)∘f = h∘(g∘f)},
        denoted by @math{h∘g∘f}.

        @image["scribblings/category/images/C-2.svg"]{[picture] C-2.svg}}
  @item{Existence of @deftech{identity morphism}s

        Every @tech{object} has an associated @tech{identity morphism}.
        For an @tech{object} @math{a} in @math{𝒞}, its @tech{identity morphism}
        is denoted by @math{id_a} or @math{1_a}, and @math{a = dom(id_a) = cod(id_a)}.

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

@math{h} is saied to @deftech{factor through} any (and all) of @math{f}, @math{g},
and @math{b}.

@subsubsection{Commutative Square}

A @deftech{commutative square} is a @tech{commutative diagram} that has the shape
of a square.

The equation @math{k∘f = g∘h} can be pictured as a @tech{commutative square}
like this:

@image["scribblings/category/images/comm-sqr.svg"]{[picture] comm-sqr.svg}

If there is a @tech{morphism} @math{l} making @math{h = l∘f} and @math{k = g∘l},
then @math{l} is a @deftech{lift} (@deftech{diagonal fill-in} or @deftech{filler})
in the @tech{commutative square}:

@image["scribblings/category/images/lift_1.svg"]{[picture] lift_1.svg}
@image["scribblings/category/images/lift_2.svg"]{[picture] lift_2.svg}

If a @tech{lift} exists in any @tech{commutative square} involving @math{f} and
@math{g}, then we say that @math{f} is @deftech{weakly orthogonal} to @math{g},
or that @math{(f, g)} has the @deftech{lifting property}, denoted by
@math{f@deftech{↓}g} or @math{f@deftech{⧄}g}. In this case, @math{f} has the
@deftech{left lifting property} with respect to @math{g}, and @math{g} has the
@deftech{right lifting property} with respect to @math{f}. If the @tech{lift} is
unique, we say that @math{f} is @deftech{orthogonal} to @math{g}, denoted by
@math{f@deftech{⊥}g}.

From experience, if two @tech{morphisms} @math{f} and @math{g} satisfy @math{f⧄g},
then @math{f} and @math{g} often possess opposite properties. This relationship
reflects the complementary nature of their roles in a @tech{commutative square},
where the lifting property typically holds due to these contrasting characteristics.

For a @tech{class} @math{𝒞} of @tech{morphisms},
the @deftech{right weak orthogonal class} (@deftech{right Quillen negation}) is
denoted by @math{𝒞@^{⧄}}, where @math{𝒞@^{⧄} := {g | f⧄g ∀f ∈ 𝒞}}, and
the @deftech{left weak orthogonal class} (@deftech{left Quillen negation}) is
denoted by @math{@^{⧄}𝒞}, where @math{@^{⧄}𝒞 := {f | f⧄g ∀g ∈ 𝒞}}. Similarly,
the @deftech{right orthogonal class} is denoted by @math{𝒞@^{⊥}} or @math{𝒞@^{↓}},
where @math{𝒞@^{⊥} := {g | f⊥g ∀f ∈ 𝒞}}, and
the @deftech{left orthogonal class} is denoted by @math{@^{⊥}𝒞} or @math{𝒞@^{↑}},
where @math{@^{⊥}𝒞 := {f | f⊥g ∀g ∈ 𝒞}}.

@bold{Exercise}: Prove @math{𝒞@^{↓↑↓} = 𝒞@^{↓}} and @math{𝒞@^{↑↓↑} = 𝒞@^{↑}}.

@subsection{Relation}

A @deftech{relation} over some @tech{sets} is a @tech{subset} of
@tech{cartesian product} of them.

Here're some properties that a @tech{relation} @math{△} over a @tech{set}
@math{S} may have:

@itemlist[
  @item{@deftech{Symmetry}:
        @math{△} is @deftech{symmetric} if: @math{∀x, y ∈ S, x △ y ⇒ y △ x}.}
  @item{@deftech{Antisymmetry}:
        @math{△} is @deftech{antisymmetric} if: @math{∀x, y ∈ S, x △ y ∧ y △ x ⇒ x = y}.}
  @item{@deftech{Reflexivity}:
        @math{△} is @deftech{reflexive} if: @math{∀x ∈ S, x △ x}.}
  @item{@deftech{Transitivity}:
        @math{△} is @deftech{transitive} if: @math{∀x, y, z ∈ S, x △ y ∧ y △ z ⇒ x △ z}.}
]

@subsubsection{Binary Relation}

A @deftech{binary relation} from a @tech{set} @math{S} to a @tech{set} @math{T}
is a @tech{relation} over @math{S×T}.

The @deftech{diagonal relation} (@deftech{equality relation}) over a @tech{set}
@math{S}, denoted by @math{Δ_S}, is the @tech{binary relation} over @math{S}:
@math{{(x, x) | x ∈ S}}.

A @deftech{function} @math{f: S → T} can be viewed as the @tech{binary relation}:
@math{{(x, f(x)) | x ∈ S}}. The @deftech{image} of @math{f}, denoted by
@math{im(f)}, is the @tech{subset} of @math{T}: @math{{f(x) | x ∈ S}}. @math{f}
may have additional properties:

@itemlist[
  @item{Be @deftech{injective}: @math{f} is an @deftech{injection} if it maps
        distinct @tech{elements} of @math{S} to distinct @tech{elements} of
        @math{T}. Formally, @math{∀x, y ∈ S, f(x) = f(y) ⇒ x = y}.}
  @item{Be @deftech{surjective}: @math{f} is a @deftech{surjection} if every
        @tech{element} of @math{T} is the @tech{image} of some @tech{element} in
        @math{S}; formally, @math{∀y ∈ T, ∃x ∈ S, f(x) = y}.}
  @item{Be @deftech{bijective}: @math{f} is a @deftech{bijection} if it is both
        @tech{injective} and @tech{surjective}, establishing a one-to-one
        correspondence between @math{S} and @math{T}.}
]

@subsubsection{Equivalence Relation}

An @deftech{equivalence relation} @math{∼} over @math{S} is a @tech{relation}
that is @tech{reflexive}, @tech{symmetric}, and @tech{transitive}. @math{∼}
partitions @math{S} into disjoint @deftech{class}es, known as
@deftech{equivalence class}es, where all @tech{elements} within an
@tech{equivalence class} are related to each other.

For example, given an @tech{equivalence relation} @math{∼} on a @tech{set} @math{S}
and an @tech{element} @math{x ∈ S}, the @tech{equivalence class} of @math{x} under
@math{∼} is the @tech{set} of all @tech{elements} in @math{S} that are related to
@math{x}. This is denoted by @math{[x]}, where @math{[x] := {y ∈ S | x ∼ y}}.
Every @tech{element} of @math{S} belongs to exactly one @tech{equivalence class}.

@image["scribblings/category/images/eq-cls.svg"]{[picture] eq-cls.svg}

@bold{Exercise}: Prove @math{x ∼ y ⇒ [x] = [y]}.

@subsubsection{Congruence Relation}

A @deftech{congruence relation} @math{@deftech{∼}} on a @tech{category} @math{𝒞}
is an @tech{equivalence relation} on the @tech{morphisms} of @math{𝒞} that is
compatible with the @tech{composition} of @tech{morphisms}. Formally, @math{∼}
satisfies the following properties:

@itemlist[
  #:style 'ordered
  @item{@math{∀a, b ∈ 𝒞_0, a ∼ b ⇒ id_a ∼ id_b}.}
  @item{@math{∀f, g ∈ 𝒞_1, f ∼ g ⇒ dom(f) ∼ dom(g) ∧ cod(f) ∼ cod(g)}.}
  @item{@math{∀f, g ∈ Hom@_{𝒞}(b, c), ∀h ∈ Hom@_{𝒞}(a, b), ∀k ∈ Hom@_{𝒞}(c, d),
              f ∼ g ⇒ f∘h ∼ g∘h ∧ k∘f ∼ k∘g}.

        @image["scribblings/category/images/congruence_1.svg"]{[picture] congruence_1.svg}}
]

@bold{Exercise}: Show that we can replace the second properties with:
@math{∀f_1, f_2 ∈ Hom@_{𝒞}(a, b), ∀g_1, g_2 ∈ Hom@_{𝒞}(b, c),
      f_1 ∼ f_2 ∧ g_1 ∼ g_2 ⇒ g_1∘f_1 ∼ g_2∘f_2}.

@image["scribblings/category/images/congruence_2.svg"]{[picture] congruence_2.svg}

@bold{Exercise}: Let @math{∼} and @math{∽} be @tech{congruence relations}.
Prove that @math{∼ ∩ ∽} is also a @tech{congruence relation}.

A @deftech{congruence class} is an @tech{equivalence class} under a
@tech{congruence relation}.

@subsection{Discrete Category}

A @deftech{discrete category} is a @tech{category} where the only @tech{morphisms}
are the @tech{identity morphisms}. In other words, every @tech{object} is only
connected to itself via its @tech{identity morphism}. This means that a
@tech{discrete category} can be viewed as the @tech{category} version of a
@deftech{set}: the @tech{objects} of the @tech{discrete category} correspond to
the @deftech{element}s of the @tech{set}. A @tech{function} between two
@tech{sets} can be viewed as a @tech{functor} between two @tech{discrete categories}.

@subsection{One-Object Category}

A @deftech{monoid} (@deftech{monoidal set}) @math{(S, ∘, s)} is a @tech{set}
@math{S} equipped with an @tech{associative} binary operation @math{∘} and an
@deftech{identity element} @math{s}.

A @deftech{one-object category} (@deftech{OOC}) can be viewed as a @tech{monoid}.
In @tech{OOC}, there is only a single @tech{object}, usually denoted by @deftech{∗},
and @tech{morphisms} are defined within the context of @tech{∗}.

@image["scribblings/category/images/ooc.svg"]{[picture] ooc.svg}

The @tech{monoid} structure becomes evident when we consider the @tech{identity morphism}
as the @tech{monoid} @tech{identity element} and the @tech{composition} operation
as the @tech{monoid} operation. Thus, @tech{OOCs} provide a categorical perspective
on @tech{monoids}.

@subsection{Concrete Category}

A @deftech{@deftech{concrete} category} is a @tech{category} where @tech{objects}
are @deftech{structured set}s (e.g., @tech{monoids}), and @tech{morphisms} are
@deftech{homomorphism}s (e.g., @tech{monoid homomorphisms}), meaning
@tech{functions} that preserve the structure.

@subsection{Hom Set}

@margin-note{
If the @tech{morphisms} from @math{a} to @math{x} do not constitute a @tech{set},
we use the term @deftech{hom class} instead of @tech{hom set}.
}

For @tech{objects} @math{a} and @math{x} in @math{𝒞}, the @deftech{hom set},
denoted by @math{Hom@_{𝒞}(a, x)}, is the @tech{set} of all @tech{morphisms} from
@math{a} to @math{x}: @math{Hom@_{𝒞}(a, x) := {f ∈ 𝒞_1 | dom(f) = a ∧ cod(f) = x}}.

For @tech{morphisms} @math{f: a → x}, @math{i: b → a} and @math{j: x → y} in
@math{𝒞}, we can @racket[define] a @tech{function}
@math{Hom@_{𝒞}(i, j): Hom@_{𝒞}(a, x) → Hom@_{𝒞}(b, y)}, where
@math{Hom@_{𝒞}(i, j)(f) := j∘f∘i}.

@image["scribblings/category/images/hom_1.svg"]{[picture] hom_1.svg}

Additionally, we can @racket[define] two other @tech{functions}:

@itemlist[
  #:style 'ordered
  @item{@math{Hom@_{𝒞}(a, j) := Hom@_{𝒞}(id@_{a}, j)}, where
        @math{Hom@_{𝒞}(a, j)(f) = j∘f}.

        @image["scribblings/category/images/hom_2.svg"]{[picture] hom_2.svg}}
  @item{@math{Hom@_{𝒞}(i, x) := Hom@_{𝒞}(i, id@_{x})}, where
        @math{Hom@_{𝒞}(i, x)(f) = f∘i}.

        @image["scribblings/category/images/hom_3.svg"]{[picture] hom_3.svg}}
  ]

@section{Mapping Category to Programming}

In this section, we'll explore how @tech{category theory} concepts can be mapped
to practical programming constructs.

Just as @racket[car], @racket[cdr], @racket[cons], @racket[pair?], and @racket[equal?]
provide an abstraction for @tech/refer{pairs} in Racket, we'll introduce the notions
of @deftech{dom}, @deftech{cod}, @deftech{∘}, @deftech{?}, and @deftech{=}
(representing @tech{domain}, @tech{codomain}, @deftech{compose}, @deftech{predicate}, and @deftech{equal})
to abstract over @tech{categories}.

We stipulate that @code{(∘)} returns @tech{∗}, @code{(∘ m)} returns @code{m},
and @code{(= m)} returns @code{#t} in Racket.

To verify the properties of @tech{categories}, we'll @racket[define] some
@deftech{check} @tech{procedures} to automate the testing of essential properties
within a @tech{category}:

@racketfile{code/category/check.rkt}

@subsection{Category Examples}

Let's see how these abstractions can be applied to create and manipulate
@tech{categories} in the context of programming.

@subsubsection{Category of Natural Numbers}

@margin-note{
Remember that @tech{objects} serve as @tech{identity morphisms}.
}

The @tech{category} of natural @tech/refer{numbers}, denoted as @deftech{𝐍𝐚𝐭},
is an example of @tech{OOC}. In @tech{𝐍𝐚𝐭}, @tech{morphisms} are natural
@tech/refer{numbers}, and the single @tech{object}, represented by @tech{∗}, is
@code{0}:

@racketfile{code/category/𝐍𝐚𝐭.rkt}

@subsubsection{Category of Lists}

The @tech{category} of @tech/refer{lists}, denoted as @deftech{𝐋𝐢𝐬𝐭}, is also an
@tech{OOC}. In @tech{𝐋𝐢𝐬𝐭}, the single @tech{object} @tech{∗} is @racket[null],
and the @tech{morphisms} are @tech/refer{lists}:

@racketfile{code/category/𝐋𝐢𝐬𝐭.rkt}

@subsubsection{Category of Strings}

The @tech{category} of @tech/refer{strings}, denoted as @deftech{𝐒𝐭𝐫}, is also
an @tech{OOC}.

@bold{Exercise}: Using the example code provided above as a reference, implement
@tech{𝐒𝐭𝐫}.

@subsubsection{Category of Relations}

The @tech{category} of @tech{relations}, denoted as @deftech{𝐑𝐞𝐥}, where
@tech{identity morphisms} are @tech{diagonal relations}, and @tech{morphisms} are
@tech{binary relations}:

@racketfile{code/category/𝐑𝐞𝐥.rkt}

@subsubsection{Category of Pairs}

The @tech{category} of @tech/refer{pairs}, denoted as @deftech{𝐏𝐚𝐢𝐫}, where
@tech{morphisms} are @tech/refer{pairs}:

@racketfile{code/category/𝐏𝐚𝐢𝐫.rkt}

A @deftech{preordered set} (@deftech{proset}), @math{(S, ≤)}, is a @tech{set}
@math{S} equipped with a @tech{relation} @math{≤} over @math{S} that is
@tech{reflexive} and @tech{transitive}. @math{≤} is called a @deftech{preorder}
on @math{S}.

A @tech{proset} can be viewed as a @tech{subcategory} of @tech{𝐏𝐚𝐢𝐫}. Such a
@tech{category} is called the @deftech{preorder category} associated to a
@tech{proset}.

A @deftech{partially ordered set} (@deftech{ordered set}, or @deftech{poset})
is a @tech{preordered set}, @math{(S, ≤)}, in which @math{≤} is @tech{antisymmetric}.
@math{≤} is called a @deftech{partial order} on @math{S}.

A @tech{poset} can be viewed as a @tech{subcategory} of @tech{𝐏𝐚𝐢𝐫}. Such a
@tech{category} is called the @deftech{order category} of a @tech{poset}.

@bold{Exercise}: Implement the @tech{order category} of a @tech{poset}.

@subsubsection{Category of Matrices}

The @tech{category} of @tech/math[#:key "matrix"]{matrices}, denoted as @deftech{𝐌𝐚𝐭𝐫},
is a fascinating example that combines linear algebra with @tech{category theory}.
In @tech{𝐌𝐚𝐭𝐫}, each @math{m×n} @tech/math{matrix} is considered a @tech{morphism},
its @tech{domain} is the n-order identity @tech/math{matrix}, and its @tech{codomain}
is the m-order identity @tech/math{matrix}:

@racketfile{code/category/𝐌𝐚𝐭𝐫.rkt}

@subsubsection{Category of Sets}

The @tech{category} of @tech{sets}, denoted as @deftech{𝐒𝐞𝐭}, where @tech{morphisms}
are @tech{functions}:

@racketfile{code/category/𝐒𝐞𝐭.rkt}

@subsubsection{Category of Procedures}

The @tech{category} of @tech{procedures}, denoted as @deftech{𝐏𝐫𝐨𝐜}, is perhaps
the most important @tech{category} in programming. As the name suggests,
@tech{𝐏𝐫𝐨𝐜} has @deftech{procedure}s
(also known as @tech[#:key "procedure"]{functions} in functional programming)
as its @tech{morphisms}. It resembles @tech{𝐒𝐞𝐭}, where @tech{morphisms} are
mathematical @tech{functions}.

An important point to consider in @tech{𝐏𝐫𝐨𝐜} is the @tech[#:key "equal"]{equality}
of @tech{morphisms}. In @tech{𝐒𝐞𝐭}, two @tech{functions} are considered
@tech{equal} if they produce the same output for every input. However, in
@tech{𝐏𝐫𝐨𝐜}, determining whether two @tech{procedures} are @tech{equal}
(i.e., produce the same output for every possible input) is undecidable in general.
As a result, we must rely on the programmer's judgment to ascertain whether the
behavior of two @tech{procedures} is the same.

From the computing science perspective, @tech{category theory} is a strongly
typed language, stronger than any programming language. This is due to the
@tech{composition rule}: @math{g∘f} exists iff @math{cod(f) = dom(g)}. Racket, being
an untyped language, allows any @tech{procedure} to be @tech[#:key "compose"]{composed},
such as @code{(∘ car +)}, but such a @tech{procedure} will only @racket[raise] an
@racket[exn] when applied. Therefore, @tech{𝐏𝐫𝐨𝐜} can be regarded as an @tech{OOC}:

@racketfile{code/category/𝐏𝐫𝐨𝐜.rkt}

@subsection{Constructions on Categories}

This section involves the creation of new @tech{categories} using existing ones.
These constructions provide a way to extend our understanding of @tech{categories}
and explore various relationships between them.

@subsubsection{Opposite Category}

The @deftech{dual} of a @tech{category} @math{𝒞} is the reverse version of @math{𝒞},
denoted as @deftech{opposite category} @math{𝒞^op}.

@image["scribblings/category/images/op-cat.svg"]{[picture] op-cat.svg}

A @tech{category} @math{𝒞} can be viewed as a @tech{directed graph} that adheres
to the @tech{composition rules}. If we reverse all the @tech{arrows} in the
@tech{directed graph}, the resulting new @tech{directed graph} still adheres to
the @tech{composition rules}, so this new @tech{directed graph} is also a
@tech{category} @math{𝒞^op}.

@bold{Exercise}: Prove @math{(𝒞^op)^op = 𝒞}.

We can @racket[define] @deftech{†} in Racket to implement the
@tech{opposite category} @math{𝒞^op}:

@racketfile{code/category/†.rkt}

@subsubsection{Subcategory}

Given @tech{categories} @math{𝒞} and @math{𝒟}, @math{𝒟} is a @deftech{subcategory}
of @math{𝒞} if:

@margin-note{
@math{𝒞_2} is the @tech{collection} of @tech{composable pairs} in @math{𝒞}.
}

@itemlist[
  #:style 'ordered
  @item{@math{𝒟_0 ⊆ 𝒞_0 ∧ 𝒟_1 ⊆ 𝒞_1}.}
  @item{@math{∀a ∈ 𝒞_0, a ∈ 𝒟_0 ⇒ id_a ∈ 𝒟_1}.}
  @item{@math{∀f ∈ 𝒞_1, f ∈ 𝒟_1 ⇒ dom(f) ∈ 𝒟_0 ∧ cod(f) ∈ 𝒟_0}.}
  @item{@math{∀(f, g) ∈ 𝒞_2, (f, g) ∈ 𝒟_2 ⇒ g∘f ∈ 𝒟_1}.}
  ]

We can @racket[define] @deftech{⊆} in Racket to implement the @tech{subcategory}
@math{𝒟} of @math{𝒞}:

@racketfile{code/category/⊆.rkt}

A @deftech{subset} can be viewed as a @tech{subcategory} of a @tech{discrete category},
and a @deftech{submonoid} can be viewed as a @tech{subcategory} of an @tech{OOC}.

A @deftech{full subcategory} arises when we selectively remove certain
@tech{objects} from a @tech{category} @math{𝒞} along with the @tech{morphisms}
whose @tech{domains} or @tech{codomains} involve these @tech{objects}. The
resulting @tech{subcategory} @math{𝒟}, retains all the @tech{morphisms} from
@math{𝒞} that have not been affected by the removal of @tech{objects}.

A @deftech{wide subcategory} is a @tech{subcategory} that includes all
@tech{objects} from the original @tech{category}. Formally, let @math{𝒟} be a
@tech{wide subcategory} of @math{𝒞}, every @tech{object} in @math{𝒞} is also an
@tech{object} in @math{𝒟}.

@subsubsection{Quotient Category}

The @deftech{quotient} of @math{𝒞} by @math{∼}, denoted as
@deftech{quotient category} @math{𝒞/∼}, reflects the structure of @math{𝒞} but
with the @tech{morphisms} grouped into
@tech[#:key "congruence class"]{congruence classes} under @math{∼}:

@itemlist[
  #:style 'ordered
  @item{The @tech{objects} of @math{𝒞/∼} are the
        @tech[#:key "congruence class"]{congruence classes} of @tech{objects}
        of @math{𝒞}.}
  @item{The @tech{morphisms} of @math{𝒞/∼} are the
        @tech[#:key "congruence class"]{congruence classes} of @tech{morphisms}
        of @math{𝒞}.}
  @item{If @math{f: a → b} in @math{𝒞}, then @math{[f]: [a] → [b]} in @math{𝒞/∼}.}
  @item{If @math{f: a → b} and @math{g: b → c} in @math{𝒞}, then
        @math{[g]∘[f] = [g∘f]: [a] → [c]} in @math{𝒞/∼}.}
]

We can @racket[define] @deftech{÷} in Racket to implement the
@tech{quotient category} @math{𝒞/∼}:

@racketfile{code/category/÷.rkt}

A @deftech{quotient set} can be viewed as a @tech{quotient category} of a
@tech{discrete category}.

@subsubsection{Product Category}

@margin-note{
In this context, @tech[#:key "cartesian product"]{products} refer to
@deftech{cartesian product}s, which are @tech{product objects} in @tech{𝐒𝐞𝐭}.
}

A @deftech{product category} @math{𝒞×𝒟} combines the given @tech{categories}
@math{𝒞} and @math{𝒟} to form a new @tech{category}.

@image["scribblings/category/images/prod-cat.svg"]{[picture] prod-cat.svg}

@margin-note{
@tech{∘} in this equation are different in different @tech{categories}. The first
two @tech{∘}s represent @tech{compose}s in the individual @tech{categories},
while the third @tech{∘} represents @tech{compose} in the @tech{product category}.
}

@bold{Exercise}: Prove @math{(g_0∘f_0)×(g_1∘f_1) = (g_0×g_1)∘(f_0×f_1)}.

Let's illustrate this concept with a Racket code example
(@racket[list] is used here as @tech{cartesian product}). In the following code,
we create a @tech{product category} by taking the
@tech[#:key "cartesian product"]{product} of @tech{𝐌𝐚𝐭𝐫} and @tech{𝐏𝐚𝐢𝐫}:

@racketfile{code/category/𝐌𝐚𝐭𝐫×𝐏𝐚𝐢𝐫.rkt}

@bold{Exercise}: Try to @racket[define] @deftech{dom×}, @deftech{cod×}, @deftech{∘×},
@deftech{?×} and @deftech{=×} so that we can @racket[define] the
@tech{product category} @math{ℳ×𝒫} in this way:

@racketblock[
(define-values (dom cod ∘ ? =)
  (values
   (dom× domℳ dom𝒫)
   (cod× codℳ cod𝒫)
   (∘× ∘ℳ ∘𝒫)
   (?× ?ℳ ?𝒫)
   (=× =ℳ =𝒫)))
]

@subsubsection{Arrow Category}

Given a @tech{category} @math{𝒞}, the @deftech{arrow category} @math{𝒞@^{→}} is
constructed by takeing its @tech{morphisms} as @tech{objects} and @tech{commutative squares}
as @tech{morphisms}.

For example, here are three @tech{commutative squares} in @math{𝒞}:

@image["scribblings/category/images/arr-cat_1.svg"]{[picture] arr-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative squares} by @tech{composition}:

@image["scribblings/category/images/arr-cat_2.svg"]{[picture] arr-cat_2.svg}

Finally, using @tech{nodes} to represent @tech{morphisms}, and using @tech{arrows}
to represent @tech{commutative squares}, we get a @tech{directed graph} that obeys
the @tech{composition rules}, which is the @tech{arrow category} @math{𝒞@^{→}}:

@margin-note{
Although we name @tech{arrows} using pairs here, note that they are not pairs,
but @tech{commutative squares}.
}

@image["scribblings/category/images/arr-cat_3.svg"]{[picture] arr-cat_3.svg}

@bold{Exercise}: Prove @math{(k∘i, l∘j) = (k, l)∘(i, j)}.

In the following code, we create an @tech{arrow category} to which @tech{𝐏𝐚𝐢𝐫}
gives rise:

@racketfile{code/category/Arr_𝐏𝐚𝐢𝐫.rkt}

@bold{Exercise}: Try to @racket[define] @deftech{Arr} so that we can
@racket[define] the @tech{arrow category} @math{𝒫@^{→}} in this way:

@racketblock[
(define-values (dom cod ∘ ? =)
  (Arr dom𝒫 cod𝒫 ∘𝒫 ?𝒫 =𝒫))
]

@subsubsection{(Co)Slice Category}

A @deftech{slice category} (@deftech{over category}) @math{𝒞/c} is a construction
that allows us to study a @tech{category} @math{𝒞} through the lens of a fixed
@tech{object} @math{c} in @math{𝒞}. Intuitively, @math{𝒞/c} consists of all
the @tech{objects} and @tech{morphisms} in @math{𝒞} that are "over" @math{c}.

@math{𝒞/c} is constructed by takeing @math{𝒞}'s @tech{morphisms} end to @math{c}
as @tech{objects}, and @tech{commutative triangles} end to @math{c} as @tech{morphisms}.

For example, here are three @tech{commutative triangles} end to @math{c_1}
in @math{𝒞}:

@image["scribblings/category/images/over-cat_1.svg"]{[picture] over-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["scribblings/category/images/over-cat_2.svg"]{[picture] over-cat_2.svg}

Finally, using @tech{nodes} to represent @tech{morphisms} end to @math{c_1}, and
using @tech{arrows} to represent @tech{commutative triangles} end to @math{c_1},
we get a @tech{directed graph} that obeys the @tech{composition rules}, which is
the @tech{slice category} @math{𝒞/c_1}:

@margin-note{
Although we name @tech{arrows} using @tech{morphisms} here, note that they are
not @tech{morphisms}, but @tech{commutative triangles} end to @math{c_1}.
}

@image["scribblings/category/images/over-cat_3.svg"]{[picture] over-cat_3.svg}

@bold{Exercise}: Referencing the example code of the @tech{arrow category}
@math{𝒫@^{→}}, implement a @tech{slice category} @math{ℳ/m} to which @tech{𝐌𝐚𝐭𝐫}
gives rise.

@bold{Exercise}: Try to @racket[define] @deftech{Sli} so that we can @racket[define]
the @tech{slice category} @math{ℳ/m} in this way:

@racketblock[
(define-values (dom cod ∘ ? =)
  ((Sli m) domℳ codℳ ∘ℳ ?ℳ =ℳ))
]

The @tech{dual} notion of a @tech{slice category} @math{𝒞/c} is a @deftech{coslice category}
(@deftech{under category}) @math{c/𝒞}, which consists of all the @tech{objects}
and @tech{morphisms} in @math{𝒞} that are "under" @math{c}.

@math{c/𝒞} is constructed by takeing @math{𝒞}'s @tech{morphisms} start from @math{c}
as @tech{objects}, and @tech{commutative triangles} start from @math{c} as @tech{morphisms}.

For example, here are three @tech{commutative triangles} start from @math{c_0}
in @math{𝒞}:

@image["scribblings/category/images/under-cat_1.svg"]{[picture] under-cat_1.svg}

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["scribblings/category/images/under-cat_2.svg"]{[picture] under-cat_2.svg}

Finally, using @tech{nodes} to represent @tech{morphisms} start from @math{c_0},
and using @tech{arrows} to represent @tech{commutative triangles} start from @math{c_0},
we get a @tech{directed graph} that obeys the @tech{composition rules}, which is
the @tech{coslice category} @math{c_0/𝒞}:

@margin-note{
Although we name @tech{arrows} using @tech{morphisms} here, note that they are
not @tech{morphisms}, but @tech{commutative triangles} start from @math{c_0}.
}

@image["scribblings/category/images/under-cat_3.svg"]{[picture] under-cat_3.svg}

@bold{Exercise}: Referencing the example code of the @tech{arrow category}
@math{𝒫@^{→}}, implement a @tech{coslice category} @math{m/ℳ} to which @tech{𝐌𝐚𝐭𝐫}
gives rise.

@bold{Exercise}: Try to @racket[define] @deftech{Sli†} so that we can @racket[define]
the @tech{coslice category} @math{m/ℳ} in this way:

@racketblock[
(define-values (dom cod ∘ ? =)
  ((Sli† m) domℳ codℳ ∘ℳ ?ℳ =ℳ))
]

@bold{Exercise}: Prove @math{ℳ^op/m = (m/ℳ)^op}.

@bold{Exercise}: Try to @racket[define] @tech{Sli†} by using @tech{†} and @tech{Sli}.

@section{Categorical Definitions}

In this section, we explore the fundamental idea of defining properties a
@tech{category} may have solely through @tech{objects} and @tech{morphisms} in it.
This approach, known as the @deftech{categorical definition}, allows us to capture
and express important concepts using the language of @tech{category theory}.

Note that many @tech{categorical definitions} can also be described in terms of
@tech{hom sets}. Readers will be invited to prove the @tech{equivalence} of these
two approaches (i.e., the iff statements).

@subsection{Endomorphism}

For a @tech{morphism} @math{f}, it is an @deftech{endomorphism} if @math{dom(f) = cod(f)}.

@image["scribblings/category/images/endo.svg"]{[picture] endo.svg}

@subsubsection{Idempotent}

For an @tech{endomorphism} @math{f}, it is an @deftech{idempotent} if @math{f = f∘f}.

The following @tech{diagram} is @tech{commutative}:

@image["scribblings/category/images/idem.svg"]{[picture] idem.svg}

@subsection{Monomorphism and Epimorphism}

A @deftech{monomorphism} (often abbreviated as @deftech{mono}, or called be @deftech{monic})
@math{m} in a @tech{category} @math{𝒞} is defined as a @deftech{left cancellable}
@tech{morphism}: @math{∀(a, m), (b, m) ∈ 𝒞_2, m∘a = m∘b ⇒ a = b}. Such a condition
ensures that no two different @tech{morphisms}, when @tech[#:key "compose"]{composed}
with @math{m} on the right, result in the same @tech{morphism}, thereby establishing
the @tech{injective} nature of @math{m}.

@image["scribblings/category/images/mono.svg"]{[picture] mono.svg}

@bold{Exercise}: Prove that a @tech{morphism} @math{j: x → y} is @tech{monic} iff
for any @tech{object} @math{a}, @math{Hom(a, j)} is @tech{injective}.

@bold{Exercise}: Prove that every @tech{monomorphism} in @tech{𝐒𝐞𝐭} is
@tech{injective}.

@bold{Exercise}: Prove that every @tech{injection} is @tech{monic} in @tech{𝐒𝐞𝐭}.

@bold{Exercise}: Prove that for @tech{monomorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{g∘f} is also a
@tech{monomorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is a @tech{monomorphism}, then @math{f}
is also a @tech{monomorphism}.

Conversely, an @deftech{epimorphism} (often abbreviated as @deftech{epi}, or called be @deftech{epic})
@math{e} in a @tech{category} @math{𝒞} is defined as a @deftech{right cancellable}
@tech{morphism}: @math{∀(e, x), (e, y) ∈ 𝒞_2, x∘e = y∘e ⇒ x = y}. Such a condition
ensures that @math{e} reaches all possible endpoints in the target @tech{object}
without duplication, thereby establishing the @tech{surjective} nature of @math{e}.

@image["scribblings/category/images/epi.svg"]{[picture] epi.svg}

@bold{Exercise}: Prove that a @tech{morphism} @math{i: b → a} is @tech{epic} iff
for any @tech{object} @math{x}, @math{Hom(i, x)} is @tech{injective}.

@bold{Exercise}: Prove that every @tech{epimorphism} in @tech{𝐒𝐞𝐭} is
@tech{surjective}.

@bold{Exercise}: Prove that every @tech{surjection} is @tech{epic} in @tech{𝐒𝐞𝐭}.

@bold{Exercise}: Prove that a @tech{monomorphism} in @math{𝒞} is an
@tech{epimorphism} in @math{𝒞^op}.

@bold{Exercise}: Prove that for @tech{epimorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{g∘f} is also an
@tech{epimorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is an @tech{epimorphism}, then @math{g}
is also an @tech{epimorphism}.

For a @tech{morphism} @math{i: t_1 → t_2}, the notation changes based on its
properties: @math{i: t_1 ↣ t_2} if @math{i} is @tech{monic}, @math{i: t_1 ↠ t_2}
if @math{i} is @tech{epic}, and @math{i: t_1 ⤖ t_2} if @math{i} is both
@tech{monic} and @tech{epic}.

@image["scribblings/category/images/mono&epi.svg"]{[picture] mono&epi.svg}

In some cases, we use @deftech{↣} and @deftech{↠} to denote @tech{morphisms} from
two distinct @tech[#:key "class"]{classes} @math{ℰ} and @math{ℳ}, rather than
exclusively representing @tech{monomorphisms} and @tech{epimorphisms}. Additionally,
@deftech{⤖} indicates @tech{morphisms} from @math{ℰ ∩ ℳ}.

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

For @tech{morphisms} @math{f: a ↣ b} and @math{g: b ↠ a} in @math{𝒞}, if
@math{g∘f = id_a}, then @math{f} is a @deftech{split monomorphism}
(often abbreviated as @deftech{split mono}, or called be @deftech{split monic}),
@math{g} is a @deftech{split epimorphism}
(often abbreviated as @deftech{split epi}, or called be @deftech{split epic}),
and @math{f∘g} is a @deftech{split idempotent}.

The following @tech{diagram} is @tech{commutative}:

@image["scribblings/category/images/split.svg"]{[picture] split.svg}

In this case, @math{f} is a @deftech{right inverse} of @math{g}, and @math{g} is
a @deftech{left inverse} of @math{f}. @math{a} is called a @deftech{retract} of
@math{b}, @math{f} is called a @deftech{section} of @math{g}, @math{g} is called
a @deftech{cosection} (@deftech{retraction}) of @math{f}, or a @tech{retraction}
of @math{b} onto @math{a}.

@bold{Exercise}: Prove that a @tech{morphism} @math{i: b → a} is @tech{split monic}
iff for any @tech{object} @math{x}, @math{Hom(i, x)} is @tech{surjective}.

@bold{Exercise}: Prove that a @tech{morphism} @math{j: x → y} is @tech{split epic}
iff for any @tech{object} @math{a}, @math{Hom(a, j)} is @tech{surjective}.

@bold{Exercise}: Prove that every @tech{injection} in @tech{𝐒𝐞𝐭} whose
@tech{domain} is not @tech{{}} is @tech{split monic}.

@bold{Exercise}: Prove that every @tech{surjection} in @tech{𝐒𝐞𝐭} is @tech{split epic}.

Examples in @tech{𝐌𝐚𝐭𝐫}:

@racketblock[
(code:comment2 "Objects")
(define a (identity-matrix 2)) (? a)
(define b (identity-matrix 3)) (? b)

(code:comment2 "Morphisms")
(define f (matrix [[1 -2] [0 1] [0 0]])) (? f)   (code:comment "split monomorphism")
(define g (matrix [[1 2 0] [0 1 0]]))    (? g)   (code:comment "split epimorphism")
(define f∘g (∘ f g))                     (? f∘g) (code:comment "split idempotent")

(code:comment2 "g∘f is the identity morphism of a")
(= a (∘ g f))

(code:comment2 "f∘g is an endomorphism of b")
(= b (dom f∘g) (cod f∘g))

(code:comment2 "f∘g is an idempotent")
(= f∘g (∘ f∘g f∘g))
]

@subsection{Isomorphism}

For @tech{morphisms} @math{f: a → b} and @math{g: b → a} in @math{𝒞}, if @math{g∘f = id_a}
and @math{f∘g = id_b}, then @math{f} and @math{g} are both @deftech{isomorphism}s
(often abbreviated as @deftech{iso}, or called be @deftech{isic} or @deftech{invertible}).

The following @tech{diagram} is @tech{commutative}:

@image["scribblings/category/images/iso.svg"]{[picture] iso.svg}

In this case, @math{g} is the @deftech{inverse} of @math{f}, denoted by @math{f@^{–1}},
and @math{f} is the @tech{inverse} of @math{g}, denoted by @math{g@^{–1}}.
@math{a} and @math{b} are @deftech{isomorphic} to each other (@math{a @deftech{≅} b})
if there exists an @tech{isomorphism} between them.

@bold{Exercise}: Prove that every @tech{isomorphism} in @tech{𝐒𝐞𝐭} is @tech{bijective}.

@bold{Exercise}: Prove that every @tech{bijection} is @tech{invertible} in @tech{𝐒𝐞𝐭}.

@tech{Isomorphisms} are crucial because they imply that the @tech{objects} they
connect can be interchanged in any context within the @tech{category}. This means
that if @math{a ≅ b}, then any property, specifically any @tech{commutative diagram}
involving @math{a}, also holds for @math{b}. In essence, we can substitute @math{b}
for @math{a} in any @tech{commutative diagram} without affecting the
@tech[#:key "commutative"]{commutativity} of the @tech{diagram}. This property
makes @tech{isomorphisms} a weak concept of @tech{identity morphisms}.

Examples in @tech{𝐏𝐚𝐢𝐫}:

@racketblock[
(code:comment2 "Objects")
(define a '(a . a)) (? a)
(define b '(b . b)) (? b)

(code:comment2 "Morphisms")
(define f '(a . b)) (? f)
(define g '(b . a)) (? g)

(code:comment2 "a ≅ b")
(= a (∘ g f))
(= b (∘ f g))
]

@bold{Exercise}: Prove that every @tech{object} is @tech{isomorphic} to itself.

@bold{Exercise}: Prove that for a @tech{isomorphism} @math{f}, @math{f = (f@^{–1})@^{–1}}.

@bold{Exercise}: Prove that for @tech{isomorphisms} @math{f} and @math{g},
if @math{(f, g)} is a @tech{composable pair}, then @math{(g∘f)@^{–1} = f@^{–1}∘g@^{–1}}.

@bold{Exercise}: Prove that a @tech{morphism} is @tech{invertible} iff it is both
@tech{monic} and @tech{split epic}.

@bold{Exercise}: Prove that a @tech{morphism} is @tech{invertible} iff it is both
@tech{split monic} and @tech{epic}.

@bold{Exercise}: Prove that a @tech{morphism} @math{f: a → b} is @tech{invertible}
iff for any @tech{object} @math{c}, @math{Hom(c, f)} is @tech{bijective}.

@bold{Exercise}: Prove that a @tech{morphism} @math{f: a → b} is @tech{invertible}
iff for any @tech{object} @math{c}, @math{Hom(f, c)} is @tech{bijective}.

@subsubsection{Automorphism}

An @deftech{automorphism} is an @tech{invertible} @tech{endomorphism}.

The following @tech{diagrams} are @tech{commutative}:

@image["scribblings/category/images/auto_1.svg"]{[picture] auto_1.svg}
@image["scribblings/category/images/auto_2.svg"]{[picture] auto_2.svg}

@subsubsection{Representative Subcategory}

A @deftech{@deftech{representative} subcategory} is a @tech{subcategory} @math{𝒟}
of a @tech{category} @math{𝒞} that every @tech{object} of @math{𝒞} is @tech{isomorphic}
to some @tech{object} of @math{𝒟}.

@subsubsection{Replete Subcategory}

A @deftech{replete subcategory} is a @tech{subcategory} that includes all
@tech{objects} in the original @tech{category} that are @tech{isomorphic} to the
@tech{objects} in the @tech{subcategory}, as well as the corresponding
@tech{isomorphisms}. Formally, let @math{𝒟} be a @tech{replete subcategory} of
@math{𝒞}, for any @tech{object} @math{a} in @math{𝒟}, if there is an
@tech{isomorphism} @math{f: a → b} in @math{𝒞}, then both @math{b} and @math{f}
are also in @math{𝒟}.

@subsection{Groupoid}

@margin-note{
@tech{Categories} are sometimes called @deftech{monoidoid}s.
}

A @deftech{groupoid} is a @tech{category} in which all @tech{morphisms} are
@tech{isomorphisms}.

@subsubsection{One-Object Groupoid}

A @deftech{group} @math{(S, ∘, s)} is a @tech{monoid} in which every
@tech{element} @math{x} has a unique @tech{inverse} @math{x@^{–1}}:
@math{x∘x@^{–1} = x@^{–1}∘x = s}.

A @deftech{one-object groupoid} (@deftech{OOG}) can be viewed as a @tech{group}.

@subsection{Initial Object and Terminal Object}

@margin-note{
@tech{0} serves as the @tech{identity element} for @tech{+}.
}

An @deftech{@deftech{initial} object} @deftech{0} in a @tech{category} @math{𝒞}
is an @tech{object} from which there exists exactly one @tech{morphism} to every
other @tech{object} @math{a} in @math{𝒞}, usually denoted by
@math{@deftech{!}_a: 0 → a}.

@bold{Exercise}: Prove that if @math{a} and @math{b} are @tech{initial objects}
in @math{𝒞}, then @math{a ≅ b}.

@bold{Exercise}: Prove that the @deftech{empty set} @deftech{{}} is the unique
@tech{initial object} in @tech{𝐒𝐞𝐭}.

@margin-note{
@tech{1} serves as the @tech{identity element} for @tech{×}.
}

Conversely, a @deftech{@deftech{terminal} object} @deftech{1} in a @tech{category}
@math{𝒞} is an @tech{object} to which there exists exactly one @tech{morphism} from
every other @tech{object} @math{a} in @math{𝒞}, usually denoted by
@math{@deftech{⟨⟩}_a: a → 1}.

@bold{Exercise}: Prove that if @math{a} and @math{b} are @tech{terminal objects}
in @math{𝒞}, then @math{a ≅ b}.

@bold{Exercise}: Prove that any @deftech{singleton set} @deftech{{∗}} is a
@tech{terminal object} in @tech{𝐒𝐞𝐭}.

The following @tech{diagrams} are @tech{commutative}:

@image["scribblings/category/images/0→1_1.svg"]{[picture] 0→1_1.svg}
@image["scribblings/category/images/0→1_2.svg"]{[picture] 0→1_2.svg}

@bold{Exercise}: Prove @math{!_1=⟨⟩_0}.

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

If a @tech{terminal object} @tech{1} exists within a @tech{category} @math{𝒞}, a
@deftech{global element} of another @tech{object} @math{a} in @math{𝒞} is defined
as a @tech{morphism} @math{1 → a}.

Although a @tech{category} is fundamentally composed of @tech{objects} and
@tech{morphisms}, @tech{objects} within some @tech{categories} possess some
inherent structures. The beauty of @tech{category theory} lies in our ability to
consistently identify special @tech{morphisms} that reveal and represent these
structures. This perspective not only deepens our understanding of tangible
structures but also inspires us to perceive connections and patterns beyond the
obvious structural definitions.

Consider @tech{𝐒𝐞𝐭} as an example. We understand that @tech{sets} contain
@tech{elements}. However, the definition of @tech{categories} does not talk about
the internal structure of their @tech{objects}. Therefore, we must adopt a
different viewpoint, characterizing @tech{elements} through @tech{morphisms} instead.

@image["scribblings/category/images/global-elem_1.svg"]{[picture] global-elem_1.svg}
@image["scribblings/category/images/global-elem_2.svg"]{[picture] global-elem_2.svg}
@image["scribblings/category/images/global-elem_3.svg"]{[picture] global-elem_3.svg}

This approach to viewing @tech{elements} requires only the presence of a
@tech{terminal object} in the @tech{category}. In this context, the @tech{morphisms}
@math{1 → a} can be seen as the @tech{elements} of the @tech{object} @math{a},
thereby generalizing the concept of @tech{elements} of @tech{sets} in a broader
and more abstract manner.

@bold{Exercise}: A @deftech{pointed set}, @math{(S, s)}, is a @tech{set} @math{S}
equipped with a distinguished @tech{element} @math{s}, often called the
@deftech{base point}. Prove that the @tech{category} of @tech{pointed sets},
denoted as @deftech{𝐒𝐞𝐭@_{∗}}, is @tech{isomorphic} to the @tech{coslice category}
@math{1/𝐒𝐞𝐭}.

@bold{Exercise}: Consider @math{f: {a, b} → {∗}} in @math{𝐒𝐞𝐭}, which is the
simplest example of a non-@tech{injective} @tech{morphism}. Prove that a
@tech{morphism} @math{g} is @tech{injective} iff @math{f⧄g}, and iff @math{g⧄f}.

@bold{Exercise}: Consider @math{f: {} → {∗}} in @math{𝐒𝐞𝐭}, which is the
simplest example of a non-@tech{surjective} @tech{morphism}. Prove that a
@tech{morphism} @math{g} is @tech{surjective} iff @math{f⧄g}.

In further exploring @tech{𝐒𝐞𝐭}, let's consider a @tech{function} @math{f: A → B}.
Traditionally, we @racket[apply] @math{f} to an @tech{element} @math{x} in @math{A},
denoted by @math{f(x)}. In @tech{category theory}, we can express this application
using @tech{morphisms}. Let @math{x} be a @tech{global element} of @math{A}, then
the application of @math{f} to @math{x} is represented by the @tech{composite}
of @math{x} and @math{f}, written as @math{f∘x}. Thus, the notation @math{f(x)}
is sometimes used in @tech{category theory} to denote the @tech{composite}
@math{f∘x}, where @math{x} is interpreted as a @tech{morphism} rather than an
@tech{element}. In this context, an arbitrary @tech{morphism} @math{x: a → b} is
called a @deftech{variable element} of @math{b}, parametrized by @math{a}. This
viewpoint aligns with the idea that @tech{morphisms} in a @tech{category} can be
thought of as @tech{elements}, and an application is a special case of @tech{morphism}
@tech{composition}.

@subsection{Subobject and Quotient Object}

In general, when we say that @math{d} is a @deftech{substructure} of @math{c},
this often means that there exists an @deftech{inclusion function} @math{i: d → c}.
However, from the perspective of @tech{category theory}, we focus only on
@tech{morphisms} and their @tech{composition}, without considering the internal
structure of @tech{objects}.

To establish the concept of a @tech{subobject} of an @tech{object}, we consider
@math{[i]}, the @tech{equivalence class} of @tech{morphisms} end to @math{c} that
contains an @tech{inclusion function} @math{i}. Since @tech{morphisms} are not
always @tech{functions}, we cannot directly say that @math{i} is an
@tech{inclusion function}, so we generalize @math{[i]} by using @tech{monomorphism}
instead of @tech{inclusion function}.

Let @math{∼} be an @tech{equivalence relation} between @tech{monomorphisms}
@math{i: a ↣ c} and @math{j: b ↣ c} in @math{𝒞} if each can @tech{factor through}
the other. A @deftech{subobject} of @math{c} is an @tech{equivalence class} of
@tech{monomorphisms} under @math{∼}. If the @tech{subobject} does not contain
@math{id_c}, then it's a @deftech{proper subobject} of @math{c}.

@bold{Exercise}: Prove that a @tech{proper subobject} does not contain any
@tech{isomorphism}.

@bold{Exercise}: Prove @math{i ∼ j ⇒ a ≅ b}.

@bold{Exercise}: Let @math{𝒞_c} be the @tech{full subcategory} of @math{𝒞/c} on
@tech{monomorphisms}. Show that @math{𝒞_c} is a @tech{proset}, and a
@tech{subobject} of @math{c} is an @tech{isomorphism} @tech{class} of @math{𝒞_c}.

The following @tech{diagram} shows how to view a @tech{subset} @math{a := {1, 2, 3}}
of @math{c := {1, 2, 3, 4, 5, 6}} as the @tech{subobject} @math{[i]} in @tech{𝐒𝐞𝐭}:

@image["scribblings/category/images/subobj.svg"]{[picture] subobj.svg}

Similar to how a @tech{subobject} is defined via an @tech{equivalence class} of
@tech{monomorphisms}, a @tech{quotient object} is defined through an
@tech{equivalence class} of @tech{epimorphisms}.

In the same way that a @tech{subobject} @math{[i]} is concerned with
@tech{inclusion function} @math{i} via @tech{monomorphisms}, a @tech{quotient object}
@math{[p]} captures the idea of @deftech{projection function} @math{p} via
@tech{epimorphisms}. A @tech{quotient object} corresponds to a
@deftech{quotient structure} (@deftech{cosubstructure}), associated with an
@tech{equivalence relations}.

Let @math{∼} be an @tech{equivalence relation} between @tech{epimorphisms}
@math{p: c ↠ b} and @math{q: c ↠ a} in @math{𝒞} if each can @tech{factor through}
the other. A @deftech{quotient object} (@deftech{cosubobject}) of @math{c} is an
@tech{equivalence class} of @tech{epimorphisms} under @math{∼}. If the
@tech{quotient object} does not contain @math{id_c}, then it's a
@deftech{proper quotient object} (@deftech{proper cosubobject}) of @math{c}.

@bold{Exercise}: Prove that a @tech{quotient object} in @math{𝒞} is also a
@tech{subobject} in @math{𝒞@^{op}}.

@bold{Exercise}: Prove that a @tech{proper quotient object} does not contain any
@tech{isomorphism}.

@bold{Exercise}: Prove @math{p ∼ q ⇒ a ≅ b}.

@bold{Exercise}: Let @math{𝒞^c} be the @tech{full subcategory} of @math{c/𝒞} on
@tech{epimorphisms}. Show that @math{𝒞^c} is a @tech{proset}, and a
@tech{quotient object} of @math{c} is an @tech{isomorphism} @tech{class} of
@math{𝒞^c}.

The following @tech{diagram} shows how to view a @tech{quotient set}
@math{b := {{1, 4}, {2, 5}, {3, 6}}} of @math{c := {1, 2, 3, 4, 5, 6}} as the
@tech{quotient object} @math{[p]} in @tech{𝐒𝐞𝐭}:

@image["scribblings/category/images/cosubobj.svg"]{[picture] cosubobj.svg}

@subsection{Factorization System}

A @deftech{factorization system} naturally arises when we want to decompose
@tech{morphisms} in a @tech{category} into two distinct types. The goal is to
ensure that any @tech{morphism} in the @tech{category} can factor as a
@tech{composition} of two @tech{morphisms} from two different
@tech[#:key "class"]{classes}, with a structured relationship between the
different possible @deftech{factorization}s.

@subsubsection{Orthogonal Factorization System}

An @deftech{orthogonal factorization system} (@deftech{OFS}) @math{(ℰ, ℳ)} in a
@tech{category} @math{𝒞} consists of two @tech[#:key "class"]{classes} @math{ℰ}
and @math{ℳ} of @tech{morphisms} in @math{𝒞}, such that:

@itemlist[
  @item{@math{∀f ∈ 𝒞_1, ∃e ∈ ℰ, ∃m ∈ ℳ, f = m∘e}.}
  @item{@math{ℰ} and @math{ℳ} are each closed under @tech{composition}.}
  @item{@math{ℰ ∩ ℳ} contains all @tech{isomorphisms}.}
  @item{If @math{f} can factor as @math{m_1∘e_1} and @math{m_2∘e_2}, then there
        exists a unique @tech{morphism} @math{l} such that @math{e_2 = l∘e_1}
        and @math{m_1 = m_2∘l}.}
  ]

@bold{Exercise}: Prove that the unique @tech{morphism} @math{l} in the
@tech{factorization} is an @tech{isomorphism}.

@image["scribblings/category/images/l.svg"]{[picture] l.svg}

For example, in @math{𝐒𝐞𝐭}, we can think of @math{ℰ} as the @tech{class} of
@tech{surjections} and @math{ℳ} as the @tech{class} of @tech{injections}.
then @math{(ℰ, ℳ)} is an @tech{OFS}.

@bold{Exercise}: Show that every @tech{category} @math{𝒞} has an @tech{OFS} in
which @math{ℰ} consists of all @tech{morphisms} and @math{ℳ} consists of all
@tech{isomorphisms}.

@bold{Exercise}: Show that every @tech{category} @math{𝒞} has an @tech{OFS} in
which @math{ℰ} consists of all @tech{isomorphisms} and @math{ℳ} consists of all
@tech{morphisms}.

This definition of @math{OFS} explains how different @tech{factorizations} of a
@tech{morphism} relate to each other, ensuring that there exists a unique
@tech{morphism} between any two ways of factoring the same @tech{morphism}.
However, there is an equally important alternative definition that focuses on the
interaction between the two @tech[#:key "class"]{classes} of @tech{morphisms}
through the @tech{lifting property}.

This second perspective shifts the focus to @tech{commutative squares}: for every
@tech{morphism} in @math{ℰ} and every @tech{morphism} in @math{ℳ}, any
@tech{commutative square} involving them admits a unique @tech{lift}. This
property provides another way to describe the system by expressing the deep
relationship between the two @tech[#:key "class"]{classes}:

@margin-note{
@math{(ℰ, ℳ)} such that @math{ℰ⊥ℳ} is sometimes called a
@deftech{prefactorization system}.
}

@itemlist[
  @item{@math{∀f ∈ 𝒞_1, ∃e ∈ ℰ, ∃m ∈ ℳ, f = m∘e}.}
  @item{@math{ℰ⊥ℳ}: @math{ℰ = @^{⊥}ℳ ∧ ℳ = ℰ@^{⊥}}.}
]

The following @tech{diagram} is @tech{commutative}:

@image["scribblings/category/images/ℰ⊥ℳ.svg"]{[picture] ℰ⊥ℳ.svg}

@bold{Exercise}: Prove that these two definitions are @tech{equivalent}.

@subsubsection{Weak Factorization System}

In a @deftech{weak factorization system} (@deftech{WFS}) @math{(ℰ, ℳ)} in @math{𝒞},
the uniqueness condition found in @tech{OFS} is relaxed, meaning that while every
@tech{morphism} can still factor as a @tech{composition} of @tech{morphisms} from
@math{ℰ} and @math{ℳ}, there is no guarantee that the @tech{factorization} is
unique up to @tech{isomorphism}:

@itemlist[
  @item{@math{∀f ∈ 𝒞_1, ∃e ∈ ℰ, ∃m ∈ ℳ, f = m∘e}.}
  @item{@math{ℰ⧄ℳ}: @math{ℰ = @^{⧄}ℳ ∧ ℳ = ℰ@^{⧄}}.}
]

@bold{Exercise}: Prove that a @tech{WFS} @math{(ℰ, ℳ)} in @math{𝒞} is also a
@tech{WFS} @math{(ℳ, ℰ)} in @math{𝒞@^{op}}.

@bold{Exercise}: Prove @math{∀e ∈ 𝒞_1, e⧄ℳ ⇒ e ∈ ℰ}.

@bold{Exercise}: Prove @math{∀m ∈ 𝒞_1, ℰ⧄m ⇒ m ∈ ℳ}.

@bold{Exercise}: Prove that if every @tech{morphism} in @math{ℰ} is @tech{epic},
then the @tech{WFS} @math{(ℰ, ℳ)} is an @tech{OFS}.

@bold{Exercise}: Prove that if every @tech{morphism} in @math{ℳ} is @tech{monic},
then the @tech{WFS} @math{(ℰ, ℳ)} is an @tech{OFS}.
