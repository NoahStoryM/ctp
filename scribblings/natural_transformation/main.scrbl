#lang scribble/manual

@(module for-label typed/racket/base/no-check
   (require ctp
            (only-in typed/racket/base
                     require/typed
                     require/typed/provide)
            racket/function
            math/array
            math/matrix
            rackunit)
   (provide (all-from-out ctp
                          typed/racket/base/no-check
                          typed/racket/base
                          racket/function
                          math/array
                          math/matrix
                          rackunit)))
@(require (for-label (only-meta-in 0 'for-label))
          "../ctp-utils.rkt")

@title[#:tag "_Natural_Transformation_"]{Natural Transformation}

In this @seclink["_Natural_Transformation_"]{chapter}, we extend our exploration
of @tech{category theory} by introducing the concept of @tech{natural transformations}.
@tech{Natural transformations} offer a formal framework for understanding how
@tech{functors} between two @tech{categories} correspond and interact with each
other.

Building on our foundation of @tech{categories} and @tech{functors}, this
@seclink["_Natural_Transformation_"]{chapter} presents @tech{natural transformations}
in a unique way: they are defined similarly to @tech{functors}, as @tech{functions}
that map @tech{morphisms} in the @tech{domain} @tech{category} to corresponding
@tech{morphisms} in the @tech{codomain} @tech{category}, with the
@tech{naturality condition} that certain @tech{commutative diagrams} hold.

This approach highlights that @tech{functors} themselves can be viewed as special
@tech{natural transformations}, much like @tech{objects} can be viewed as special
@tech{morphisms}, specifically as @tech{identity morphisms}.

As in the previous @seclink["_Functor_"]{chapter}, we'll leverage Typed Racket
to illustrate the core principles, allowing us to express these abstract
mathematical concepts through practical programming constructs.

@local-table-of-contents[]

@section{Natural Transformation}

@margin-note{
In a sense, @math{α(f)} can be considered the @tech{commutative square}.
}

A @deftech{natural transformation} @math{α} between @tech{parallel}
@tech{functors} @math{F} and @math{G} from @math{𝒞} to @math{𝒟}, denoted by
@math{α : F @deftech{⇒} G : 𝒞 → 𝒟}, maps each @tech{morphism} @math{f : a → b : 𝒞}
to a corresponding @tech{morphism} @math{α(f) : F(a) → G(b) : 𝒟}. This mapping
must adhere to the @deftech{@deftech{naturality} condition}, expressed as
@math{α(f) = α(b)∘F(f) = G(f)∘α(a)}, ensuring that the following @tech{diagram}
is @tech{commutative}:

@image["scribblings/natural_transformation/images/N-1.svg"]{[picture] N-1.svg}

The @tech{morphism} @math{α(a)} in @math{𝒟} for an @tech{object} @math{a} in
@math{𝒞} is the @deftech{component} of @math{α} at @math{a}.

@bold{Exercise}: For a @tech{morphism} @math{i : b → a : 𝒞}. Prove that
@math{Hom@_{𝒞}(i, -)} is a @tech{hom natural transformation} from
@math{Hom@_{𝒞}(a, -)} to @math{Hom@_{𝒞}(b, -)}.

@image["scribblings/natural_transformation/images/Hom_1.svg"]{[picture] Hom_1.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: b 𝒞) (: a 𝒞) (: i (→𝒞 b a))
(: |(→𝒞 i _)| (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (|(→𝒞 i _)| j)
  (define |(→𝒞 i j)| (λ (f) (∘𝒞 j f i)))
  |(→𝒞 i j)|)
]

@bold{Exercise}: For a @tech{morphism} @math{j : x → y : 𝒞}. Prove that
@math{Hom@_{𝒞}(-, j)} is a @tech{hom natural transformation} from
@math{Hom@_{𝒞}(-, x)} to @math{Hom@_{𝒞}(-, y)}.

@image["scribblings/natural_transformation/images/Hom_2.svg"]{[picture] Hom_2.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: x 𝒞) (: y 𝒞) (: j (→𝒞 x y))
(: |(→𝒞 _ j)| (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (|(→𝒞 _ j)| i)
  (define |(→𝒞 i j)| (λ (f) (∘𝒞 j f i)))
  |(→𝒞 i j)|)
]

To verify the properties of @tech{natural transformations}, we @racket[define]
some @tech{check} @tech{procedures} to automate the testing of the
@tech{naturality} a @tech{natural transformation} has:

@racketfile{code/natural_transformation/check.rkt}

The following example illustrates how to implement @tech{natural transformations}
in Racket:

@racketfile{code/natural_transformation/Set=>Rel.rkt}

@subsection{Composition}

In this @seclink["Composition"]{section}, we explore two types of
@tech{composition} for @tech{natural transformations}: @tech{horizontal composition}
and @tech{vertical composition}. These forms of @tech{composition} are fundamental
to understanding how @tech{natural transformations} interact and provide a deeper
insight into their algebraic properties.

@subsubsection{Horizontal Composition}

Given that @tech{natural transformations} are defined as mappings of
@tech{morphisms}, it is natural to consider whether and how they can be
@tech[#:key "compose"]{composed}, similar to the @tech{composition} of
@tech{functors}. In fact, a key insight is that @tech{functors} themselves can
be viewed as special types of @tech{natural transformations}. This leads us to
@racket[define] a type of @tech{composition} for @tech{natural transformations},
known as @tech{horizontal composition}.

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

@image["scribblings/natural_transformation/images/N-2.svg"]{[picture] N-2.svg}

@bold{Exercise}: Prove that the @tech{horizontal composition} of
@tech{natural transformations} ensures that the resulting @tech{diagram} is
@tech{commutative}.

For @math{βα}, its type is @math{HF ⇒ KG}, where @math{HF} and @math{KG} are
@tech{functors} from @math{𝒞} to @math{ℰ}. Additionally, there are three important
@tech{commutative squares} associated with @math{βα}:

@itemlist[
  #:style 'ordered
  @item{@math{βα(f) : HF(f) ⇒ KG(f)} @image["scribblings/natural_transformation/images/N-2_1.svg"]{[picture] N-2_1.svg}}
  @item{@math{βα(f) : Hα(f) ⇒ Kα(f)} @image["scribblings/natural_transformation/images/N-2_2.svg"]{[picture] N-2_2.svg}}
  @item{@math{βα(f) : βF(f) ⇒ βG(f)} @image["scribblings/natural_transformation/images/N-2_3.svg"]{[picture] N-2_3.svg}}
  ]

Note that @math{Hα}, @math{Kα}, @math{βF}, and @math{βG} are all
@tech{natural transformations}, not @tech{functors}. These three
@tech{commutative squares} arise naturally because @math{βα(f)} is the body
diagonal of a @tech{commutative cube}, and there are precisely three faces of
this cube whose face diagonals coincide with this body diagonal. Each of these
faces also has a cube edge that starts from @math{HF(a)}, which uniquely
identifies them among all the cube's faces:

@image["scribblings/natural_transformation/images/N-2_4.svg"]{[picture] N-2_4.svg}

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

@image["scribblings/natural_transformation/images/N-3.svg"]{[picture] N-3.svg}
@image["scribblings/natural_transformation/images/N-3_1.svg"]{[picture] N-3_1.svg}
@image["scribblings/natural_transformation/images/N-3_2.svg"]{[picture] N-3_2.svg}

@bold{Exercise}: Prove that the @tech{vertical composition} of
@tech{natural transformations} ensures that the resulting @tech{diagram} is
@tech{commutative}.

@bold{Exercise}: Prove that @tech{vertical composition} is @tech{associative}.

@subsubsection{Interchange Law}

The @tech{Interchange Law} explains how @tech{horizontal composition} and
@tech{vertical composition} of @tech{natural transformations} interact with
each other.

To understand how the @tech{IL} works, recall that applying a @tech{natural transformation}
to a @tech{morphism} often results in a @tech{morphism} which is the diagonal of
a @tech{commutative square}. All @tech{composition} of @tech{natural transformations}
with the same @tech{domain} and @tech{codomain} ultimately produce the same
@tech{commutative squares}, so that they are equal. This is precisely what the
@tech{IL} states.

Consider the @tech{natural transformations} @math{α_0 : F ⇒ G : 𝒞 → 𝒟},
@math{β_0 : G ⇒ H : 𝒞 → 𝒟}, @math{α_1 : K ⇒ L : 𝒟 → ℰ}, and
@math{β_1 : L ⇒ M : 𝒟 → ℰ}. The @tech{commutative diagram} below illustrates the
relationships between them:

@image["scribblings/natural_transformation/images/N-4.svg"]{[picture] N-4.svg}

@margin-note{
In some @tech{category theory} texts, @math{∘} denotes @tech{vertical composition}
and @math{∗} denotes @tech{horizontal composition}:
@math{(β_1∗β_0)∘(α_1∗α_0) = (β_1∘α_1)∗(β_0∘α_0)}.
}

We can @tech[#:key "vertical composition"]{vertically compose}
@math{α_0} with @math{β_0}, and @math{α_1} with @math{β_1}, as well as
@tech[#:key "horizontal composition"]{horizontally compose} @math{α_0} with
@math{α_1}, and @math{β_0} with @math{β_1}. The @tech{IL} states that the
@tech{horizontal composition} of two @tech{vertical compositions} is equal to the
@tech{vertical composition} of two @tech{horizontal compositions}. More precisely,
the @deftech{interchange law} (@deftech{IL}) can be written as:
@math{(β_1∘β_0)∙(α_1∘α_0) = (β_1∙α_1)∘(β_0∙α_0)}.

@image["scribblings/natural_transformation/images/IL.svg"]{[picture] IL.svg}

Here are some important @tech{commutative squares} that arise:

@itemlist[
  #:style 'ordered
  @item{@math{(β_1∘β_0)∙(α_1∘α_0)(f) : KF(f) ⇒ MH(f)}
        @image["scribblings/natural_transformation/images/N-4_0.svg"]{[picture] N-4_0.svg}}
  @item{@math{(β_1∙α_1)∘(β_0∙α_0)(f) : KF(f) ⇒ MH(f)}
        @image["scribblings/natural_transformation/images/N-4_1.svg"]{[picture] N-4_1.svg}}
  @item{@math{(β_1∙α_1)∘(β_0∙α_0)(f) : K(β_0∙α_0)(f) ⇒ M(β_0∙α_0)(f)}
        @image["scribblings/natural_transformation/images/N-4_2.svg"]{[picture] N-4_2.svg}}
  @item{@math{(β_1∙α_1)∘(β_0∙α_0)(f) : (β_1∙α_1)F(f) ⇒ (β_1∙α_1)H(f)}
        @image["scribblings/natural_transformation/images/N-4_3.svg"]{[picture] N-4_3.svg}}
  ]

@image["scribblings/natural_transformation/images/N-4_4.svg"]{[picture] N-4_4.svg}

@subsection{Structure of 𝐂𝐚𝐭}

In the previous @seclink["_Functor_"]{chapter}, we introduced what we referred to
as @tech{𝐂𝐚𝐭}, which consists of @tech{categories} as @tech{objects} and
@tech{functors} as @tech{morphisms}. Strictly speaking, this was actually the
@tech{base category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{b}}.

@image["scribblings/natural_transformation/images/Cat^b.svg"]{[picture] Cat^b.svg}

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

The @tech{horizontal category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{h}},
can be viewed as an extension of @tech{𝐂𝐚𝐭@^{b}}. In @tech{𝐂𝐚𝐭@^{b}},
@tech{objects} are @tech{categories} and @tech{morphisms} are @tech{functors}
between @tech{categories}. In @tech{𝐂𝐚𝐭@^{h}}, the @tech{objects} remain the same
but the @tech{morphisms} are generalized to include all @tech{natural transformations}
between @tech{functors}.

@image["scribblings/natural_transformation/images/Cat^h.svg"]{[picture] Cat^h.svg}

In @tech{𝐂𝐚𝐭@^{h}}, @tech{horizontal composition} serves as the @tech{composition}
operation for @tech{morphisms} in it. This perspective allows us to see that
@tech{horizontal composition} essentially works like the @tech{composition} of
@tech{functions}: both @tech{functors} and @tech{natural transformations} are
kinds of @tech{functions} between @tech{categories}.

@subsubsection{Vertical Category}

The @tech{vertical category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{v}},
provides a perspective that focuses on the relationships between @tech{functors}
through @tech{natural transformations}. In @tech{𝐂𝐚𝐭@^{v}}, @tech{objects} are
@tech{functors} between @tech{categories} and @tech{morphisms} are
@tech{natural transformations} between @tech{functors}. An @tech{isomorphism}
@math{α : F ⇒ G} in @tech{𝐂𝐚𝐭@^{v}} is called a @deftech{natural isomorphism},
and @math{F} and @math{G} are @deftech{naturally isomorphic} to each other.

@image["scribblings/natural_transformation/images/Cat^v.svg"]{[picture] Cat^v.svg}

@bold{Exercise}: Prove that a @tech{natural transformation} @math{α : F ⇒ G : 𝒞 → 𝒟}
is a @tech{natural isomorphism} iff each @tech{component} of @math{α} is an
@tech{isomorphism} in @math{𝒟}.

@bold{Exercise}: For a @tech{natural isomorphism} @math{α : F ⇒ G : 𝒞 → 𝒟} and
a @tech{morphism} @math{f : a → b : 𝒞}. Prove @math{F(f) = α@^{–1}(b)∘G(f)∘α(a)}
and @math{G(f) = α(b)∘F(f)∘α@^{–1}(a)}.

In @tech{𝐂𝐚𝐭@^{v}}, @tech{vertical composition} serves as the @tech{composition}
operation for @tech{morphisms} in it. This perspective helps us understand why
@tech{functors} can be viewed as a special case of @tech{natural transformations}.

Consider a @deftech{functor category} @math{[𝒞 → 𝒟]}
(@deftech{exponential category} @math{𝒟@^{𝒞}}), which has all the @tech{functors}
from @math{𝒞} to @math{𝒟} as @tech{objects}, and all the
@tech{natural transformations} between those @tech{functors} as @tech{morphisms}.
In this @tech{category}, every @tech{functor} @math{F : 𝒞 → 𝒟} can be viewed as
the @tech{identity} @tech{natural transformation} @math{id_F : F ⇒ F}, which acts
as the @tech{identity morphism}.

@bold{Exercise}: For @tech{objects} @math{𝒜}, @math{ℬ}, and @math{𝒞} in @tech{𝐂𝐚𝐭}.
Prove the @tech{exponential laws}:

@itemlist[
  @item{@math{𝒜^0 ≅ 1@^{𝒜} ≅ 1}}
  @item{@math{𝒜^1 ≅ 𝒜}}
  @item{@math{𝒜@^{𝒞}×ℬ@^{𝒞} ≅ (𝒜×ℬ)@^{𝒞}}}
  @item{@math{𝒞@^{𝒜×ℬ} ≅ (𝒞@^{ℬ})@^{𝒜}}}
  @item{@math{𝒞@^{𝒜+ℬ} ≅ 𝒞@^{𝒜}×𝒞@^{ℬ}}}
]

@bold{Exercise}: Think about what structure an @deftech{endofunctor category}
@math{[𝒞 → 𝒞]} exhibits when considering @tech{horizontal composition}.

In Racket, to distinguish between operations in the @tech{horizontal category}
and @tech{vertical category}, we introduce the notions of @deftech{src},
@deftech{tgt}, and @deftech{∙} to denote the @tech{domain}, @tech{codomain}, and
@tech{compose} operators in @math{𝒟@^{𝒞}}. Additionally, we stipulate that
@racket[(∘)] and @racket[(∙)] must return the same value.

@bold{Exercise}: Try to @racket[define] @racket[make-vertical-compose] so that
we can @racket[define] the @tech{compose} operator in @math{𝒟@^{𝒞}} like this:

@racketblock[(define ∙ (make-vertical-compose 𝒞 𝒟))]

@section{Comma Category}

In previous content, we examined how @tech{natural transformations} provide a
framework for studying relationships between two @tech{functors} with the same
@tech{domain} and @tech{codomain}. Now, we consider a more general question:
can we use a similar structure to investigate @tech{functors} with different
@tech{domains} but the same @tech{codomain}?

As a @tech{function} that maps @tech{morphisms}, a @tech{natural transformation}
@math{α : F ⇒ G : 𝒞 → 𝒟} establishes a relationship between @math{F} and @math{G}
through the @tech{naturality condition}. Specifically, for each @tech{morphism}
@math{f} in @math{𝒞}, @math{α(f)} corresponds uniquely to a @tech{commutative square}.
In this sense, studying the relationship between @math{F} and @math{G} via @math{α}
essentially constructs an @tech{arrow category}, where @tech{objects} are the
@tech{components} of @math{α}, and @tech{morphisms} are the @tech{commutative squares}
that satisfy the @tech{naturality condition}.

Inspired by this approach, we can investigate the relationship between two
@tech{functors} @math{F: 𝒞 → ℰ} and @math{G: 𝒟 → ℰ} by constructing an
@tech{arrow category} that represents the interactions between @math{F} and
@math{G}. In such a @tech{category}, @tech{objects} are triples @math{(a, x, α)},
where @math{a} is an @tech{object} in @math{𝒞}, @math{x} is an @tech{object} in
@math{𝒟}, and @math{α : F(a) → G(x)} is a @tech{morphism} in @math{ℰ}. The
@tech{morphisms} in this @tech{category} are pairs @math{(i, j)}, where
@math{i : a → b} is a @tech{morphism} in @math{𝒞} and @math{j : x → y} is a
@tech{morphism} in @math{𝒟}, such that the following @tech{diagram} is
@tech{commutative}:

@image["scribblings/natural_transformation/images/comma_1.svg"]{[picture] comma_1.svg}

This @tech{category} is called a @deftech{comma category} of @math{F} and
@math{G}, denoted by @math{F/G} (@math{F↓G}).

@bold{Exercise}: Try using @tech{comma category} to @racket[define]
@tech{arrow category} and (@tech[#:key "coslice category"]{co})@tech{slice category}.

To explore the relationships between @math{F/G}, @math{𝒞}, @math{𝒟}, and @math{ℰ},
we introduce two @tech{forgetful functors} @math{H_F : F/G → 𝒞} and
@math{H_G : F/G → 𝒟}. @math{H_F} maps @math{(a, x, α)} to @math{a} and
@math{(i, j)} to @math{i}, while @math{H_G} maps @math{(a, x, α)} to @math{x}
and @math{(i, j)} to @math{j}. Furthermore, these @tech{functors} are connected
by a @tech{natural transformation} @math{θ : F∘H_F ⇒ G∘H_G}, which maps
@math{(a, x, α)} to @math{α}.

@image["scribblings/natural_transformation/images/comma_2.svg"]{[picture] comma_2.svg}

Building on the earlier motivation, we saw that @tech{comma categories} can
be used to study relationships between two @tech{functors} with the same
@tech{codomain}. Since @tech{natural transformations} describe relationships
between @tech{functors} with the same @tech{domain} and @tech{codomain}, it is
natural to explore their connection with @tech{comma categories}.

Given a @tech{natural transformation} @math{α : F ⇒ G : 𝒞 → 𝒟}, we can construct
a @tech{functor} @math{A : 𝒞 → F/G} such that @math{H_F∘A = H_G∘A = id@_{𝒞}}.
Conversely, given such a @tech{functor} @math{A}, we can construct a
@tech{natural transformation} @math{α}.

@image["scribblings/natural_transformation/images/ntf-as-ftr.svg"]{[picture] ntf-as-ftr.svg}

@section{Yoneda Lemma}

Philosophically speaking, one might say that the essence of an entity lies in the
totality of its relationships with other entities. In @tech{category theory},
this idea extends to @tech{morphisms}, which can be seen as fundamental "entities"
within a @tech{category}.

We can express this concept by examining the relationships between a @tech{morphism}
@math{f : a → x} in a @tech{category} @math{𝒞} and other @tech{morphisms} in
@math{𝒞}. Specifically, we use the @tech{natural transformations}
@math{Hom@_{𝒞}(f, -) : Hom@_{𝒞}(x, -) ⇒ Hom@_{𝒞}(a, -) : 𝒞 → 𝐒𝐞𝐭} and
@math{Hom@_{𝒞}(-, f) : Hom@_{𝒞}(-, a) ⇒ Hom@_{𝒞}(-, x) : 𝒞@^{op} → 𝐒𝐞𝐭}
to describe how @math{f} interacts with other @tech{morphisms} in @math{𝒞}.

This naturally leads us to ask about the connection between @math{f},
@math{Hom@_{𝒞}(f, -)}, and @math{Hom@_{𝒞}(-, f)}. The @tech{Yoneda Lemma},
a cornerstone of @tech{category theory}, addresses this question by establishing
that these perspectives are in one-to-one correspondence with each other.

The @deftech{Yoneda Lemma} sets up a one-to-one correspondence between
@tech{elements} of @math{F(S)}, where @math{S : 𝒞} and @math{F : 𝒞 → 𝐒𝐞𝐭}, and
@tech{variable elements} of @math{F}, parametrized by @math{Hom@_{𝒞}(S, -)}.
Specifically, an @tech{element} @math{s ∈ F(S)} uniquely corresponds to a
@tech{natural transformation} @math{ρ : Hom@_{𝒞}(S, -) ⇒ F}.

@image["scribblings/natural_transformation/images/run_1.svg"]{[picture] run_1.svg}

To better understand the @tech{Yoneda Lemma}, let's revisit the @tech{TDFA}
@math{ℳ} defined in @seclink["Typed_Deterministic_Finite_Automaton"]. We denote
the @tech{free category} of the @tech{typed alphabet} @math{𝒢} as @math{𝒞},
and let @math{F : 𝒞 → 𝐒𝐞𝐭} represent the @tech{typed action} @math{φ@^{*}}.
In this context, consider the @tech{run function}, which takes an input
@tech{sequence} @math{w} and returns a @tech{final state} @math{F(w)(s_0)} after
processing @math{w}, starting from the @tech{start state} @math{s_0}.

According to the @tech{Yoneda Lemma}, we see that each @tech{component} of a
@tech{natural transformation} @math{ρ : Hom@_{𝒞}(S, -) ⇒ F} at an @tech{object}
@math{T : 𝒞} can be understood as a @tech{run function} starting from a
@tech{state} @math{s ∈ F(S)}: @math{∀w ∈ Hom@_{𝒞}(S, T), ρ(T)(w) = F(w)(s)}.
Here, @math{F(T)} can be interpreted as the @tech{set} of @tech{final states}.

To illustrate this correspondence, we use Racket code to @racket[define] two
@tech{procedures}: @racket[s->ρ] and @racket[ρ->s]. They demonstrate how an
@tech{element} @math{s} of @math{F(S)} can be transformed into a
@tech{natural transformation} @math{ρ}, and vice versa, providing a concrete way
to visualize the one-to-one correspondence described by the @tech{Yoneda Lemma}.

@racketfile{code/natural_transformation/s<->run.rkt}

This correspondence also holds for @tech{contravariant hom functors}. There is a
one-to-one correspondence between @tech{elements} of @math{G(T)}, where
@math{T : 𝒞@^{op}} and @math{G : 𝒞@^{op} → 𝐒𝐞𝐭}, and @tech{variable elements} of
@math{G}, parametrized by @math{Hom@_{𝒞}(-, T)}. Specifically, an @tech{element}
@math{t ∈ G(T)} uniquely corresponds to a @tech{natural transformation}
@math{ρ : Hom@_{𝒞}(-, T) ⇒ G}.

@image["scribblings/natural_transformation/images/run_2.svg"]{[picture] run_2.svg}

@bold{Exercise}: Try to @racket[define] two @tech{procedures} using Racket code:
@racket[t->ρ] and @racket[ρ->t].

@margin-note{
@tech{Forgetful functors} to @tech{𝐒𝐞𝐭} are very often @tech{representable}.
}

The definition of the @tech{Yoneda Lemma} can be generalized by replacing the
@tech{hom functor} @math{Hom@_{𝒞}(S, -)} with any @tech{functor} that is
@tech{naturally isomorphic} to it. Such a @tech{functor} is called a
@deftech{@deftech{representable} functor} and we say that @math{S}
@deftech{represent}s this @tech{functor}.

@bold{Exercise}: For a @tech{functor} @math{F : 𝒞 → 𝐒𝐞𝐭}. Prove that if
@tech{objects} @math{a} and @math{x} in @math{𝒞} both @tech{represent} @math{F},
then @math{a ≅ x}.

@subsection{Yoneda Embedding}

The @tech{Yoneda Lemma} establishes a powerful correspondence: a
@tech{natural transformation} @math{ρ : Hom@_{𝒞}(S, -) ⇒ F} uniquely corresponds
to an @tech{element} @math{s ∈ F(S)}. By carefully selecting @tech{functor}
@math{F}, we can choose to study specific entities within the corresponding
@tech{set} @math{F(S)} and examine the relationship between these entities and
@tech{natural transformations}.

For example, if we let @math{F} be a @tech{hom functor}, then the @tech{elements}
of @math{F(S)} are precisely the @tech{morphisms} in @math{𝒞} that start from or
end to @math{S}. An important corollary of the @tech{Yoneda Lemma} is that any
@tech{natural transformation} between @tech{hom functors} in a @tech{category}
@math{𝒞} must be a @tech{hom natural transformation} because it corresponds
exactly to a @tech{morphism} in @math{𝒞}.

To illustrate this, consider two @tech{hom functors} @math{Hom@_{𝒞}(x, −)} and
@math{Hom@_{𝒞}(a, −)}, where @math{a} and @math{x} are @tech{objects} in @math{𝒞}.
According to the @tech{Yoneda Lemma}, an @tech{element} @math{f ∈ Hom@_{𝒞}(a, x)},
which is also a @tech{morphism} in @math{𝒞}, uniquely corresponds to the
@tech{hom natural transformation}
@math{Hom@_{𝒞}(f, -) : Hom@_{𝒞}(x, -) ⇒ Hom@_{𝒞}(a, -)}.

This shows that any @tech{natural transformation} between @tech{hom functors}
is directly determined by a @tech{morphism} in @math{𝒞}. Thus, the
@tech{Yoneda Lemma} establishes a one-to-one correspondence between a
@tech{morphism} @math{f} and a @deftech{hom natural transformation}
@math{Hom@_{𝒞}(f, -)}.

@bold{Exercise}: Show the one-to-one correspondence between @math{f} and
@math{Hom@_{𝒞}(-, f)}.

@bold{Exercise}: Prove that @math{f} is an @tech{isomorphism} in @math{𝒞} iff
@math{Hom@_{𝒞}(f, -)} is a @tech{natural isomorphism}, and iff
@math{Hom@_{𝒞}(-, f)} is a @tech{natural isomorphism}.

Building on the correspondence established by the @tech{Yoneda Lemma}, we can
@racket[define] a @tech{functor} @math{Y : 𝒞@^{op} → [𝒞 → 𝐒𝐞𝐭]} that maps each
@tech{object} @math{a : 𝒞} to the @tech{covariant hom functor} @math{Hom@_{𝒞}(a, -)},
and each @tech{morphism} @math{i : b → a : 𝒞} to the @tech{hom natural transformation}
@math{Hom@_{𝒞}(i, -) : Hom@_{𝒞}(a, -) ⇒ Hom@_{𝒞}(b, -)}. This @tech{functor} is
known as the @deftech{Yoneda embedding} for @math{𝒞}.

@racketblock[
(: Y (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→ (→𝒞 a x) (→𝒞 b y)))))))
(define Y (curry |(→𝒞 _ _)|))
]

@bold{Exercise}: Prove that @math{Y} is an @tech{embedding}.

Here is another @tech{Yoneda embedding} @math{J : 𝒞 → [𝒞@^{op} → 𝐒𝐞𝐭]} that maps
each @tech{object} @math{x : 𝒞} to the @tech{contravariant hom functor}
@math{Hom@_{𝒞}(-, x)}, and each @tech{morphism} @math{j : x → y : 𝒞} to the
@tech{hom natural transformation}
@math{Hom@_{𝒞}(-, j) : Hom@_{𝒞}(-, x) ⇒ Hom@_{𝒞}(-, y)}.

@racketblock[
(: J (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→ (→𝒞 a x) (→𝒞 b y)))))))
(define J (curryr |(→𝒞 _ _)|))
]

@bold{Exercise}: Prove that @math{J} is an @tech{embedding}.

In @seclink["Cayley_s_Theorem"], we explored the categorical version of
@tech{Cayley's theorem}, which provides a way to represent an @tech{object}
@math{b : 𝒞} via the @tech{functor} @math{H}, such that
@math{H(b) = ∐@_{a∈𝒞_0}Hom@_{𝒞}(a, b)}.

Building on this idea, the @tech{Yoneda embedding} generalizes
@tech{Cayley's theorem}. The @tech{functor} @math{J} represents an @tech{object}
@math{b : 𝒞} as a @tech{hom functor}, such that @math{J(b) = Hom@_{𝒞}(-, b)}.
Moreover, any @tech{representable functors} @tech{naturally isomorphic} to
@math{Hom@_{𝒞}(-, b)} can also be used to represent @math{b}.

The @tech{Yoneda embedding} has interesting connections to programming paradigms,
such as @deftech{Continuation-Passing Style} (@deftech{CPS}). Specifically, when
applied to @math{𝐒𝐞𝐭}, the @tech{Yoneda embedding} @math{J} maps a @tech{set}
@math{x} to the @tech{endofunctor} @math{Hom@_{𝐒𝐞𝐭}(-, x)}, which encapsulates
computations that use @tech{functions} as @deftech{continuation}s to produce
results in @math{x}.

To illustrate this, consider a result type @math{x}. In Typed Racket, the type of
@math{J(x)} can be expressed as: @racket[(∀ (a b) (→ (→ b a) (→ (→ a x) (→ b x))))].
By uncurrying it, we obtain a @tech{procedure} @math{cps}:

@racketblock[
(: cps (∀ (a b) (→ (→ b a) (→ b (→ a x) x))))
(define (cps i) (λ (m k) (k (i m))))
]

This type signature shows that @math{cps} maps a @tech{procedure} @math{i} of type
@racket[(→ b a)] to a @tech{procedure} @math{cps(i)} of type @racket[(→ b (→ a x) x)].
In other words, instead of returning a result of type @math{a} directly like @math{i},
@math{cps(i)} requires an additional argument to indicate the @tech{continuation}
of type @racket[(→ a x)] that specifies what to do with the result once it is
produced. In many programming languages, the keyword @deftech{return} serves as a
form of @tech{continuation}, directing where to proceed after a result is obtained.

@subsection{Universal Element}

In our earlier discussion of the @tech{Yoneda Lemma}, we highlighted how
@tech{category theory} views a @tech{morphism} in terms of its relationships with
other @tech{morphisms}. This was illustrated by the one-to-one correspondence
between @math{f}, @math{Hom@_{𝒞}(f, -)}, and @math{Hom@_{𝒞}(-, f)}.

The @tech{Yoneda Lemma} showed that this correspondence holds when we substitute
@math{F} for a @tech{hom functor}. But what if @math{F} is a more general
@tech{representable functor}? This naturally leads to the question: What special
property does the @tech{elements} corresponding to the @tech{natural isomorphisms}
have in such cases? The answer is the @tech{universal property}, which is
reflected by the concept of @tech{universal elements}.

A @deftech{universal element} of a @tech{functor} @math{F : 𝒞 → 𝐒𝐞𝐭} is an
@tech{element} @math{s ∈ F(S)} for some @tech{object} @math{S : 𝒞} such that,
for any other @tech{object} @math{T : 𝒞} and @tech{element} @math{t ∈ F(T)},
there exists a unique @tech{morphism} @math{w : S → T : 𝒞} for which
@math{t = F(w)(s)}. This reflects the general form of a
@deftech{universal property}, which is typically described as follows:
@math{∀T ∈ 𝒞_0, ∃!w ∈ Hom@_{𝒞}(S, T), t = F(w)(s)}.

@bold{Exercise}: Prove that an @tech{object} @math{S : 𝒞} @tech{represents}
a @tech{functor} @math{F : 𝒞 → 𝐒𝐞𝐭} iff there exists a @tech{universal element}
@math{s ∈ F(S)}.

@bold{Exercise}: Prove that @math{s} is a @tech{universal element} of @math{F}
iff the @tech{natural transformation} @math{ρ : Hom@_{𝒞}(S, -) ⇒ F} corresponding
to @math{s} is a @tech{natural isomorphism}, i.e., each @tech{component} of
@math{ρ} at an @tech{object} @math{T : 𝒞} is a @tech{bijection}:
@math{ρ(T)@^{–1}(t) = w}.

@bold{Exercise}: Prove that if @math{s ∈ F(S)} and @math{t ∈ F(T)} both are
@tech{universal elements} of @math{F}, then there is a unique @tech{isomorphism}
between @math{S} and @math{T} in @math{𝒞}.

The concept of @tech{universal elements} mirrors the @tech{universal property}
seen in @tech{initial objects}, where there exists a unique @tech{morphism}
@math{f} from an @tech{initial object} @tech{0} to any other @tech{object}
@math{a} in the same @tech{category} @math{𝒞}:
@math{∀a ∈ 𝒞_0, ∃!f ∈ 𝒞_1, dom@_{𝒞}(f) = 0 ∧ cod@_{𝒞}(f) = a}.

In fact, any @tech{universal property} can be viewed as an instance of an
@tech{initial object} in some @tech{category} (usually the @tech{comma category}).
For example, @tech{universal elements} of a @tech{representable functor}
@math{F : 𝒞 → 𝐒𝐞𝐭} can be seen as @tech{initial objects} in @math{∫@^{T:𝒞}F(T)}.

@bold{Exercise}: Prove that @math{F} is a @tech{representable functor} iff there
exists an @tech{initial object} in @math{∫@^{𝒞}F}.

@bold{Exercise}: Prove that the @tech{state diagram} of a @tech{TDFA} is a
@tech{tree} whose @tech{root} is the @tech{start state} @math{s_0} iff
@math{s_0} is a @tech{universal element} of the @tech{typed action}.

@bold{Exercise}: For a @tech{category} @math{𝒞} and an @tech{object} @math{a : 𝒞}.
Prove @math{∫@^{x:𝒞}Hom@_{𝒞}(a, x) ≅ a/𝒞}.

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
@tech{1-category}, each @tech{0-cell} is an @deftech{identity 1-cell}, in a
@tech{2-category}, each @tech{1-cell} is an @deftech{identity 2-cell}.

We already have an example of such a structure: @tech{𝐂𝐚𝐭}. In @tech{𝐂𝐚𝐭},
@tech{categories} serve as @tech{0-cells}, @tech{functors} act as @tech{1-cells},
and @tech{natural transformations} provide the additional layer of abstraction
as @tech{2-cells}. This makes @tech{𝐂𝐚𝐭} a natural reference for understanding
the concept of @tech{2-categories}.

To formalize this idea, we look at how @tech{𝐂𝐚𝐭} operates. There are two distinct
@tech{composition} operations for @tech{natural transformations} within @tech{𝐂𝐚𝐭}:
@tech{horizontal composition} and @tech{vertical composition}. The interaction
between these two forms of @tech{composition} follows the @tech{interchange law}.
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

Having introduced the concept of @tech{2-categories}, we naturally consider the
mappings between @tech{2-categories}. Just as @deftech{1-functors} map between
@tech{1-categories} by preserving their structure, @tech{2-functors} map between
@tech{2-categories}, preserving the richer structure.

To @racket[define] a @tech{2-functor}, we note that a @tech{2-category} @math{𝐂}
consists of three @tech{collections}: @math{𝐂_0}, @math{𝐂_1} and @math{𝐂_2}.
Consequently, a @deftech{2-functor} @math{F : 𝐂 → 𝐃} consists of three @tech{functions}:
@math{F_0 : 𝐂_0 → 𝐃_0}, @math{F_1 : 𝐂_1 → 𝐃_1}, and @math{F_2 : 𝐂_2 → 𝐃_2}.
Additionally, @math{F} can be described in terms of three @tech{1-functors}:

@itemlist[
  @item{The @deftech{base functor} @math{F^b : 𝐂^b → 𝐃^b}:
        @math{F^b_0 = F_0} and @math{F^b_1 = F_1}.}
  @item{The @deftech{horizontal functor} @math{F^h : 𝐂^h → 𝐃^h}:
        @math{F^h_0 = F_0} and @math{F^h_1 = F_2}.}
  @item{The @deftech{vertical functor} @math{F^v : 𝐂^v → 𝐃^v}:
        @math{F^v_0 = F_1} and @math{F^v_1 = F_2}.}
]

@subsection{Strict Monoidal Category}

A @deftech{strict monoidal category} @math{(𝒞, ⊗, I)} is a @tech{category}
@math{𝒞} equipped with a @deftech{tensor product} @deftech{⊗} and a
@deftech{tensor unit} @math{I}. The @tech{tensor product} is a @tech{functor}
@math{⊗ : 𝒞×𝒞 → 𝒞}, and the @tech{tensor unit} is a @deftech{unit object}
@math{I : 𝒞}, such that for all @tech{morphisms} @math{f, g, h} in @math{𝒞},
@math{(f⊗g)⊗h = f⊗(g⊗h)} and @math{f = f⊗id_I = id_I⊗f}.

@image["scribblings/natural_transformation/images/mon-cat.svg"]{[picture] mon-cat.svg}

@bold{Exercise}: Prove the @tech{interchange law}:
@math{(g_0⊗g_1)∘(f_0⊗f_1) = (g_0∘f_0)⊗(g_1∘f_1)}.

The following is an example of a @tech{strict monoidal category}:

@racketfile{code/natural_transformation/Matr.rkt}

If @math{𝒞} is a @tech{discrete category}, i.e., a @tech{set}, then the
@tech{strict monoidal category} @math{(𝒞, ⊗, I)} reduces to a @deftech{monoidal set}.
In this case, @math{⊗} becomes an @tech{associative binary operation} and @math{I}
becomes the @tech{identity element} of @math{𝒞}. This structure corresponds
exactly to what we call a @tech{monoid}. Hence, @tech{monoidal set} and
@tech{monoid} are the same concept.

Just as a @tech{one-object category} @math{𝒞} can be viewed as a @tech{monoid}
@math{(𝒞_1, ∘, id@_{∗})}, we extend this idea to view a @deftech{one-object 2-category}
@math{𝐂} as a @tech{strict monoidal category} @math{(𝐂^v, ∘, id^b@_{∗})}.
In this context, the @tech{vertical category} @math{𝐂^v} is equipped with the
@tech{horizontal composition} @math{∘}, which acts as the @tech{tensor product},
and the @tech{identity} @tech{1-cell} @math{id^b@_{∗}}, which serves as the
@tech{tensor unit}.

@bold{Exercise}: Show that every @tech{endofunctor category} is a
@tech{strict monoidal category}.

@subsubsection{Strict Symmetric Monoidal Category}

A @deftech{strict symmetric monoidal category} @math{(𝒞, ⊗, I)} is a
@tech{strict monoidal category} that @tech{⊗} is @tech{symmetric}: for all
@tech{morphisms} @math{f, g} in @math{𝒞}, @math{f⊗g = g⊗f}.

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

@image["scribblings/natural_transformation/images/alpha.svg"]{[picture] alpha.svg}

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

@image["scribblings/natural_transformation/images/alpha_0.svg"]{[picture] alpha_0.svg}
@image["scribblings/natural_transformation/images/alpha_1.svg"]{[picture] alpha_1.svg}
@image["scribblings/natural_transformation/images/alpha_2.svg"]{[picture] alpha_2.svg}
@image["scribblings/natural_transformation/images/alpha_3.svg"]{[picture] alpha_3.svg}
@image["scribblings/natural_transformation/images/alpha_4.svg"]{[picture] alpha_4.svg}

The second one shows two @tech{2-cells} @math{α : G∘F ⇒ id@_{𝒞}} and
@math{β : id@_{𝒟} ⇒ H∘G}, where @math{F : 𝒞 → 𝒟 : 𝐂}, @math{G : 𝒟 → 𝒞 : 𝐂},
and @math{H : 𝒞 → 𝒟 : 𝐂}:

@image["scribblings/natural_transformation/images/beta&alpha_0.svg"]{[picture] beta&alpha_0.svg}
@image["scribblings/natural_transformation/images/beta&alpha_1.svg"]{[picture] beta&alpha_1.svg}
@image["scribblings/natural_transformation/images/beta&alpha_2.svg"]{[picture] beta&alpha_2.svg}
@image["scribblings/natural_transformation/images/beta&alpha_3.svg"]{[picture] beta&alpha_3.svg}
@image["scribblings/natural_transformation/images/beta&alpha_4.svg"]{[picture] beta&alpha_4.svg}

The advantage of using @tech{string diagrams} lies in their simplicity when
representing complex structures in a @tech{2-category}. Instead of working with
layers of @tech{nodes} and @tech{arrows}, @tech{string diagrams} allow us to
represent these relationships in a clear, visual manner that highlights how each
part of the structure interacts with the others.

@bold{Exercise}: The following is a @tech{string diagram}, try to draw the
corresponding @tech{diagram}.

@image["scribblings/natural_transformation/images/str-diag.svg"]{[picture] str-diag.svg}

If there are no @tech{2-cells} in a @tech{string diagram}, we can further
compress it for simplicity. Specifically:

@itemlist[
  @item{@tech{0-cell} is represented by a line segment.}
  @item{@tech{1-cell} is represented by a point separating the line in two.}
]

This compressed representation is not limited to @math{𝐂^b} but can also be
extended to other @tech{1-categories}.

@subsection{Equivalence}

In a @tech{2-category} @math{𝐂}, @tech{equivalence} is a weaker version of
@tech{isomorphism}. For @tech{1-cells} @math{F: 𝒞 → 𝒟 : 𝐂} and @math{G: 𝒟 → 𝒞 : 𝐂},
if @math{id@_{𝒞} ≅ G∘F} and @math{F∘G ≅ id@_{𝒟}}, then @math{F} and @math{G} are
both @deftech{equivalence}s (often called be @deftech{weakly invertible}).

@image["scribblings/natural_transformation/images/eqv_1.svg"]{[picture] eqv_1.svg}
@image["scribblings/natural_transformation/images/eqv_2.svg"]{[picture] eqv_2.svg}

In this case, both @math{F} and @math{G} are @tech{inverses} up to
@tech{2-isomorphisms} @math{η : id@_{𝒞} ⇒ G∘F} and @math{ϵ : F∘G ⇒ id@_{𝒟}}.
@math{G} is a @deftech{pseudo-inverse} of @math{F}, and @math{F} is a
@tech{pseudo-inverse} of @math{G}. @math{𝒞} and @math{𝒟} are @deftech{equivalent}
to each other (@math{𝒞 @deftech{≃} 𝒟}) if there exists an @tech{equivalence}
between them.

@bold{Exercise}: Prove that if @math{η} and @math{ϵ} are @tech{identities}, then
@math{𝒞 ≅ 𝒟}.

@bold{Exercise}: Prove that @tech{≃} is an @tech{equivalence relation} over
@math{𝐂_0}.

@bold{Exercise}: Prove that every @tech{0-cell} is @tech{equivalent} to itself.

@bold{Exercise}: Prove that the @tech{pseudo-inverse} of an @tech{equivalence} is
not unique.

@bold{Exercise}: Prove that @tech{𝐓𝐫𝐞𝐞} is @tech{equivalent} to @tech{𝐅𝐬𝐭}.

@;; @section{Enriched Category}

@;; @subsection{Enrich Over}
