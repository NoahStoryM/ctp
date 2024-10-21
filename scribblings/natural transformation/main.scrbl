#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0
                                   (except-in typed/racket/base/no-check
                                              =
                                              require/typed
                                              require/typed/provide)
                                   (only-in typed/racket/base
                                            require/typed
                                            require/typed/provide))
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

@bold{Exercise}: For a @tech{morphism} @math{i : b → a : 𝒞}. Prove that
@math{Hom@_{𝒞}(i, -)} is a @tech{natural transformation} from
@math{Hom@_{𝒞}(a, -)} to @math{Hom@_{𝒞}(b, -)}.

@image["scribblings/natural transformation/images/Hom_1.svg"]{[picture] Hom_1.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: b 𝒞) (: a 𝒞) (: i (→𝒞 b a))
(: |(→𝒞 i _)| (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (|(→𝒞 i _)| j)
  (define |(→𝒞 i j)| (λ (f) (∘𝒞 j f i)))
  |(→𝒞 i j)|)
]

@bold{Exercise}: For a @tech{morphism} @math{j : x → y : 𝒞}. Prove that
@math{Hom@_{𝒞}(-, j)} is a @tech{natural transformation} from
@math{Hom@_{𝒞}(-, x)} to @math{Hom@_{𝒞}(-, y)}.

@image["scribblings/natural transformation/images/Hom_2.svg"]{[picture] Hom_2.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: x 𝒞) (: y 𝒞) (: j (→𝒞 x y))
(: |(→𝒞 _ j)| (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (|(→𝒞 _ j)| i)
  (define |(→𝒞 i j)| (λ (f) (∘𝒞 j f i)))
  |(→𝒞 i j)|)
]

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
@tech{base category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{b}}.

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

The @tech{horizontal category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{h}},
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

The @tech{vertical category} of @tech{𝐂𝐚𝐭}, denoted by @deftech{𝐂𝐚𝐭@^{v}},
provides a perspective that focuses on the relationships between @tech{functors}
through @tech{natural transformations}. In @tech{𝐂𝐚𝐭@^{v}}, @tech{objects} are
@tech{functors} between @tech{categories} and @tech{morphisms} are
@tech{natural transformations} between @tech{functors}. An @tech{isomorphism}
@math{α : F ⇒ G} in @tech{𝐂𝐚𝐭@^{v}} is called a @deftech{natural isomorphism},
and @math{F} and @math{G} are @deftech{naturally isomorphic} to each other.

@image["scribblings/natural transformation/images/𝐂𝐚𝐭^v.svg"]{[picture] 𝐂𝐚𝐭^v.svg}

@bold{Exercise}: Prove that a @tech{natural transformation} @math{α : F ⇒ G : 𝒞 → 𝒟}
is a @tech{natural isomorphism} iff each @tech{component} of @math{α} is an
@tech{isomorphism} in @math{𝒟}.

In @tech{𝐂𝐚𝐭@^{v}}, @tech{vertical composition} serves as the @tech{composition}
operation for @tech{morphisms}. This perspective helps us understand why
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

@image["scribblings/natural transformation/images/run_1.svg"]{[picture] run_1.svg}

To better understand the @tech{Yoneda Lemma}, let's revisit the @tech{TFSM}
@math{ℳ} defined in @seclink["Typed_Finite_State_Machine"]. We'll denote the
@tech{path category} of the @tech{typed alphabet} @math{𝒢} as @math{𝒞}, and let
@math{F : 𝒞 → 𝐒𝐞𝐭} represent the @tech{typed action} @math{φ*}. In this context,
consider the @tech{run function}, which takes an input sequence @math{w} and
returns a @tech{final state} @math{F(w)(s_0)} after processing @math{w}, starting
from the @tech{initial state} @math{s_0}.

According to the @tech{Yoneda Lemma}, we see that each @tech{component} of a
@tech{natural transformation} @math{ρ : Hom@_{𝒞}(S, -) ⇒ F} at an @tech{object}
@math{T : 𝒞} can be understood as a @tech{run function} starting from a
@tech{state} @math{s ∈ F(S)}: @math{∀w ∈ Hom@_{𝒞}(S, T), ρ(T)(w) = F(w)(s)}.

To illustrate this correspondence, we'll use Racket code to @racket[define] two
@tech{procedures}: @racket[s->ρ] and @racket[ρ->s]. They demonstrate how an
@tech{element} @math{s} of @math{F(S)} can be transformed into a
@tech{natural transformation} @math{ρ}, and vice versa, providing a concrete way
to visualize the one-to-one correspondence described by the @tech{Yoneda Lemma}.

@racketfile{code/natural transformation/s<->ρ.rkt}

This correspondence also holds for @tech{contravariant hom functors}. There is a
one-to-one correspondence between @tech{elements} of @math{G(T)}, where
@math{T : 𝒞@^{op}} and @math{G : 𝒞@^{op} → 𝐒𝐞𝐭}, and @tech{variable elements} of
@math{G}, parametrized by @math{Hom@_{𝒞}(-, T)}. Specifically, an @tech{element}
@math{t ∈ G(T)} uniquely corresponds to a @tech{natural transformation}
@math{ρ : Hom@_{𝒞}(-, T) ⇒ G}.

@image["scribblings/natural transformation/images/run_2.svg"]{[picture] run_2.svg}

@bold{Exercise}: Try to @racket[define] two @tech{procedures} using Racket code:
@racket[t->ρ] and @racket[ρ->t].
