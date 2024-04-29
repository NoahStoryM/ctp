#lang scribble/manual

@(require (for-label racket/base
                     math/matrix)
          "../ctp-utils.rkt")

@title[#:tag "_Category Definition_"]{Categorical Definition}

In this chapter, we explore the fundamental idea of defining properties a
@tech{category} may have solely through @tech{objects} and @tech{morphisms} in it.
This approach, known as the @deftech{categorical definition}, allows us to capture
and express important concepts using the language of @tech{category theory}.

Let's embark on a journey to uncover the beauty and simplicity that arise when
we define properties categorically.

@section{Endomorphism}

For a @tech{morphism} @math{f}, it is an @deftech{endomorphism} iff @math{dom(f) = cod(f)}.

@image["scribblings/categorical definition/images/endo.svg"]{[picture] endo.svg}

@subsection{Idempotent}

For an @tech{endomorphism} @math{f}, it is an @deftech{idempotent} iff @math{f = f∘f}.

@image["scribblings/categorical definition/images/idem.svg"]{[picture] idem.svg}

@section{Split Morphism}

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
(@math{f} is a @deftech{split mono} or @math{f} is @deftech{split monic}),
@math{g} is a @deftech{split epimorphism}
(@math{g} is a @deftech{split epi} or @math{g} is @deftech{split epic}),
and @math{f∘g} is a @deftech{split idempotent}.

@image["scribblings/categorical definition/images/split.svg"]{[picture] split.svg}

In this case, @math{f} is a @deftech{right inverse} of @math{g}, and @math{g} is
a @deftech{left inverse} of @math{f}. @math{a} is called a @deftech{retract} of
@math{b}, @math{f} is called a @deftech{section} of @math{g}, @math{g} is called
a @deftech{cosection} (@deftech{retraction}) of @math{f} or a @tech{retraction}
of @math{b} onto @math{a}.

Examples in @secref["Matrix_Category"]:

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

@section{Isomorphism}

For @tech{morphisms} @math{f: a → b} and @math{g: b → a} in @math{𝒞}, if @math{g∘f = id_a}
and @math{f∘g = id_b}, then @math{f} and @math{g} are both @deftech{isomorphisms}
(@math{f} and @math{g} are both @deftech{isos} or @math{f} and @math{g} are both @deftech{invertible}).

@image["scribblings/categorical definition/images/iso.svg"]{[picture] iso.svg}

In this case, @math{g} is the @deftech{inverse} of @math{f}, denoted as @math{f^{–1}},
and @math{f} is the @tech{inverse} of @math{g}, denoted as @math{g^{–1}}.
@math{a} and @math{b} are @deftech{isomorphic} to each other (@math{a ≅ b}) iff
there exists an @tech{isomorphism} between them.

Examples in @secref["Binary_Relation_Category"]:

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

@bold{Exercise}: prove that for @tech{isomorphism} @math{f}, @math{f = (f^{–1})^{–1}}.

@bold{Exercise}: prove that for @tech{isomorphisms} @math{f} and @math{g},
if @math{cod(f) = dom(g)}, then @math{(g∘f)^{-1} = f^{-1}∘g^{-1}}.

@subsection{Automorphism}

An @deftech{automorphism} is an @tech{invertible} @tech{endomorphism}.

@image["scribblings/categorical definition/images/auto.svg"]{[picture] auto.svg}

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

@subsection{Representative Subcategory}

A @deftech{@deftech{representative} subcategory} is a @tech{subcategory} @math{𝒟}
of a @tech{category} @math{𝒞} that every @tech{object} of @math{𝒞} is @tech{isomorphic}
to some @tech{object} of @math{𝒟}.

@section{Monomorphism and Epimorphism}

A @deftech{monomorphism} @math{m} is defined as a @deftech{left cancellable}
@tech{morphism}, where for all @tech{morphisms} @math{a} and @math{b}, if @math{m∘a = m∘b},
then @math{a = b}.

@bold{Exercise}: Prove that for @tech{monomorphisms} @math{f} and @math{g},
if @math{cod(f) = dom(g)}, then @math{g∘f} is also a @tech{monomorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is a @tech{monomorphism}, then @math{f}
is also a @tech{monomorphism}.

Conversely, an @deftech{epimorphism} @math{e} is defined as a @deftech{right cancellable}
@tech{morphism}, where for all @tech{morphisms} @math{x} and @math{y}, if @math{x∘e = y∘e},
then @math{x = y}.

@bold{Exercise}: Prove that for @tech{epimorphisms} @math{f} and @math{g},
if @math{cod(f) = dom(g)}, then @math{g∘f} is also an @tech{epimorphism}.

@bold{Exercise}: Prove that if @math{g∘f} is an @tech{epimorphism}, then @math{g}
is also an @tech{epimorphism}.

@bold{Exercise}: Prove that a @tech{monomorphism} in @math{𝒞} is an @tech{epimorphism}
in @math{𝒞^op}.

@bold{Exercise}: Prove that a @tech{morphism} is an @tech{isomorphism} iff it is
both a @tech{monomorphism} and a @tech{split epimorphism}
(or both an @tech{epimorphism} and a @tech{split monomorphism}).

@section{Initial Object and Terminal Object}

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

@image["scribblings/categorical definition/images/0→1_1.svg"]{[picture] 0→1_1.svg}
@image["scribblings/categorical definition/images/0→1_2.svg"]{[picture] 0→1_2.svg}

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

@subsection{Global Element}

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

@image["scribblings/categorical definition/images/global-elem_1.svg"]{[picture] global-elem_1.svg}
@image["scribblings/categorical definition/images/global-elem_2.svg"]{[picture] global-elem_2.svg}
@image["scribblings/categorical definition/images/global-elem_3.svg"]{[picture] global-elem_3.svg}

In further exploring the @tech{category} of @tech{sets}, let's consider a
@tech{function} @math{f: A → B}. Traditionally, we apply @math{f} to an @tech{element}
@math{x} in @math{A}, denoted as @math{f(x)}. In @tech{category theory}, we can
express this application using @tech{morphisms}. Let @math{x} be a @tech{global element}
of @math{A}, then the application of @math{f} to @math{x} is represented by the
@tech{composition} of @math{f} with @math{x}, written as @math{f∘x}.

Thus, the notation @math{f(x)} is sometimes used in @tech{category theory} to
denote the @tech{composition} @math{f∘x}, where @math{x} is interpreted as a
@tech{morphism} rather than an @tech{element}. This viewpoint aligns with the
idea that @tech{morphisms} in a @tech{category} can be thought of as @tech{elements},
and an application is a special case of @tech{morphism} @tech{composition}.

This approach to viewing @tech{elements} requires only the presence of a
@tech{terminal object} in the @tech{category}. In this context, the @tech{morphisms}
@math{1 → a} can be seen as the @tech{elements} of the @tech{object} @math{a},
thereby generalizing the concept of @tech{elements} of @tech{sets} in a broader
and more abstract manner.
