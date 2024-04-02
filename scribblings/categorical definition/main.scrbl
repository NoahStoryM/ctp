#lang scribble/manual

@(require (for-label racket/base
                     math/matrix)
          "../ctp-utils.rkt")

@title{Categorical Definition}

In this chapter, we explore the fundamental idea of defining properties a
@tech{category} may have solely through @tech{objects} and @tech{morphisms} in it.
This approach, known as the @deftech{categorical definition}, allows us to capture
and express important concepts using the language of @tech{category theory}.

Let's embark on a journey to uncover the beauty and simplicity that arise when
we define properties categorically.

@section{Endomorphism}

For a @tech{morphism} @math{f}, it is an @deftech{endomorphism} if and only if
@math{dom(f) = cod(f)}.

@image["scribblings/categorical definition/images/endo.svg"]

@subsection{Idempotent}

For an @tech{endomorphism} @math{f}, it is an @deftech{idempotent} if and only if
@math{f = f∘f}.

@image["scribblings/categorical definition/images/idem.svg"]

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

@image["scribblings/categorical definition/images/split.svg"]

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

@image["scribblings/categorical definition/images/iso.svg"]

In this case, @math{g} is the @deftech{inverse} of @math{f}, denoted as @math{f^{–1}},
and @math{f} is the @tech{inverse} of @math{g}, denoted as @math{g^{–1}}.
@math{a} and @math{b} are @deftech{isomorphic} to each other (@math{a ≅ b}) if and
only if there exists an @tech{isomorphism} between them.

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

@subsection{Automorphism}

An @deftech{automorphism} is an @tech{invertible} @tech{endomorphism}.

@image["scribblings/categorical definition/images/auto.svg"]

@subsection{Groupoid}

A @deftech{groupoid} is equivalently a @tech{category} in which all @tech{morphisms}
are @tech{isomorphisms}.

@subsubsection{One-Object Groupoid}

@margin-note{
A @deftech{group} is a @tech{monoid} in which every @tech{element} has a unique
@tech{inverse}.
}

A @deftech{one-object groupoid} (@deftech{OOG}) can be viewed as a @tech{group}.

@subsection{Representative Subcategory}

A @deftech{representative subcategory} is a @tech{subcategory} @math{𝒟} of a
@tech{category} @math{𝒞} that every @tech{object} of @math{𝒞} is @tech{isomorphic}
to some @tech{object} of @math{𝒟}.

@section{Monomorphism and Epimorphism}

variable element

@bold{Exercise}: proof that split epi and mono is iso.

@bold{Exercise}: proof that split mono and epi is iso.

@section{Terminal Object and Initial Object}

@subsection{Global Element}
