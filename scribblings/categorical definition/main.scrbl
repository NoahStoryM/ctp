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

@subsection{Idempotent}

For an @tech{endomorphism} @math{f}, it is an @deftech{idempotent} if and only if
@math{f = f∘f}.

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
@math{g∘f = id_a}, then @math{f} is called a @deftech{split monomorphism},
@math{g} is called a @deftech{split epimorphism}, and @math{f∘g} is called a
@deftech{split idempotent}.

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

@subsection{Automorphism}

@subsection{Groupoid}

@subsubsection{Group}

@subsection{Representable Subcategory}

@section{Monomorphism and Epimorphism}

variable element

exercise: proof split epi and mono is iso

exercise: proof split mono and epi is iso

@section{Terminal Object and Initial Object}

@subsection{Global Element}
