#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0 (except-in typed/racket/base/no-check =))
                     math/matrix)
          "../ctp-utils.rkt")

@title[#:tag "_Functor_"]{Functor}

In this chapter, we introduce the concept of @tech{functors}. @tech{Functors}
play a crucial role in @tech{category theory} by mapping @tech{objects} and
@tech{morphisms} between @tech{categories} while preserving their structural
properties. To enhance readability and provide a clear understanding of how
@tech{functors} can be applied in programming, we will use the syntax of Typed
Racket.

Specifically, we will use @code{#lang typed/racket/base/no-check} instead of
@code{#lang racket/base}. This choice allows us to leverage the benefits of
Typed Racket, such as type annotations, which improve code clarity and help
illustrate the functorial properties more effectively.

@local-table-of-contents[]

@section{Functor}

Just as @tech{functions} map @tech{elements} between @tech{sets}, @tech{functors}
provide a way to map @tech{objects} and @tech{morphisms} between @tech{categories}.
This mapping preserves the structural aspects of @tech{categories}.

@margin-note{
For convenience, if @math{a} is an @tech{object} in @math{𝒞}, @math{F(a) = F_0(a)};
if @math{f} is a @tech{morphism} in @math{𝒞}, @math{F(f) = F_1(f)}.
}

Similar to how a @tech{category} @math{𝒞} is defined by two collections @math{𝒞_0}
and @math{𝒞_1}, a @deftech{functor} @math{F: 𝒞 → 𝒟} is also defined by two
@tech{functions} @math{F_0: 𝒞_0 → 𝒟_0} and @math{F_1: 𝒞_1 → 𝒟_1}. These
@tech{functions} must satisfy the following properties:

@itemlist[
  #:style 'ordered
  @item{Preservation of @tech{domain} and @tech{codomain}

        For any @tech{morphism} @math{f: a → b} in @math{𝒞}, there is a @tech{morphism}
        @math{F(f): F(a) → F(b)} in @math{𝒟}.

        @image["scribblings/functor/images/F-1.svg"]{[picture] F-1.svg}}
  @item{Preservation of @tech{identity morphisms}

        For any @tech{object} @math{a} in @math{𝒞}, @math{F(id_a) = id@_{F(a)}}.

        @image["scribblings/functor/images/F-2.svg"]{[picture] F-2.svg}}
  @item{Preservation of @tech{composition}

        If @math{(f, g)} is a @tech{composable pair} in @math{𝒞}, then @math{(F(f), F(g))}
        is a @tech{composable pair} in @math{𝒟}, and @math{F(g∘f) = F(g)∘F(f)}.

        @image["scribblings/functor/images/F-3.svg"]{[picture] F-3.svg}}
  ]

The following example illustrates how to implement @tech{functors} in Racket:

@racketfile{functor/code/𝐌𝐚𝐭𝐫→𝐑𝐞𝐥.rkt}

@bold{Exercise}: Prove that @tech{functors} can be composed and that this
@tech{composition} is associative.
