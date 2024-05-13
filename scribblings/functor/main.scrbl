#lang scribble/manual

@(require (for-label (only-meta-in 0 typed/racket/base/no-check))
          "../ctp-utils.rkt")

@title[#:tag "_Functor_"]{Functor}

@;; 本章节我们介绍函子，使用过程范畴表示函数范畴和范畴范畴。为了提升可读性，会使用 typed/racket 的语法形式。

@local-table-of-contents[]

@section{Functor}

Just as @tech{functions} map @tech{elements} between @tech{sets}, @tech{functors}
provide a way to map @tech{objects} and @tech{morphisms} between @tech{categories}.
This mapping preserves the structural aspects of @tech{categories}.

@margin-note{
For convenience, if @math{a} is an @tech{object} in @math{𝒞}, @math{F(a) = F_0(a)};
if @math{f} is a @tech{morphism} in @math{𝒞}, @math{F(f) = F_1(f)}.
}

A @tech{category} @math{𝒞} is defined by 2 collections @math{𝒞_0} and @math{𝒞_1},
a @deftech{functor} @math{F: 𝒞 → 𝒟} is similarly defined by 2 @tech{functions}
@math{F_0: 𝒞_0 → 𝒟_0} and @math{F_1: 𝒞_1 → 𝒟_1} for which

@itemlist[
  #:style 'ordered
  @item{Preservation of @tech{domain} and @tech{codomain}

        For any @tech{morphism} @math{f: a → b} of @math{𝒞}, there is a @tech{morphism}
        @math{F(f): F(a) → F(b)} in @math{𝒟}.

        @image["scribblings/functor/images/F-1.svg"]{[picture] F-1.svg}}
  @item{Preservation of @tech{identity morphisms}

        For any @tech{object} @math{a} of @math{𝒞}, @math{F(id_a) = id@_{F(a)}}.

        @image["scribblings/functor/images/F-2.svg"]{[picture] F-2.svg}}
  @item{Preservation of @tech{composition}

        if @math{(f, g)} is a @tech{composable pair} in @math{𝒞}, then @math{(F(f), F(g))}
        is a @tech{composable pair} in @math{𝒟}, and @math{F(g∘f) = F(g)∘F(f)}.

        @image["scribblings/functor/images/F-3.svg"]{[picture] F-3.svg}}
  ]

@racketfile{functor/code/ℳ→ℛ.rkt}

@bold{Exercise}: Prove the associativity law of @tech{functor} @tech{composition}.
