#lang scribble/manual

@title[#:tag "CTP"]{Category Theory in Programming}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]

Welcome to the @secref{CTP} tutorial! In programming, applying abstract mathematical
concepts often leads to powerful and elegant solutions. One such area of mathematics
that has found its way into the programming landscape is @tech{category theory}.

@margin-note{
In addition to this tutorial, you may find the following resources helpful for
further exploration of @tech{category theory}:

@itemlist[
  @item{@italic{Category Theory for Computing Science} by @italic{Michael Barr} and @italic{Charles Wells}}
  @item{@italic{Category Theory in Context} by @italic{Emily Riehl}}
  @item{@italic{Category Theory} by @italic{Steve Awodey}}
  @item{@italic{Categories for the Working Mathematician} by @italic{Saunders Mac Lane}}
  @item{@hyperlink["https://ncatlab.org/nlab/show/HomePage"]{nLab}}
  @item{@hyperlink["https://www.youtube.com/@TheCatsters"]{TheCatsters YouTube Channel}}
  ]
}

@deftech{Category theory} offers a formal, abstract framework to understand
mathematical structures and their interrelations. While it might seem esoteric
at first, its principles can significantly enhance our ability to reason about
and design software. This tutorial aims to bridge the gap between
@tech{category theory} and practical programming, using the Racket programming
language as our tool of choice.

Through hands-on examples and clear explanations, we will explore fundamental
concepts like @tech{objects}, @tech{morphisms}, @tech{categories}, @tech{functors},
@tech{natural transformations}, @tech{Yoneda Lemma}, @tech{2-categories} & @tech{2-morphisms},
(co)@tech{limits}, @tech{cartesion closed categories} & @tech{typed lambda},
@tech{adjunctions}, (co)@tech{monads}, @tech{monoidal categories}, and more.
Each concept will be accompanied by Racket code, illustrating how these abstract
ideas find concrete expression in programming.

Whether you're a seasoned Racket programmer or someone new to both Racket and
@tech{category theory}, this tutorial is designed to provide valuable insights
and practical knowledge. Let's embark on a journey to discover the beauty and
applicability of @tech{category theory} in the realm of programming!


@local-table-of-contents[]

@include-section[(file "category/main.scrbl")]
@include-section[(file "categorical definition/main.scrbl")]
@;; include-section[(file "functor/main.scrbl")]
@;; include-section[(file "natural transformations/main.scrbl")]
@;; include-section[(file "n-category/main.scrbl")]
@;; include-section[(file "co-limit/main.scrbl")]
@;; include-section[(file "ccc&λ/main.scrbl")]
@;; include-section[(file "adjunction/main.scrbl")]
@;; include-section[(file "co-monad/main.scrbl")]
@;; include-section[(file "monoidal category/main.scrbl")]
