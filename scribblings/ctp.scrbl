#lang scribble/manual

@title[#:tag "_CTP_"]{Category Theory in Programming}
@author[@author+email["Noah Ma" "noahstorym@gmail.com"]]


Welcome to @secref{_CTP_}, a journey into the conceptual world where mathematics
meets software development. This tutorial is designed for Racket programmers who
are curious about the underlying structures and principles that shape our understanding
of both mathematical and computational systems.

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

@deftech{Category theory}, a branch of mathematics that deals with abstract
structures and relationships, may seem esoteric at first glance. However, its
principles are deeply intertwined with the concepts and patterns we encounter in
programming. Through this tutorial, we aim to bridge the gap between these two
worlds, offering a unique perspective that enriches the programmer's toolkit with
new ways of thinking, problem-solving, and system design.

In the following chapters, we will explore the core concepts of @tech{category theory}
— @tech{objects}, @tech{morphisms}, @tech{categories}, @tech{functors},
@tech{natural transformations}, @tech{Yoneda Lemma}, @tech{2-categories} &
@tech{2-morphisms}, (@tech[#:key "colimit"]{co})@tech{limits},
@tech{Cartesion closed categories} & @tech{typed lambda}, @tech{adjunctions},
(@tech[#:key "comonad"]{co})@tech{monads}, @tech{monoidal categories}, and more —
and how these can be represented and utilized within the Racket programming
language. The goal is not to exhaustively cover @tech{category theory} or to
transform you into a @tech{category} theorist. Instead, we will focus on mapping
these abstract concepts into programming constructs, providing a foundation that
you, the reader, can build upon and apply in your work.

Why study @tech{category theory} as a programmer? The answer lies in the power of
abstraction and generalization that @tech{category theory} offers. It allows us
to see beyond the specifics of a particular programming language, problem, or
system, revealing the underlying structures that are common across different
domains. This tutorial aims to open the door to this broader perspective,
enriching your approach to programming.

As you embark on this journey, keep in mind that the true value of understanding
@tech{category theory} in the context of programming is not merely in acquiring
new knowledge but in developing a new way of thinking about problems or systems.
We encourage you to approach the material with an open mind and to explore how
the concepts presented here can be applied or extended in your programming
endeavors.

@secref{_CTP_} is an invitation to explore, to question, and to discover. It is a
starting point for a deeper inquiry into the vast and fascinating intersection of
mathematics and programming. We hope this tutorial will inspire you to delve
further into both fields, exploring new ideas and forging connections that will
enhance your work as a programmer.

Let the journey begin.

@table-of-contents[]

@include-section[(file "category/main.scrbl")]
@include-section[(file "functor/main.scrbl")]
@include-section[(file "natural transformation/main.scrbl")]
@;; include-section[(file "co-limit/main.scrbl")]
@;; include-section[(file "sketch/main.scrbl")]
@;; include-section[(file "adjunction/main.scrbl")]
@;; include-section[(file "co-monad/main.scrbl")]
@;; include-section[(file "2-category/main.scrbl")]
@;; include-section[(file "monoidal category/main.scrbl")]
@;; include-section[(file "ccc&λ/main.scrbl")]
@;; include-section[(file "topos/main.scrbl")]
@;; include-section[(file "kan extension/main.scrbl")]

@include-section[(file "appendix.scrbl")]

@index-section{}
