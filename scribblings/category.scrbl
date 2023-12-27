#lang scribble/manual

@(require (for-label racket/base
                     racket/match
                     math/matrix)
          "ctp-utils.rkt")

@title[#:tag "Cat"]{Category}

Welcome to the first chapter of our @secref{CTP} tutorial!
In this chapter, we'll explore the foundational concepts of @tech{category theory}
and demonstrate how to map these abstract ideas into practical constructs using
the Racket programming language.

@section{Definitions}

In the realm of mathematics, @tech{category theory} serves as a powerful and
abstract tool for understanding relationships and compositions within various
mathematical structures. At its core, a @tech{category} consists of @tech{objects}
and @tech{morphisms}, forming a conceptual framework that generalizes the notion
of a mathematical structure.

@subsection{Category}

Think of a @deftech{category} @math{𝒞} as a directed graph, where @deftech{objects}
are nodes, and @deftech{morphisms} are arrows connecting these nodes. The key
distinguishing features of a @tech{category} are the @deftech{identity morphisms}
and the @deftech{composition} of @tech{morphisms}, governed by @deftech{composition rules}:

@image["assets/images/intro-cat.svg"]

@itemlist[
  #:style 'ordered
  @item{Existence of @tech{composition}

        For @tech{morphisms} @math{f} and @math{g} in @math{𝒞}, @math{g∘f} is defined
        if and only if @math{(g, f)} is a @deftech{composable pair}: @math{cod(f) = dom(g)}.
        When @math{g∘f} is defined, @math{dom(g∘f) = dom(f)} and @math{cod(g∘f) = cod(g)}.

        @image["assets/images/C-1.svg"]}
  @item{Associativity of @tech{composition}

        For @tech{composable pairs} @math{(g, f)} and @math{(h, g)} in @math{𝒞},
        @math{(h∘g)∘f = h∘(g∘f)}, denoted as @math{h∘g∘f}.

        @image["assets/images/C-2.svg"]}
  @item{Existence of @tech{identity morphisms}

        Every @tech{object} has an associated @tech{identity morphism}.
        For @tech{object} @math{a} in @math{𝒞}, its @tech{identity morphism}
        is denoted as @math{id.a}, and @math{a = dom(id.a) = cod(id.a)}.

        @image["assets/images/C-3.svg"]}
  @item{@tech{Composition} and @tech{identity morphisms}

        For @tech{morphism} @math{f: a -> b} in @math{𝒞}, @math{f = f∘id.a = id.b∘f}.

        @image["assets/images/C-4.svg"]}
  ]

@margin-note{
In traditional @tech{category theory}, @tech{categories} are often named after
their @tech{objects}. In this tutorial, we intentionally use @tech{morphisms} to
label @tech{categories}. This approach emphasizes the role of @tech{morphisms}
as the central focus. Nevertheless, classic @tech{category} names will still be
given precedence and used whenever applicable in this tutorial.
}

In traditional @tech{category theory}, a distinction is often made between
@tech{objects} and @tech{identity morphisms}. However, in this tutorial, we adopt
a unique perspective by using @tech{identity morphisms} to directly represent @tech{objects}.
There is no strict demarcation between @tech{objects} and their associated
@tech{identity morphisms}; they are treated interchangeably (@math{a = id.a}).

One common misconception among beginners in @tech{category theory} is the assumption
that @tech{morphisms} must represent traditional mappings, transformations, or
relations. In reality, @tech{morphisms} in @tech{category theory} can be incredibly
diverse. They can represent not only traditional mappings but also entities like
@tech[#:doc rkt-scrbl]{numbers}, @tech[#:doc rkt-scrbl]{strings}, or
@tech[#:doc rkt-scrbl]{lists}, as long as they adhere to the @tech{composition rules}.

@subsection{Commutative Diagram}

Informally, a @deftech{diagram} comprises various @tech{objects} connected by
various @tech{morphisms}. When the @tech{morphisms} with the same @tech{domain}
and @tech{codomain} are the same one, the @tech{diagram} is a @deftech{commutative diagram}.

@tech{Commutative diagrams} serve as a powerful language for expressing equations.

@image["assets/images/intro-comm-diag.svg"]

@subsection{Semicategory (Non-Unital Category)}

@margin-note{
See more in @hyperlink["https://ncatlab.org/nlab/show/semicategory"]{nLab}.
}

@deftech{Semicategories} (@deftech{non-unital categories}) are similar to
@tech{categories} but omit the @tech{identity morphism} requirement.

@subsection{One-Object Category (OOC)}

@margin-note{
A @deftech{monoid} (@deftech{monoidal set}) @math{(S, *, e)} is a @tech{set}
@math{S} equipped with an associative binary operation @math{*} and an
@deftech{identity element} @math{e}.
}

A @deftech{one-object category} (@deftech{OOC}) can be viewed as a @tech{monoid}.
In @tech{OOC}, there is only a single @tech{object}, usually denoted by @deftech{*},
and @tech{morphisms} are defined within the context of @tech{*}.

@image["assets/images/intro-ooc.svg"]

@margin-note{
@tech{categories} are sometimes called @deftech{monoidoids}.
}

The @tech{monoid} structure becomes evident when we consider the @tech{identity morphism}
as the @tech{monoid} @tech{identity element} and the @tech{composition} operation
as the @tech{monoid} operation. Thus, @tech{OOCs} provide a categorical perspective
on @tech{monoids}.

@section{Mapping Category to Programming}

In this section, we'll explore how @tech{category theory} concepts can be mapped to
practical programming constructs.

Just as @racket[car], @racket[cdr], and @racket[cons] provide an abstraction for
@tech[#:doc rkt-scrbl]{pairs} in Racket, we'll introduce the notions of
@deftech{dom}, @deftech{cod}, and @deftech{∘}
(representing @deftech{domain}, @deftech{codomain}, and @deftech{compose})
to abstract over @tech{categories}.

We stipulate that @code{(∘ m)} returns @code{m} and @code{(∘)} returns @tech{*}
in Racket.

@subsection{Category Examples}

Let's see how these abstractions can be applied to create and manipulate
@tech{categories} in the context of programming.

@subsubsection{Natural Number Category}

The @tech{category} of natural @tech[#:doc rkt-scrbl]{numbers} is an example of
@tech{OOC}. In this case, @tech{morphisms} are natural @tech[#:doc rkt-scrbl]{numbers},
and the single @tech{object} @tech{*} is @code{0} (as the @tech{identity morphism}):

@racketblock[
(code:comment2 "Category of Natural Numbers")
(define (dom m) *)
(define (cod m) *)
(define (∘ . m*) (apply + m*))

(define (morphism=? m . m*) (apply = m m*))

(code:comment2 "Objects")
(define * (∘))

(code:comment2 "Morphisms")
(define f 1)
(define g 2)
(define h 3)

(code:comment2 "Existence of composition")
(morphism=? (cod f) (dom g))
(morphism=? (dom (∘ g f)) (dom f))
(morphism=? (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? * (dom *) (cod *))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@subsubsection{List Category}

The @tech{category} of @tech[#:doc rkt-scrbl]{lists} is also an @tech{OOC},
where @tech{*} is @racket[null] and morphisms are @tech[#:doc rkt-scrbl]{lists}:

@racketblock[
(code:comment2 "Category of Lists")
(define (dom m) *)
(define (cod m) *)
(define (∘ . m*) (apply append m*))

(define morphism=?
  (case-lambda
    [(m) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

(code:comment2 "Objects")
(define * (∘))

(code:comment2 "Morphisms")
(define f '(1 2 3))
(define g '(a b c))
(define h '(A B C))

(code:comment2 "Existence of composition")
(morphism=? (cod f) (dom g))
(morphism=? (dom (∘ g f)) (dom f))
(morphism=? (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? * (dom *) (cod *))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@subsubsection{String Category}

As an exercise, consider the @tech{category} of @tech[#:doc rkt-scrbl]{strings},
which is also an @tech{OOC}. Here's a skeleton code for the @tech{category} and
your task is to complete the implementation:

@racketblock[
(code:comment2 "Category of Strings")
(define (dom m) *)
(define (cod m) *)
(define (∘ . m*) ???)

(define (morphism=? m . m*) (apply string=? m m*))

(code:comment2 "Objects")
(define * (∘))

(code:comment2 "Morphisms")
(define f "123")
(define g "abc")
(define h "ABC")

(code:comment2 "Existence of composition")
(morphism=? (cod f) (dom g))
(morphism=? (dom (∘ g f)) (dom f))
(morphism=? (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? * (dom *) (cod *))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]


@subsubsection{Matrix Category}

The @tech{category} of @tech[#:doc math-scrbl #:key "matrix"]{matrices} is a
fascinating example that brings together linear algebra and @tech{category theory}.
In this @tech{category}, each @math{m × n} @tech[#:doc math-scrbl]{matrix} is
considered a @tech{morphism}, its @tech{domain} is the n-order identity @tech[#:doc math-scrbl]{matrix},
and its @tech{codomain} is the m-order identity @tech[#:doc math-scrbl]{matrix}:

@racketblock[
(require math/matrix)

(code:comment2 "Category of Matrics")
(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))

(define morphism=?
  (case-lambda
    [(m) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

(code:comment2 "Objects")
(define a (identity-matrix 1))
(define b (identity-matrix 2))
(define c (identity-matrix 3))
(define d (identity-matrix 4))

(code:comment2 "Morphisms")
(define (proc m n) (random 1 9))
(define f (build-matrix 2 1 proc))
(define g (build-matrix 3 2 proc))
(define h (build-matrix 4 3 proc))

(code:comment2 "Existence of composition")
(morphism=? (cod f) (dom g))
(morphism=? (dom (∘ g f)) (dom f))
(morphism=? (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@subsubsection{Binary Relation Category}

@margin-note{
A @deftech{preordered set} @math{(S, <=)} is a @tech{set} @math{S} equipped with a
binary relation @math{<=} that is reflexive and transitive.
}

A @tech{preordered set} @math{(S, <=)} can be viewed as a @tech{category} where
@tech{morphisms} are binary relations on its underlying @tech{set} @math{S}:

@racketblock[
(code:comment2 "Category of Binary Relations")
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))

(define morphism=?
  (case-lambda
    [(m) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

(code:comment2 "Objects")
(define a '(a . a))
(define b '(b . b))
(define c '(c . c))
(define d '(d . d))

(code:comment2 "Morphisms")
(define f '(a . b))
(define g '(b . c))
(define h '(c . d))

(code:comment2 "Existence of composition")
(morphism=? (cod f) (dom g))
(morphism=? (dom (∘ g f)) (dom f))
(morphism=? (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@margin-note{
A @deftech{partially ordered set} (@deftech{poset}) is a @tech{preordered set}
@math{(S, <=)} for which @math{<=} is antisymmetric.
}

A @tech{poset} can also be viewed as a @tech{category}. It is left as an exercise
for further exploration.

@subsubsection{Procedure Category}

The @tech{category} of procedures is perhaps the most important @tech{category}
in programming. As the name suggests, this @tech{category} has procedures
(also known as functions in functional programming) as its @tech{morphisms},
so it resembles the @tech{category} of @tech{sets}
(with mathematical @tech{functions} as @tech{morphisms}) in category theory.

However, this @tech{category} is not a strict @tech{category} that follows the
@tech{composition rules}, unlike the examples we introduced above. It has some defects.

From the computing science point of view, @tech{category theory} is a strongly
typed language, stronger than any programming language. This is because of the
@tech{composition rule}: @math{g∘f} exists if and only if @math{cod(f) = dom(g)}.
Racket is an untyped language, it allows any procedure to be composed, such as
@code{(compose car +)}, but such a procedure will only report an error when applied.
Therefore, this @tech{category} can be regarded as an @tech{OOC}:

@racketblock[
(define (dom m) (∘))
(define (cod m) (∘))
(define ∘ compose)
]

Another defect is that we cannot compare whether two procedures have the same
functionality, which means we cannot define @code{morphism=?}, and have to rely on
the programmer to judge whether the behavior of two procedures is same. For Racket,
it cannot even be sure that @math{g∘f = g∘f} !

@subsection{Constructions on Categories}

@subsubsection{Dual Category}

A @tech{category} can be viewed as a directed graph that adheres to the
@tech{composition rules}. If we reverse all the arrows in the directed graph,
the resulting new directed graph still adheres to the @tech{composition rules},
so this new directed graph is also a @tech{category}. The new @tech{category} is
the @deftech{dual} of the original @tech{category}.

@image["assets/images/intro-¬cat.svg"]

@racketblock[
(define (¬dom m) (cod m))
(define (¬cod m) (dom m))
(define (¬∘ . m*) (apply ∘ (reverse m*)))
]

@subsubsection{Subcategory}

@subsection{Special Morphisms}

@subsubsection{Monic and Epic}

@subsubsection{Split}

@subsubsection{Iso}
