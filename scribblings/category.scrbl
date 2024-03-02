#lang scribble/manual

@(require (for-label racket/base
                     racket/match
                     math/matrix)
          "ctp-utils.rkt")

@title{Category}

Welcome to the first chapter of our @secref{CTP} tutorial!
In this chapter, we'll explore the foundational concepts of @tech{category theory}
and demonstrate how to map these abstract ideas into practical constructs using
the Racket programming language.

@section{Definitions}

In the realm of mathematics, @tech{category theory} serves as a powerful and
abstract tool for understanding relationships and compositions within various
mathematical structures. At its core, a @deftech{category} @math{𝒞} consists of
a collection @math{𝒞_0} of @deftech{objects} and a collection @math{𝒞_1} of
@deftech{morphisms}, forming a conceptual framework that generalizes the notion
of a mathematical structure.

@subsection{Basic Category}

Think of a @tech{category} @math{𝒞} as a directed graph, where @tech{objects}
are nodes, and @tech{morphisms} are arrows connecting these nodes.

@image["assets/images/cat.svg"]

The key distinguishing features of a @tech{category} are the @deftech{identity morphisms}
and the @deftech{composition} of @tech{morphisms}, governed by @deftech{composition rules}:

@itemlist[
  #:style 'ordered
  @item{Existence of @tech{composition}

        For @tech{morphisms} @math{f} and @math{g} in @math{𝒞}, @math{g∘f} is defined
        if and only if @math{cod(f) = dom(g)}. When @math{g∘f} is defined,
        @math{dom(g∘f) = dom(f)} and @math{cod(g∘f) = cod(g)}.

        @image["assets/images/C-1.svg"]}
  @item{Associativity of @tech{composition}

        @margin-note{
        Note that a @deftech{composable pair} consists of not only a pair of
        @tech{morphisms}, but also the @tech{domain} and @tech{codomain} of them.
        See more in @hyperlink["https://ncatlab.org/nlab/show/composable%20pair"]{nLab}.
        }

        For @tech{composable pairs} @math{(f, g)} and @math{(g, h)} in @math{𝒞},
        @math{(h∘g)∘f = h∘(g∘f)}, denoted as @math{h∘g∘f}.

        @image["assets/images/C-2.svg"]}
  @item{Existence of @tech{identity morphisms}

        Every @tech{object} has an associated @tech{identity morphism}.
        For @tech{object} @math{a} in @math{𝒞}, its @tech{identity morphism}
        is denoted as @math{id_a} or @math{1_a}, and @math{a = dom(id_a) = cod(id_a)}.

        @image["assets/images/C-3.svg"]}
  @item{@tech{Composition} and @tech{identity morphisms}

        For @tech{morphism} @math{f: a → b} in @math{𝒞}, @math{f = f∘id_a = id_b∘f}.

        @image["assets/images/C-4.svg"]}
  ]

@margin-note{
In traditional @tech{category theory}, @tech{categories} are often named after
their @tech{objects}. In this tutorial, we intentionally use @tech{morphisms} to
label @tech{categories}. This approach emphasizes the role of @tech{morphisms}
as the central focus. Nevertheless, classic @tech{category} names will still be
given precedence and used whenever applicable in this tutorial.
}

In traditional @tech{category theory}, a distinction is often made between @tech{objects}
and @tech{identity morphisms}. However, in this tutorial, we adopt a unique
perspective by using @tech{identity morphisms} to directly represent @tech{objects}.
There is no strict demarcation between @tech{objects} and their associated
@tech{identity morphisms}; they are treated interchangeably (@math{a = id_a}).

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

@subsubsection{Commutative Triangle}

A @deftech{commutative triangle} is a @tech{commutative diagram} that has the shape
of a triangle.

The equation @math{h = g∘f} can be pictured as a @tech{commutative triangle}
like this:

@image["assets/images/comm-tri.svg"]{h = g∘f}

@subsubsection{Commutative Square}

A @deftech{commutative square} is a @tech{commutative diagram} that has the shape
of a square.

The equation @math{k∘f = g∘h} can be pictured as a @tech{commutative square}
like this:

@image["assets/images/comm-sqr.svg"]{k∘f = g∘h}

If there is a @tech{morphism} @math{l} making @math{f = l∘h} and @math{g = k∘l},
then @math{l} is a @deftech{lift} (@deftech{diagonal fill-in} or @deftech{filler})
in the @tech{commutative square}:

@image["assets/images/lift_1.svg"]
@image["assets/images/lift_2.svg"]

@subsection{One-Object Category}

@margin-note{
A @deftech{monoid} (@deftech{monoidal set}) @math{(S, *, e)} is a @tech{set}
@math{S} equipped with an associative binary operation @math{*} and an
@deftech{identity element} @math{e}.
}

A @deftech{one-object category} (@deftech{OOC}) can be viewed as a @tech{monoid}.
In @tech{OOC}, there is only a single @tech{object}, usually denoted by @deftech{*},
and @tech{morphisms} are defined within the context of @tech{*}.

@image["assets/images/ooc.svg"]

@margin-note{
@tech{categories} are sometimes called @deftech{monoidoids}.
}

The @tech{monoid} structure becomes evident when we consider the @tech{identity morphism}
as the @tech{monoid} @tech{identity element} and the @tech{composition} operation
as the @tech{monoid} operation. Thus, @tech{OOCs} provide a categorical perspective
on @tech{monoids}.

@subsection{Subcategory}

Given @tech{categories} @math{𝒞} and @math{𝒟}, @math{𝒟} is a @deftech{subcategory}
of @math{𝒞} if and only if:

@itemlist[
  #:style 'ordered
  @item{@math{𝒟_0 ⊆ 𝒞_0} and @math{𝒟_1 ⊆ 𝒞_1}.}
  @item{If the @tech{object} @math{a} is in @math{𝒟}, then so is @math{id_a}.}
  @item{If the @tech{morphism} @math{f} is in @math{𝒟}, then so are @math{dom(f)} and @math{cod(f)}.}
  @item{If the @tech{composable pair} @math{(f, g)} is in @math{𝒟}, then so is @math{g∘f}.}
  ]

@subsubsection{Full Subcategory}

A @deftech{full subcategory} arises when we selectively remove certain @tech{objects}
from a @tech{category} @math{𝒞} along with the @tech{morphisms} whose @tech{domains}
or @tech{codomains} involve these @tech{objects}. The resulting @tech{subcategory}
@math{𝒟}, retains all the @tech{morphisms} from @math{𝒞} that have not been
affected by the removal of @tech{objects}.

@subsubsection{Wide Subcategory}

A @deftech{wide subcategory} is a @tech{subcategory} that includes all @tech{objects}
from the original @tech{category}. Formally, if @math{𝒟} is a @tech{wide subcategory}
of @math{𝒞}, then every @tech{object} in @math{𝒞} is also an @tech{object} in @math{𝒟}.

@subsection{Semicategory}

@margin-note{
See more in @hyperlink["https://ncatlab.org/nlab/show/semicategory"]{nLab}.
}

@deftech{Semicategories} (@deftech{non-unital categories}) are similar to
@tech{categories} but omit the @tech{identity morphism} requirement.


@section{Mapping Category to Programming}

In this section, we'll explore how @tech{category theory} concepts can be mapped
to practical programming constructs.

Just as @racket[car], @racket[cdr], and @racket[cons] provide an abstraction for
@tech[#:doc rkt-scrbl]{pairs} in Racket, we'll introduce the notions of
@deftech{dom}, @deftech{cod}, and @deftech{∘}
(representing @deftech{domain}, @deftech{codomain}, and @deftech{compose})
to abstract over @tech{categories}.

We stipulate that @code{(∘)} returns @tech{*}, @code{(∘ m)} returns @code{m},
and @code{(morphism=? m)} returns @code{#t} in Racket.

@subsection{Category Examples}

Let's see how these abstractions can be applied to create and manipulate
@tech{categories} in the context of programming.

@subsubsection{Natural Number Category}

The @tech{category} of natural @tech[#:doc rkt-scrbl]{numbers} is an example of
@tech{OOC}. In this case, @tech{morphisms} are natural @tech[#:doc rkt-scrbl]{numbers},
and the single @tech{object} @tech{*} is @code{0} (as the @tech{identity morphism}):

@racketmod[
racket/base

(code:comment2 "Category of Natural Numbers")
(define (dom _) *)
(define (cod _) *)
(define (∘ . m*) (apply + m*))

(define (morphism? m) (exact-nonnegative-integer? m))
(define (morphism=? m . m*) (apply = m m*))

(code:comment2 "Objects")
(define * (∘))

(code:comment2 "Morphisms")
(define f 1)
(define g 2)
(define h 3)

(code:comment2 "Existence of composition")
(morphism=? * (cod f) (dom g))
(morphism=? * (dom (∘ g f)) (dom f))
(morphism=? * (cod (∘ g f)) (cod g))

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

@racketmod[
racket/base

(code:comment2 "Category of Lists")
(define (dom _) *)
(define (cod _) *)
(define (∘ . m*) (apply append m*))

(define (morphism? m) (list? m))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (equal? m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

(code:comment2 "Objects")
(define * (∘))

(code:comment2 "Morphisms")
(define f '(1 2 3))
(define g '(a b c))
(define h '(A B C))

(code:comment2 "Existence of composition")
(morphism=? * (cod f) (dom g))
(morphism=? * (dom (∘ g f)) (dom f))
(morphism=? * (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? * (dom *) (cod *))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@subsubsection{String Category}

@bold{Exercise}: referencing the example code above, implement the @tech{category}
of @tech[#:doc rkt-scrbl]{strings}, which is also an @tech{OOC}.

@subsubsection{Matrix Category}

The @tech{category} of @tech[#:doc math-scrbl #:key "matrix"]{matrices} is a
fascinating example that brings together linear algebra and @tech{category theory}.
In this @tech{category}, each @math{m × n} @tech[#:doc math-scrbl]{matrix} is
considered a @tech{morphism}, its @tech{domain} is the n-order identity
@tech[#:doc math-scrbl]{matrix}, and its @tech{codomain} is the m-order identity
@tech[#:doc math-scrbl]{matrix}:

@racketmod[
racket/base

(require math/matrix)

(code:comment2 "Category of Matrices")
(define (dom m) (identity-matrix (matrix-num-cols m)))
(define (cod m) (identity-matrix (matrix-num-rows m)))
(define (∘ m . m*) (apply matrix* m m*))

(define (morphism? m) (matrix? m))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (morphism=? m1 m2) (apply morphism=? m*))]))

(code:comment2 "Objects")
(define a (identity-matrix 1))
(define b (identity-matrix 2))
(define c (identity-matrix 3))
(define d (identity-matrix 4))

(code:comment2 "Morphisms")
(define (rand m n) (random 1 9))
(define f (build-matrix 2 1 rand))
(define g (build-matrix 3 2 rand))
(define h (build-matrix 4 3 rand))

(code:comment2 "Existence of composition")
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@subsubsection{Binary Relation Category}

@margin-note{
A @deftech{preordered set} @math{(S, ≤)} is a @tech{set} @math{S} equipped with
a binary relation @math{≤} that is reflexive and transitive.
}

A @tech{preordered set} @math{(S, ≤)} can be viewed as a @tech{category} where
@tech{morphisms} are binary relations on its underlying @tech{set} @math{S}:

@racketmod[
racket/base

(require racket/match)

(code:comment2 "Category of Binary Relations")
(define (dom m) (define o (car m)) (cons o o))
(define (cod m) (define o (cdr m)) (cons o o))
(define ∘
  (case-lambda
    [(m) m]
    [(m1 m2) (match* (m1 m2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(m1 m2 . m*) (apply ∘ (∘ m1 m2) m*)]))

(define (morphism? m) (pair? m))
(define morphism=?
  (case-lambda
    [(_) #t]
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
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@margin-note{
A @deftech{partially ordered set} (@deftech{poset}) is a @tech{preordered set}
@math{(S, ≤)} for which @math{≤} is antisymmetric.
}

@bold{Exercise}: view a @tech{poset} as a @tech{category} and implement it.

@subsubsection{Procedure Category}

The @tech{category} of procedures is perhaps the most important @tech{category}
in programming. As the name suggests, this @tech{category} has procedures
(also known as functions in functional programming) as its @tech{morphisms},
so it resembles the @tech{category} of @tech{sets}
(with mathematical @tech{functions} as @tech{morphisms}) in @tech{category theory}.

However, this @tech{category} is not a strict @tech{category} that follows the
@tech{composition rules}, unlike the examples we introduced above. It has some defects.

From the computing science point of view, @tech{category theory} is a strongly
typed language, stronger than any programming language. This is because of the
@tech{composition rule}: @math{g∘f} exists if and only if @math{cod(f) = dom(g)}.
Racket is an untyped language, it allows any procedure to be composed, such as
@code{(compose car +)}, but such a procedure will only @racket[raise] an @racket[exn]
when applied. Therefore, this @tech{category} can be regarded as an @tech{OOC}:

@racketblock[
(define (dom _) (∘))
(define (cod _) (∘))
(define ∘ compose)
]

Another defect is that we cannot compare whether two procedures have the same
functionality, which means we cannot @racket[define] @code{morphism=?}, and have
to rely on the programmer to judge whether the behavior of two procedures is same.
For Racket, it cannot even be sure that @math{g∘f = g∘f} !

@subsection{Constructions on Categories}

This section involves the creation of new categories using existing ones.
These constructions provide a way to extend our understanding of @tech{categories}
and explore various relationships between them.

@subsubsection{Dual Category}

The @tech{dual} of a @tech{category} is the reverse version of the given
@tech{category}.

@image["assets/images/¬cat.svg"]

A @tech{category} @math{𝒞} can be viewed as a directed graph that adheres to the
@tech{composition rules}. If we reverse all the arrows in the directed graph,
the resulting new directed graph still adheres to the @tech{composition rules},
so this new directed graph is also a @tech{category} denoted @math{𝒞^op}.
@math{𝒞^op} is the @deftech{dual} of @math{𝒞}, and @math{(𝒞^op)^op = 𝒞}.

@racketblock[
(define (¬ dom𝒞 cod𝒞 ∘𝒞)
  (define (dom m) (cod𝒞 m))
  (define (cod m) (dom𝒞 m))
  (define (∘ . m*) (apply ∘𝒞 (reverse m*)))
  (values dom cod ∘))
]

@subsubsection{Product Category}

@margin-note{
In this context, @tech[#:key "cartesian product"]{product} refers to the
@deftech{cartesian product}, which is the @tech{product object} in the
@tech{category} of @tech{sets}.
}

The @deftech{product category} combines the given @tech{categories} to form a
new @tech{category}.

@image["assets/images/prod-cat.svg"]

Let's illustrate this concept with a Racket code example
(@racket[list] is used here as @tech{cartesian product}). In the following code,
we create a @tech{product category} by taking the @tech[#:key "cartesian product"]{product}
of @secref["Matrix_Category"] and @secref["Binary_Relation_Category"].

@racketmod[
racket/base

(require racket/match math/matrix)

(code:comment2 "Category of Matrices ℳ")
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))

(define (morphismℳ? m) (matrix? m))
(define morphismℳ=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (morphismℳ=? m1 m2) (apply morphismℳ=? m*))]))

(code:comment2 "Objects in ℳ")
(define a0 (identity-matrix 1))
(define b0 (identity-matrix 2))
(define c0 (identity-matrix 3))
(define d0 (identity-matrix 4))

(code:comment2 "Morphisms in ℳ")
(define (rand m n) (random 1 9))
(define f0 (build-matrix 2 1 rand))
(define g0 (build-matrix 3 2 rand))
(define h0 (build-matrix 4 3 rand))


(code:comment2 "Category of Binary Relations ℛ")
(define (domℛ r) (define o (car r)) (cons o o))
(define (codℛ r) (define o (cdr r)) (cons o o))
(define ∘ℛ
  (case-lambda
    [(r) r]
    [(r1 r2) (match* (r1 r2) [(`(,b . ,c) `(,a . ,b)) `(,a . ,c)])]
    [(r1 r2 . r*) (apply ∘ℛ (∘ℛ r1 r2) r*)]))

(define (morphismℛ? r) (pair? r))
(define morphismℛ=?
  (case-lambda
    [(_) #t]
    [(r1 r2) (equal? r1 r2)]
    [(r1 r2 . r*) (and (morphismℛ=? r1 r2) (apply morphismℛ=? r*))]))

(code:comment2 "Objects in ℛ")
(define a1 '(a . a))
(define b1 '(b . b))
(define c1 '(c . c))
(define d1 '(d . d))

(code:comment2 "Morphisms in ℛ")
(define f1 '(a . b))
(define g1 '(b . c))
(define h1 '(c . d))


(code:comment2 "Product Category ℳ × ℛ")
(define (dom p) (match p [`(,m ,r) `(,(domℳ m) ,(domℛ r))]))
(define (cod p) (match p [`(,m ,r) `(,(codℳ m) ,(codℛ r))]))
(define (∘ p . p*)
  (define m* (map car  (cons p p*)))
  (define r* (map cadr (cons p p*)))
  (list (apply ∘ℳ m*) (apply ∘ℛ r*)))

(define (morphism? p)
  (and (list? p) (= 2 (length p))
       (morphismℳ? (car  p))
       (morphismℛ? (cadr p))))
(define (morphism=? p . p*)
  (define m* (map car  (cons p p*)))
  (define r* (map cadr (cons p p*)))
  (and (apply morphismℳ=? m*) (apply morphismℛ=? r*)))

(code:comment2 "Objects in ℳ × ℛ")
(define a (list a0 a1)) (code:comment "(a0, a1)")
(define b (list b0 b1)) (code:comment "(b0, b1)")
(define c (list c0 c1)) (code:comment "(c0, c1)")
(define d (list d0 d1)) (code:comment "(d0, d1)")

(code:comment2 "Morphisms in ℳ × ℛ")
(define f (list f0 f1)) (code:comment "(f0, f1)")
(define g (list g0 g1)) (code:comment "(g0, g1)")
(define h (list h0 h1)) (code:comment "(h0, h1)")

(code:comment2 "Existence of composition")
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@bold{Exercise}: @racket[define] @code{dom×}, @code{cod×}, @code{∘×},
@code{morphism×?} and @code{morphism×=?} so that we can @racket[define]
@tech{category} @math{ℳ × ℛ} in this way:

@racketblock[
(define dom (dom× domℳ domℛ))
(define cod (cod× codℳ codℛ))
(define ∘ (∘× ∘ℳ ∘ℛ))
(define morphism? (morphism×? morphismℳ? morphismℛ?))
(define morphism=? (morphism×=? morphismℳ=? morphismℛ=?))
]

@subsubsection{Arrow Category}

Given a @tech{category} @math{𝒞}, the @deftech{arrow category} @math{Arr(𝒞)} is
constructed by takeing its @tech{morphisms} as @tech{objects} and @tech{commutative squares}
as @tech{morphisms}.

For example, here are @code{3} @tech{commutative squares} in @math{𝒞}:

@image["assets/images/arr-cat_1.svg"]

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative squares} by @tech{composition}:

@image["assets/images/arr-cat_2.svg"]

Finally, using nodes to represent @tech{morphisms}, and using arrows to represent
@tech{commutative squares}, we get a directed graph that obeys the @tech{composition rules},
which is the @tech{arrow category} @math{Arr(𝒞)}:

@margin-note{
Although we name arrows using pairs here, note that they are not pairs, but
@tech{commutative squares}.
}

@image["assets/images/arr-cat_3.svg"]

In the following code, we create an @tech{arrow category} to which
@secref["Matrix_Category"] gives rise:

@racketmod[
racket/base

(require racket/match math/matrix)

(code:comment2 "Category of Matrices ℳ")
(define (domℳ m) (identity-matrix (matrix-num-cols m)))
(define (codℳ m) (identity-matrix (matrix-num-rows m)))
(define (∘ℳ m . m*) (apply matrix* m m*))

(define (morphismℳ? m) (matrix? m))
(define morphismℳ=?
  (case-lambda
    [(_) #t]
    [(m1 m2) (matrix= m1 m2)]
    [(m1 m2 . m*) (and (morphismℳ=? m1 m2) (apply morphismℳ=? m*))]))

(code:comment2 "Objects in ℳ")
(define a0 (identity-matrix 1))
(define b0 (identity-matrix 2))
(define c0 (identity-matrix 3))
(define d0 (identity-matrix 4))
(define e0 (identity-matrix 5))
(define f0 (identity-matrix 6))
(define g0 (identity-matrix 7))
(define h0 (identity-matrix 8))

(code:comment2 "Morphisms in ℳ")
(define (rand m n) (random 1 9))

(define p0 (build-matrix 2 1 rand))
(define q0 (build-matrix 4 3 rand))
(define r0 (build-matrix 6 5 rand))
(define s0 (build-matrix 8 7 rand))

(define i0 (build-matrix 3 1 rand))
(define j0 (build-matrix 4 2 rand))
(define k0 (build-matrix 5 3 rand))
(define l0 (build-matrix 6 4 rand))
(define m0 (build-matrix 7 5 rand))
(define n0 (build-matrix 8 6 rand))


(code:comment2 "Arrow Category Arr(ℳ)")
(define (dom s)
  (match s
    [`((,j ,p) (,q ,i))
     (define a (domℳ i))
     (define b (domℳ j))
     `((,b ,p) (,p ,a))]))
(define (cod s)
  (match s
    [`((,j ,p) (,q ,i))
     (define c (codℳ i))
     (define d (codℳ j))
     `((,d ,q) (,q ,c))]))
(define ∘
  (case-lambda
    [(s) s]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,l ,q) (,r ,k))
         `((,j ,p) (,q ,i)))
        `((,(∘ℳ l j) ,p) (,r ,(∘ℳ k i)))])]
    [(s1 s2 . s*) (apply ∘ (∘ s1 s2) s*)]))

(define (morphism? s)
  (match s
    [`((,j ,p) (,q ,i))
     (and (morphismℳ? j)
          (morphismℳ? p)
          (morphismℳ? q)
          (morphismℳ? i)
          (morphismℳ=? (∘ℳ j p) (∘ℳ q i)))]
    [_ #f]))
(define morphism=?
  (case-lambda
    [(_) #t]
    [(s1 s2)
     (match* (s1 s2)
       [(`((,n ,r) (,s ,m))
         `((,j ,p) (,q ,i)))
        (and (morphismℳ=? n j)
             (morphismℳ=? r p)
             (morphismℳ=? s q)
             (morphismℳ=? m i))]
       [(_ _) #f])]
    [(s1 s2 . s*) (and (morphism=? s1 s2) (apply morphism=? s*))]))

(code:comment2 "Objects in Arr(ℳ)")
(define a `((,b0 ,p0) (,p0 ,a0))) (code:comment "p0")
(define b `((,d0 ,q0) (,q0 ,c0))) (code:comment "q0")
(define c `((,f0 ,r0) (,r0 ,e0))) (code:comment "r0")
(define d `((,h0 ,s0) (,s0 ,g0))) (code:comment "s0")

(code:comment2 "Morphisms in Arr(ℳ)")
(define f `((,j0 ,p0) (,q0 ,i0))) (code:comment "(i0, j0)")
(define g `((,l0 ,q0) (,r0 ,k0))) (code:comment "(k0, l0)")
(define h `((,n0 ,r0) (,s0 ,m0))) (code:comment "(m0, n0)")

(code:comment2 "Existence of composition")
(morphism=? b (cod f) (dom g))
(morphism=? a (dom (∘ g f)) (dom f))
(morphism=? c (cod (∘ g f)) (cod g))

(code:comment2 "Associativity of composition")
(morphism=? (∘ h g f) (∘ (∘ h g) f) (∘ h (∘ g f)))

(code:comment2 "Existence of identity")
(morphism=? a (dom a) (cod a))

(code:comment2 "Identity and composition")
(morphism=? f (∘ f (dom f)) (∘ (cod f) f))
]

@bold{Exercise}: @racket[define] @code{Arr} so that we can @racket[define] the
@tech{arrow category} @math{Arr(ℳ)} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  (Arr domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?))
]

@subsubsection{(Co)Slice Category}

A @deftech{slice category} (@deftech{over category}) @math{𝒞/c} is a construction
that allows us to study a @tech{category} @math{𝒞} through the lens of a fixed
@tech{object} @math{c} in @math{𝒞}. Intuitively, @math{𝒞/c} consists of all
the @tech{objects} and @tech{morphisms} in @math{𝒞} that are "over" @math{c}.

@math{𝒞/c} is constructed by takeing @math{𝒞}'s @tech{morphisms} end to @math{c}
as @tech{objects}, and @tech{commutative triangles} end to @math{c} as @tech{morphisms}.

For example, here are @code{3} @tech{commutative triangles} end to @math{c_1}
in @math{𝒞}:

@image["assets/images/over-cat_1.svg"]

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["assets/images/over-cat_2.svg"]

Finally, using nodes to represent @tech{morphisms} end to @math{c_1}, and using
arrows to represent @tech{commutative triangles} end to @math{c_1}, we get a
directed graph that obeys the @tech{composition rules}, which is the
@tech{slice category} @math{𝒞/c_1}:

@margin-note{
Although we name arrows using @tech{morphisms} here, note that they are not
@tech{morphisms}, but @tech{commutative triangles} end to @math{c_1}.
}

@image["assets/images/over-cat_3.svg"]

@bold{Exercise}: referencing the example code of the @tech{arrow category}
@math{Arr(ℳ)}, implement the @tech{slice category} @math{ℳ/m}.

@bold{Exercise}: @racket[define] @code{Sli} so that we can @racket[define]
the @tech{slice category} @math{ℳ/m} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  ((Sli domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?) m))
]

The @tech{dual} notion of a @tech{slice category} @math{𝒞/c} is a @deftech{coslice category}
(@deftech{under category}) @math{c/𝒞}, which consists of all the @tech{objects}
and @tech{morphisms} in @math{𝒞} that are "under" @math{c}.

@math{c/𝒞} is constructed by takeing @math{𝒞}'s @tech{morphisms} start from @math{c}
as @tech{objects}, and @tech{commutative triangles} start from @math{c} as @tech{morphisms}.

For example, here are @code{3} @tech{commutative triangles} start from @math{c_0}
in @math{𝒞}:

@image["assets/images/under-cat_1.svg"]

@margin-note{
The proof is left as an exercise.
}

Then, we get some new @tech{commutative triangles} by @tech{composition}:

@image["assets/images/under-cat_2.svg"]

Finally, using nodes to represent @tech{morphisms} start from @math{c_0}, and using
arrows to represent @tech{commutative triangles} start from @math{c_0}, we get a
directed graph that obeys the @tech{composition rules}, which is the
@tech{coslice category} @math{c_0/𝒞}:

@margin-note{
Although we name arrows using @tech{morphisms} here, note that they are not
@tech{morphisms}, but @tech{commutative triangles} start from @math{c_0}.
}

@image["assets/images/under-cat_3.svg"]

@bold{Exercise}: referencing the example code of the @tech{arrow category}
@math{Arr(ℳ)}, implement the @tech{coslice category} @math{m/ℳ}.

@bold{Exercise}: @racket[define] @code{¬Sli} so that we can @racket[define]
the @tech{coslice category} @math{m/ℳ} in this way:

@racketblock[
(define-values (dom cod ∘ morphism? morphism=?)
  ((¬Sli domℳ codℳ ∘ℳ morphismℳ? morphismℳ=?) m))
]

@bold{Exercise}: prove that @math{ℳ^op/m = (m/ℳ)^op}.

@bold{Exercise}: @racket[define] @code{¬Sli} by using @code{¬} and @code{Sli}.
