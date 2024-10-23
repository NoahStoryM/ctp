#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0 (except-in typed/racket/no-check =))
                     racket/function
                     racket/hash
                     racket/match
                     racket/promise
                     rackunit
                     math/matrix)
          "../ctp-utils.rkt")

@title[#:tag "_Functor_"]{Functor}

In this @seclink["_Functor_"]{chapter}, we introduce the concept of @tech{functors}.
@tech{Functors} are fundamental in @tech{category theory}, as they map both
@tech{objects} and @tech{morphisms} between @tech{categories} while preserving
the structural relationships between them.

To clarify the explanation and show how @tech{functors} apply to programming,
we will use Typed Racket syntax. Specifically, we will opt for
@code{#lang typed/racket/base/no-check} instead of @code{#lang racket/base}.
This choice allows us to incorporate type annotations for improved code clarity
and better illustration of functorial properties.

@local-table-of-contents[]

@section{Functor}

Just as @tech{functions} map @tech{elements} between @tech{sets}, @tech{functors}
provide a way to map @tech{objects} and @tech{morphisms} between @tech{categories}.
This mapping preserves the structural aspects of @tech{categories}.

@margin-note{
For convenience, if @math{a} is an @tech{object} in @math{𝒞}, @math{F(a) = F_0(a)};
if @math{f} is a @tech{morphism} in @math{𝒞}, @math{F(f) = F_1(f)}.
}

Similar to how a @tech{category} @math{𝒞} is defined by two @tech{collections}
@math{𝒞_0} and @math{𝒞_1}, a @deftech{functor} @math{F} from @math{𝒞} to @math{𝒟},
denoted by @math{F : 𝒞 → 𝒟}, is also defined by two @tech{functions}
@math{F_0 : 𝒞_0 → 𝒟_0} and @math{F_1 : 𝒞_1 → 𝒟_1}. These @tech{functions} must
satisfy the following properties:

@margin-note{
The @tech{functors} discussed in this tutorial default to the @deftech{covariant functor}s.
A @deftech{contravariant functor} from @math{𝒞} to @math{𝒟} can be considered as
a @tech{covariant functor} from @math{𝒞@^{op}} to @math{𝒟}. See more in
@hyperlink["https://ncatlab.org/nlab/show/contravariant+functor"]{nLab}.
}

@itemlist[
  #:style 'ordered
  @item{Preservation of @tech{domains} and @tech{codomains}

        For any @tech{morphism} @math{f : a → b : 𝒞}, there is a @tech{morphism}
        @math{F(f) : F(a) → F(b) : 𝒟}.

        @image["scribblings/functor/images/F-1.svg"]{[picture] F-1.svg}}
  @item{Preservation of @tech{identity morphisms}

        For any @tech{object} @math{a : 𝒞}, @math{F(id_a) = id@_{F(a)}}.

        @image["scribblings/functor/images/F-2.svg"]{[picture] F-2.svg}}
  @item{Preservation of @tech{composable pairs}

        If @math{(f, g)} is a @tech{composable pair} in @math{𝒞}, then
        @math{(F(f), F(g))} is a @tech{composable pair} in @math{𝒟}, and
        @math{F(g∘f) = F(g)∘F(f)}.

        @image["scribblings/functor/images/F-3.svg"]{[picture] F-3.svg}}
  ]

@bold{Exercise}: Show that a @tech{function} between two @tech{sets} can be
viewed as a @tech{functor} between two @tech{discrete categories}.

We can describe @math{𝒞} with the following @tech{diagram}:

@image["scribblings/functor/images/cat.svg"]{[picture] cat.svg}

To illustrate the @tech{functor} @math{F}, consider a @tech{function}
@math{F_2 : 𝒞_2 → 𝒟_2}. This allows us to describe @math{F} with the following
@tech{diagram}:

@image["scribblings/functor/images/functor.svg"]{[picture] functor.svg}

@margin-note{
The proof is left as an exercise.
}

We can see that each similarly labeled square in the @tech{diagram} is a
@tech{commutative square}. Additionally, the @tech{commutative squares} show the
properties of @tech{functors}.

To verify the properties of @tech{functors}, we'll @racket[define] some
@tech{check} @tech{procedures} to automate the testing of essential properties
a @tech{functor} preserves:

@racketfile{code/functor/check.rkt}

The following example illustrates how to implement @tech{functors} in Racket:

@racketfile{code/functor/𝐒𝐞𝐭→𝐑𝐞𝐥.rkt}

@bold{Exercise}: Show that a @deftech{diagram} can be defined as a @tech{functor},
where the @tech{domain} is called the @deftech{indexing category} of the
@tech{diagram}.

@bold{Exercise}: Show that a @tech{diagram} is @deftech{commutative} iff its
@tech{indexing category} is a @tech{thin category}.

A @tech{functor} @math{F : 𝒞 → 𝒟} @deftech{preserve}s a property @math{P} of
@tech{morphisms} in @math{𝒞} if whenever @math{f} has @math{P}, so does @math{F(f)}.
Conversely, a @tech{functor} @deftech{reflect}s a property @math{P} of
@tech{morphisms} in @math{𝒟} if whenever @math{F(f)} has @math{P}, so does @math{f}.

@math{F} may have additional properties:

@itemlist[
  @item{@deftech{Faithful}:
        for every pair of @tech{objects} @math{a} and @math{b} in @math{𝒞}, the
        induced mapping from @math{Hom@_{𝒞}(a, b)} to @math{Hom@_{𝒟}(F(a), F(b))}
        is @tech{injective}.}
  @item{@deftech{Full}:
        for every pair of @tech{objects} @math{a} and @math{b} in @math{𝒞}, the
        induced mapping from @math{Hom@_{𝒞}(a, b)} to @math{Hom@_{𝒟}(F(a), F(b))}
        is @tech{surjective}.}
  @item{@deftech{Fully Faithful}:
        for every pair of @tech{objects} @math{a} and @math{b} in @math{𝒞}, the
        induced mapping from @math{Hom@_{𝒞}(a, b)} to @math{Hom@_{𝒟}(F(a), F(b))}
        is @tech{bijective}.}
]

If a @tech{functor} is @tech{fully faithful}, and @tech{injective} on @tech{objects},
it is called an @deftech{embedding}.

@bold{Exercise}: Prove that every @tech{functor} @tech{preserves}
@tech{commutative diagrams}.

@bold{Exercise}: Prove that every @tech{faithful} @tech{functor} @tech{reflects}
@tech{commutative diagrams}.

@bold{Exercise}: Prove that every @tech{faithful} @tech{functor} @tech{reflects}
@tech{monomorphisms}.

@bold{Exercise}: Prove that every @tech{faithful} @tech{functor} @tech{reflects}
@tech{epimorphisms}.

@bold{Exercise}: Let @math{F : 𝒞 → 𝒟} be @tech{fully faithful}. Prove that if
@math{F(a) = F(b)}, then @math{a ≅ b} and
@math{Hom@_{𝒞}(a, a) ≅ Hom@_{𝒞}(a, b) ≅ Hom@_{𝒞}(b, a) ≅ Hom@_{𝒞}(b, b)}.

@subsection{Category of Categories}

Since @tech{functors} are defined as a kind of @tech{function}, it is natural to
@racket[define] the @tech{composition} of @tech{functors} in the same way as the
@tech{composition} of @tech{functions}: @math{G∘F(f) = G(F(f))}. This allows us
to treat @tech{functors} as @tech{morphisms}, leading to the construction of the
@tech{category} of @tech{categories}, denoted as @tech{𝐂𝐚𝐭}, which forms a
higher-level structure.

@bold{Exercise}: Prove that the @tech{composition} of @tech{functors} is
@tech{associative}.

An @deftech{endofunctor} is an @tech{endomorphism} in @tech{𝐂𝐚𝐭}, while an
@deftech{autofunctor} is an @tech{automorphism} in @tech{𝐂𝐚𝐭}.

@bold{Exercise}: Prove that every @tech{invertible} @tech{functor} is an
@tech{embedding}.

@bold{Exercise}: For @tech{objects} @math{𝒞} and @math{𝒟} in @tech{𝐂𝐚𝐭}.
Prove @math{𝒞×𝒟 ≅ 𝒟×𝒞}.

@bold{Exercise}: Think about what @tech{1} is in @tech{𝐂𝐚𝐭}.
Prove @math{𝒞 ≅ 1×𝒞 ≅ 𝒞×1}.

@bold{Exercise}: Show that both @math{𝒞} and @math{𝒟} are @tech{isomorphic} to
some @tech{quotient categories} of @math{𝒞×𝒟}.

@bold{Exercise}: For @tech{objects} @math{𝒞} and @math{𝒟} in @tech{𝐂𝐚𝐭}.
Prove @math{𝒞+𝒟 ≅ 𝒟+𝒞}.

@bold{Exercise}: Think about what @tech{0} is in @tech{𝐂𝐚𝐭}.
Prove @math{𝒞 ≅ 0+𝒞 ≅ 𝒞+0}.

@bold{Exercise}: Show that both @math{𝒞} and @math{𝒟} are @tech{isomorphic} to
some @tech{subcategories} of @math{𝒞+𝒟}.

@bold{Exercise}: Prove that if there is a @tech{functor} from @math{𝒞} to
@tech{0}, then @math{𝒞 ≅ 0}.

@bold{Exercise}: Prove @math{0 ≅ 0×𝒞 ≅ 𝒞×0}.

@bold{Exercise}: For @tech{objects} @math{𝒜}, @math{ℬ}, and @math{𝒞} in @tech{𝐂𝐚𝐭}.
Prove the @tech{distributive laws}: @math{𝒜×(ℬ+𝒞) ≅ 𝒜×ℬ+𝒜×𝒞} and
@math{(𝒜+ℬ)×𝒞 ≅ 𝒜×𝒞+ℬ×𝒞}.

In practical implementations using Racket, we'll employ @tech{𝐏𝐫𝐨𝐜} to symbolize
@tech{𝐂𝐚𝐭}. This is because, in Racket, we implement @tech{functors} as
@tech{procedures}. Note that since the task of comparing @tech{procedure}
functionality can only be done by the programmer, we'll avoid using @racket[=]
or just use it as pseudocode.

@;; @racketblock[
@;; (code:comment2 "Category of Categories")
@;; (: dom (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) 𝒜)))
@;; (: cod (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) ℬ)))
@;; (: ∘ (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] ... [𝒵 : 𝐂𝐚𝐭]) (→ (× (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 ℬ 𝒞) ...) (→𝐂𝐚𝐭 𝒜 𝒵))))
@;; (: ? (pred (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→𝐂𝐚𝐭 𝒜 ℬ))))
@;; (: = (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] [𝒟 : 𝐂𝐚𝐭] ...) (→ (× (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 𝒞 𝒟) ...) Boolean)))
@;;
@;; (code:comment "Categories")
@;; (: 𝒜 𝐂𝐚𝐭) (? 𝒜) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→𝒜 a b)))")
@;; (: ℬ 𝐂𝐚𝐭) (? ℬ) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→ℬ a b)))")
@;; (: 𝒞 𝐂𝐚𝐭) (? 𝒞) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒞 a b)))")
@;; (: 𝒟 𝐂𝐚𝐭) (? 𝒟) (code:comment# "(∀ ([a : 𝒟] [b : 𝒟]) (→ (→𝒟 a b) (→𝒟 a b)))")
@;;
@;; (code:comment2 "Functors")
@;; (: F (→𝐂𝐚𝐭 𝒜 ℬ)) (? F) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→ℬ (F a) (F b))))")
@;; (: G (→𝐂𝐚𝐭 ℬ 𝒞)) (? G) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→𝒞 (G a) (G b))))")
@;; (: H (→𝐂𝐚𝐭 𝒞 𝒟)) (? H) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒟 (H a) (H b))))")
@;;
@;; (code:comment2 "Existence of composition")
@;; (= ℬ (cod F) (dom G))
@;; (= 𝒜 (dom (∘ G F)) (dom F))
@;; (= 𝒞 (cod (∘ G F)) (cod G))
@;;
@;; (code:comment2 "Associativity of composition")
@;; (= (∘ H G F) (∘ (∘ H G) F) (∘ H (∘ G F)))
@;;
@;; (code:comment2 "Existence of identity morphisms")
@;; (= 𝒜 (dom 𝒜) (cod 𝒜))
@;;
@;; (code:comment2 "Composition and identity morphisms")
@;; (= F (∘ F (dom F)) (∘ (cod F) F))
@;; ]

Although we have given specific @secref{Category_Examples}, these examples are
just one way to implement the corresponding concepts. We can @racket[define]
these concepts in other ways as well. These different implementations of the same
concept can be seen as @tech{equivalent} @tech{objects} in @tech{𝐂𝐚𝐭}.

Therefore, in the following sections, when we discuss specific @tech{categories},
their definitions might differ from the Racket code in the previous sections.
For instance, in the @secref{Category_of_Sets}, @tech{morphisms} of @tech{𝐒𝐞𝐭} are
defined as @racket[hash] tables, but essentially they are mappings and might be
defined as @tech{procedures} in later sections.

@subsection{Representative Functor}

A @deftech{@deftech{representative} functor} @math{F : 𝒞 → 𝒟} is a @tech{functor}
that @math{∀d ∈ 𝒟_0, ∃c ∈ 𝒞_0, F(c) ≅ d}.

@subsection{Inclusion Functor}

The @deftech{inclusion functor} @math{I : 𝒟 → 𝒞} is a @tech{functor} which sends
each @tech{object} and each @tech{morphism} in @math{𝒟} to @math{𝒞}.

@bold{Exercise}: Prove @math{𝒟 ⊆ 𝒞}.

@bold{Exercise}: Prove that if @math{I} is @tech{representative}, then @math{𝒟}
is a @tech{representative subcategory} of @math{𝒞}.

@bold{Exercise}: Prove that every @tech{inclusion functor} is a
@tech{faithful} @tech{functor}.

@bold{Exercise}: Prove that every @tech{inclusion functor} from a
@tech{full subcategory} is @tech{fully faithful}.

@subsection{Projection Functor}

The @deftech{projection functor} (@deftech{quotient functor}) @math{P : 𝒞 → 𝒞/∼}
is a @tech{functor} that @math{∀a ∈ 𝒞_0, P(a) = [a]} and @math{∀f ∈ 𝒞_1, P(f) = [f]}.

@bold{Exercise}: For a @tech{functor} @math{F : 𝒞 → 𝒟} and a
@tech{congruence relation} @math{∼} that @math{∀f, g ∈ 𝒞_1, f ∼ g ⇒ F(f) = F(g)}.
Prove that there is a unique @tech{functor} @math{F′ : 𝒞/∼ → 𝒟} for which
@math{F′∘P = F}.

@image["scribblings/functor/images/F′.svg"]{[picture] F′.svg}

@subsection{Forgetful Functor}

A @deftech{forgetful functor} (@deftech{underlying functor} or @deftech{stripping functor})
is a type of @tech{functor} that forgets some or all of the structure of the
@tech{objects} and the structure-preserving @tech{functions} in its @tech{domain}
@tech{category}.

For example, if we forget @tech{morphisms} in a @tech{category}, then we get a
@tech{set}. Extending this idea, we get a @tech{forgetful functor} @math{U : 𝐂𝐚𝐭 → 𝐒𝐞𝐭},
which forgets @math{𝒞_1} and @math{F_1}, but preserves @math{𝒞_0} and @math{F_0}:
@math{U_0(𝒞) = 𝒞_0} and @math{U_1(F) = F_0}.

@image["scribblings/functor/images/U.svg"]{[picture] U.svg}

Here's how we can @racket[define] @math{U} in Racket:

@racketblock[
(: U (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐒𝐞𝐭 (U 𝒜) (U ℬ)))))
(define (U F) (λ (a) (F a)))
]

@bold{Exercise}: Try to @racket[define] a @tech{forgetful functor} from @tech{𝐌𝐨𝐧}
to @tech{𝐒𝐞𝐭@_{∗}}.

@bold{Exercise}: Try to @racket[define] a @tech{forgetful functor} from @tech{𝐒𝐞𝐭@_{∗}}
to @tech{𝐒𝐞𝐭}.

@subsection{Composition Functor}

For @tech{morphism} @math{f : a → b : 𝒞} and @math{g : b → c : 𝒞}, there are two
kinds of @deftech{composition functor}s: @math{f/𝒞 : b/𝒞 → a/𝒞} and
@math{𝒞/g : 𝒞/b → 𝒞/c}. @math{f/𝒞} maps an @tech{object} @math{g} in @math{b/𝒞}
to the @tech{object} @math{g∘f} in @math{a/𝒞}, while @math{𝒞/g} maps an
@tech{object} @math{f} in @math{𝒞/b} to the @tech{object} @math{g∘f} in @math{𝒞/c}.

@bold{Exercise}: Implement a @tech{composition functor} @math{f/ℳ : b/ℳ → a/ℳ},
where @math{ℳ} is @tech{𝐌𝐚𝐭𝐫} and @math{f : a → b} is a @tech{morphism} in @math{ℳ}.

@bold{Exercise}: Implement a @tech{composition functor} @math{ℳ/g : ℳ/b → ℳ/c},
where @math{ℳ} is @tech{𝐌𝐚𝐭𝐫} and @math{g : b → c} is a @tech{morphism} in @math{ℳ}.

@subsection{(Co)Slice Functor}

For a @tech{category} @math{𝒞}, the @deftech{slice functor} @math{𝒞/-} maps each
@tech{morphism} @math{g : b → c : 𝒞} to the @tech{composition functor}
@math{𝒞/g : 𝒞/b → 𝒞/c}.

@image["scribblings/functor/images/𝒞÷-.svg"]{[picture] 𝒞÷-.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭)
(: 𝒞/- (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝒞 b c) (→𝐂𝐚𝐭 𝒞/b 𝒞/c))))
(define (𝒞/- g)
  (: 𝒞/g (∀ ([x : 𝒞/b] [y : 𝒞/b]) (→ (→𝒞/b x y) (→𝒞/c (∘𝒞 g x) (∘𝒞 g y)))))
  (define 𝒞/g
    (match-λ
      [`((,x) (,y ,z))
       `((,(∘𝒞 g x)) (,(∘𝒞 g y) ,z))]))
  𝒞/g)
]

@bold{Exercise}: Prove @math{𝒞/g∘f = 𝒞/g∘𝒞/f}.

For a @tech{category} @math{𝒞}, the @deftech{coslice functor} @math{-/𝒞} maps each
@tech{morphism} @math{f : a → b : 𝒞} to the @tech{composition functor}
@math{f/𝒞 : b/𝒞 → a/𝒞}.

@image["scribblings/functor/images/-÷𝒞.svg"]{[picture] -÷𝒞.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭)
(: -/𝒞 (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝒞 a b) (→𝐂𝐚𝐭 b/𝒞 a/𝒞))))
(define (-/𝒞 f)
  (: f/𝒞 (∀ ([x : b/𝒞] [y : b/𝒞]) (→ (→b/𝒞 x y) (→a/𝒞 (∘𝒞 x f) (∘𝒞 y f)))))
  (define f/𝒞
    (match-λ
      [`((,z ,x) (,y))
       `((,z ,(∘𝒞 x f)) (,(∘𝒞 y f)))]))
  f/𝒞)
]

@bold{Exercise}: Prove @math{g∘f/𝒞 = f/𝒞∘g/𝒞}.

@section{Categories of Structured Sets}

@margin-note{
@tech{Functors} are sometimes called @deftech{category homomorphism}s.
}

A @deftech{structured set} is a @tech{set}, known as @deftech{underlying set},
equipped with some additional structure (e.g., @tech{monoids}), and the
@deftech{homomorphisms} between them (e.g., @tech{monoid homomorphisms}) are
@tech{functions} that preserve that structure.

Although many @tech{categories} of @tech{structured sets} are examples of
@tech{concrete categories}, the concept of a @tech{concrete category} is more
general. A @deftech{concrete category} is a @tech{category} @math{𝒞} equipped
with a @tech{faithful} @tech{functor} @math{U : 𝒞 → 𝐒𝐞𝐭}. A @tech{category} is
said to be @deftech{concretizable} iff it admits such a @tech{functor}.

@tech{Structured sets} and their @tech{homomorphisms} form fundamental
@tech{categories} that encapsulate various algebraic structures. These
@tech{categories} allow us to study and generalize properties and operations
across different mathematical systems. In this @seclink["Categories_of_Structured_Sets"]{section},
we'll explore several important @tech{categories} of @tech{structured sets}.

@subsection{Category of Monoids}

A @deftech{monoid homomorphism} @math{f : (S, ∘, s) → (T, ∙, t)} is a
@tech{function} that preserves the @tech{monoid} structure:
@math{∀x, y ∈ S, f(x∘y) = f(x)∙f(y)}, and @math{f(s) = t}.

The @tech{category} of @tech{monoids}, denoted as @deftech{𝐌𝐨𝐧}, where
@tech{objects} are @tech{monoids} and @tech{morphisms} are @tech{monoid homomorphisms}.
@tech{𝐌𝐨𝐧} is @tech{equivalent} to the @tech{category} of @tech{OOCs}, denoted
as @deftech{𝐎𝐨𝐜}.

@subsection{Category of Groups}

A @deftech{group homomorphism} @math{f : (S, ∘, s) → (T, ∙, t)} is a
@tech{monoid homomorphism} that preserves the @tech{group} structure:
@math{∀x ∈ S, f(x@^{–1}) = f(x)@^{–1}}.

The @tech{category} of @tech{groups}, denoted as @deftech{𝐆𝐫𝐩}, where
@tech{objects} are @tech{groups} and @tech{morphisms} are @tech{group homomorphisms}.
@tech{𝐆𝐫𝐩} is @tech{equivalent} to the @tech{category} of @tech{OOGs}, denoted
as @deftech{𝐎𝐨𝐠}.

@subsection{Category of Prosets}

A @deftech{@deftech{monotone} function}
(@deftech{@deftech{monotonic} function}, @deftech{@deftech{isotone} function},
 @deftech{@deftech{isotonic} function}, or @deftech{order homomorphism})
@math{f : (S, ≤)  → (T, ⋜)} is a @tech{function} that preserves the @tech{proset}
structure: @math{∀x, y ∈ S, x ≤ y ⇒ f(x) ⋜ f(y)}.

The @tech{category} of @tech{prosets}, denoted as @deftech{𝐏𝐫𝐨𝐬}, where
@tech{objects} are @tech{prosets} and @tech{morphisms} are @tech{monotone functions}.
@tech{𝐏𝐫𝐨𝐬} is @tech{equivalent} to the @tech{category} of @tech{thin categories}.

@subsection{Category of Posets}

The @tech{category} of @tech{posets}, denoted as @deftech{𝐏𝐨𝐬}, is a
@tech{full subcategory} of @tech{𝐏𝐫𝐨𝐬} where @tech{objects} are @tech{posets}.

@subsection{Category of Graphs}

@margin-note{
In this context, @tech{graphs} are assumed to be @tech{directed graphs} by default.
}

Similar to @tech{categories}, we can describe a @tech{graph} @math{𝒢} with
the following @tech{diagram}:

@image["scribblings/functor/images/grf.svg"]{[picture] grf.svg}

A @deftech{graph homomorphism} @math{φ : 𝒢 → ℋ} is like a @tech{functor} that
only preserves @tech{sources} and @tech{targets}. We can describe @math{φ} with
the following @tech{diagram}:

@image["scribblings/functor/images/grf-hom.svg"]{[picture] grf-hom.svg}

The @tech{category} of @deftech{graph}, denoted as @deftech{𝐆𝐫𝐟}, where
@tech{objects} are @tech{graphs} and @tech{morphisms} are @tech{graph homomorphisms}.

@section{𝐒𝐞𝐭-Valued Functor}

A @deftech{𝐒𝐞𝐭-valued functor} on @math{𝒞} is a @tech{functor} from @math{𝒞} to
@tech{𝐒𝐞𝐭}. Such @tech{functors} map @tech{objects} in @math{𝒞} to @tech{sets},
and @tech{morphisms} in @math{𝒞} to @tech{functions} between these @tech{sets}.
These @tech{functors} are central to many areas of @tech{category theory}.

@margin-note{
More generally, given any @tech{category} @math{𝒟}, a @math{𝒟}-valued
@tech{presheaf} on @math{𝒞} is a @tech{functor} from @math{𝒞@^{op}} to @math{𝒟}.
}

One important type of @tech{𝐒𝐞𝐭-valued functor} is the @deftech{presheaf}, which
is a @tech{functor} from @math{𝒞@^{op}} to @tech{𝐒𝐞𝐭}. The name @tech{presheaf}
comes from @deftech{topology}, but the concept applies to arbitrary
@tech{categories}.

@tech{𝐒𝐞𝐭-valued functors}, including @tech[#:key "presheaf"]{presheaves}, have
theoretical importance due to the @tech{Yoneda Lemma}, a fundamental result in
@tech{category theory} that will be introduced in detail in the next
@seclink["_Natural_Transformation_"]{chapter}.

@subsection{Powerset Functor}

The @deftech{powerset} of a @tech{set} @math{s}, denoted by @math{𝒫(s)}, is the
@tech{set} of all @tech{subsets} of @math{s}. This concept is central to several
@tech{powerset functors}, which map @tech{sets} to their corresponding
@tech{powersets}.

@racketfile{code/function/𝒫.rkt}

@image["scribblings/functor/images/f.svg"]{[picture] f.svg}

Given a @tech{function} @math{f : a → b}, where @math{a_0} is a @tech{subset} of
@math{a} and @math{b_0} is a @tech{subset} of @math{b}, we can @racket[define]
several @deftech{powerset functor}s that map a @tech{set} @math{s} to @math{𝒫(s)}.

@subsubsection{Direct Image Functor}

The @deftech{direct image} (@deftech{existential image}) of @math{a_0}, denoted
by @math{f@_{∗}(a_0)}, is the @tech{subset} of @math{b}:
@math{f@_{∗}(a_0) ≔ {f(x) | x ∈ a_0}}.

The @deftech{direct image functor} (@deftech{existential image functor})
@math{𝒫@_{∗}} takes @math{f} to @math{f@_{∗}}: @math{𝒫@_{∗}(f) = f@_{∗}}.

@image["scribblings/functor/images/f_∗.svg"]{[picture] f_∗.svg}

@racketfile{code/functor/𝒫_∗.rkt}

@subsubsection{Preimage Functor}

The @deftech{preimage} (@deftech{inverse image}) of @math{b_0}, denoted by
@math{f@^{∗}(b_0)}, is the @tech{subset} of @math{a}:
@math{f@^{∗}(b_0) ≔ {x ∈ a | f(x) ∈ b_0}}.

The @deftech{preimage functor} (@deftech{inverse image functor}) @math{𝒫@^{∗}}
takes @math{f} to @math{f@^{∗}}: @math{𝒫@^{∗}(f) = f@^{∗}}.

@image["scribblings/functor/images/f^∗.svg"]{[picture] f^∗.svg}

@racketfile{code/functor/𝒫^∗.rkt}

@subsubsection{Universal Image Functor}

The @deftech{universal image} of @math{a_0}, denoted by @math{f@_{!}(a_0)}, is
the @tech{subset} of @math{b}: @math{f@_{!}(a_0) ≔ {y ∈ b | f@^{∗}({y}) ⊆ a_0}}.

The @deftech{universal image functor} @math{𝒫@_{!}} takes @math{f} to @math{f@_{!}}:
@math{𝒫@_{!}(f) = f@_{!}}.

@image["scribblings/functor/images/f_!.svg"]{[picture] f_!.svg}

@racketfile{code/functor/𝒫_!.rkt}

@subsection{Hom Functor}

The @deftech{hom functor} is a key construction in @tech{category theory}. It
provides a way to associate a @tech{set} of @tech{morphisms} to each pair of
@tech{objects} in a @tech{category}. @tech{Hom functors} play a central role in
studying how @tech{objects} within a @tech{category} are related to each other
via @tech{morphisms}, and they are foundational to concepts like the
@tech{Yoneda Lemma}.

@subsubsection{Covariant Hom Functor}

The @deftech{covariant hom functor} @math{Hom@_{𝒞}(a, -) : 𝒞 → 𝐒𝐞𝐭} maps a
@tech{morphism} @math{j : x → y : 𝒞} to @math{Hom@_{𝒞}(a, j)}.

@image["scribblings/functor/images/Hom_1.svg"]{[picture] Hom_1.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: a 𝒞)
(: |(→𝒞 a _)| (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→ (→𝒞 a x) (→𝒞 a y)))))
(define |(→𝒞 a _)| (curry ∘𝒞))
]

@bold{Exercise}: Prove @math{Hom@_{𝒞}(a, id_x) = id@_{Hom@_{𝒞}(a, x)}}.

@bold{Exercise}: Prove @math{Hom@_{𝒞}(a, j)∘Hom@_{𝒞}(a, g) = Hom@_{𝒞}(a, j∘g)}.

@subsubsection{Contravariant Hom Functor}

The @deftech{contravariant hom functor} @math{Hom@_{𝒞}(-, x) : 𝒞@^{op} → 𝐒𝐞𝐭}
maps a @tech{morphism} @math{i : b → a : 𝒞} to @math{Hom@_{𝒞}(i, x)}.

@image["scribblings/functor/images/Hom_2.svg"]{[picture] Hom_2.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: x 𝒞)
(: |(→𝒞 _ x)| (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→ (→𝒞 a x) (→𝒞 b x)))))
(define |(→𝒞 _ x)| (curryr ∘𝒞))
]

@bold{Exercise}: Prove @math{Hom@_{𝒞}(id_a, x) = id@_{Hom@_{𝒞}(a, x)}}.

@bold{Exercise}: Prove @math{Hom@_{𝒞}(i, x)∘Hom@_{𝒞}(f, x) = Hom@_{𝒞}(f∘i, x)}.

@subsubsection{Two-Variable Hom Functor}

The @deftech{two-variable hom functor} @math{Hom@_{𝒞}(-, -) : 𝒞@^{op}×𝒞 → 𝐒𝐞𝐭}
maps @math{(i, j) : (b, x) → (a, y) : 𝒞×𝒞} to @math{Hom@_{𝒞}(i, j)}.

@image["scribblings/functor/images/Hom_3.svg"]{[picture] Hom_3.svg}

@racketblock[
(: 𝒞 𝐂𝐚𝐭)
(: |(→𝒞 _ _)| (∀ ([a : 𝒞] [b : 𝒞] [x : 𝒞] [y : 𝒞]) (→ (× (→𝒞 b a) (→𝒞 x y)) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (|(→𝒞 _ _)| i j)
  (define |(→𝒞 i j)| (λ (f) (∘𝒞 j f i)))
  |(→𝒞 i j)|)
]

@bold{Exercise}: Prove @math{Hom@_{𝒞}(id_a, id_x) = id@_{Hom@_{𝒞}(a, x)}}.

@bold{Exercise}: Prove the @tech{interchange law}:
@math{Hom@_{𝒞}(i, j)∘Hom@_{𝒞}(f, g) = Hom@_{𝒞}(f∘i, j∘g)}.

@subsection{Cayley's Theorem}

@deftech{Cayley's theorem} in the context of @tech{category theory} states that
every @tech{small category} @math{𝒞} is @tech{isomorphic} to a @tech{subcategory}
@math{𝒮} of @math{𝐒𝐞𝐭}. @math{𝒮} is called the @deftech{Cayley's representation}
of @math{𝒞}.

To demonstrate this, we'll @racket[define] @tech{isomorphisms} @math{H : 𝒞 → 𝒮}
and @math{G : 𝒮 → 𝒞}:

@margin-note{
The definition of @math{G} is pseudocode. Since we treat @tech{sets} and
@tech{functions} as @tech{procedures} here, we cannot @racket[define] @code{dom𝒮}
and @code{choose-id}.
}

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: 𝒮 𝐂𝐚𝐭)

(: H (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝒞 b c) (∀ ([a : 𝒞]) (→ (→𝒞 a b) (→𝒞 a c))))))
(define H (curry ∘𝒞))

(: G (∀ ([b : 𝒞] [c : 𝒞]) (→ (∀ ([a : 𝒞]) (→ (→𝒞 a b) (→𝒞 a c))) (→𝒞 b c))))
(define (G Hg)
  (define Hb (dom𝒮 Hg))
  (define b (choose-id Hb))
  (define g (Hg b))
  g)
]

@bold{Exercise}: Prove @math{H = G@^{–1}} and @math{G = H@^{–1}}.

@bold{Exercise}: Think about the relationships between @math{H} and
@tech{covariant hom functor}.

@math{H} is equal to the @tech{composite} of the @tech{slice functor} @math{𝒞/-}
and the @tech{forgetful functor} @math{U : 𝐂𝐚𝐭 → 𝒮}:

@image["scribblings/functor/images/H_1.svg"]{[picture] H_1.svg}

@racketblock[
(: U (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝐂𝐚𝐭 𝒞/b 𝒞/c) (→𝒮 (H b) (H c)))))
(define (U 𝒞/g)
  (: Hg (∀ ([a : 𝒞]) (→ (→𝒞 a b) (→𝒞 a c))))
  (define (Hg f)
    (define b (cod𝒞 f))
    (define g (caar (𝒞/g `((,b) (,b ,b)))))
    (∘𝒞 g f))
  Hg)
]

@bold{Exercise}: Prove @math{H = U∘𝒞/-}.

Having explored the @tech{Cayley's representation} of @math{𝒞}, we now turn our
attention to its @tech{opposite category} @math{𝒞^op}:

@racketblock[
(: 𝒞 𝐂𝐚𝐭) (: 𝒮 𝐂𝐚𝐭)

(: H (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝒞 a b) (∀ ([c : 𝒞]) (→ (→𝒞 b c) (→𝒞 a c))))))
(define H (curryr ∘𝒞))

(: G (∀ ([b : 𝒞] [a : 𝒞]) (→ (∀ ([c : 𝒞]) (→ (→𝒞 b c) (→𝒞 a c))) (→𝒞 a b))))
(define (G Hf)
  (define Hb (dom𝒮 Hf))
  (define b (choose-id Hb))
  (define f (Hf b))
  f)
]

@bold{Exercise}: Prove @math{H = G@^{–1}} and @math{G = H@^{–1}}.

@bold{Exercise}: Think about the relationships between @math{H} and
@tech{contravariant hom functor}.

@math{H} is equal to the @tech{composite} of the @tech{coslice functor} @math{-/𝒞}
and the @tech{forgetful functor} @math{U : 𝐂𝐚𝐭 → 𝒮}:

@image["scribblings/functor/images/H_2.svg"]{[picture] H_2.svg}

@racketblock[
(: U (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝐂𝐚𝐭 b/𝒞 a/𝒞) (→𝒮 (H b) (H a)))))
(define (U f/𝒞)
  (: Hf (∀ ([c : 𝒞]) (→ (→𝒞 b c) (→𝒞 a c))))
  (define (Hf g)
    (define b (dom𝒞 g))
    (define f (caadr (f/𝒞 `((,b ,b) (,b)))))
    (∘𝒞 g f))
  Hf)
]

@bold{Exercise}: Prove @math{H = U∘-/𝒞}.

@subsection{Action}

@margin-note{
In this context, @tech{actions} are assumed to be @deftech{left action}s by default.
There is also a concept of @deftech{right action}, which is a @tech{function}
@math{β : S×B → S}.
}

An @deftech{action} of a @tech{set} @math{A} on a @tech{set} @math{S} is a
@tech{function} @math{α : A×S → S}. @math{α} shows how each @tech{element} of
@math{A} transforms the @tech{elements} of @math{S} in a consistent manner.

@margin-note{
If @math{M} is a @tech{group}, then @math{α} is a @deftech{group action}.
}

Let @math{M} be a @tech{monoid} @math{(M, ∘, 1)}, a @deftech{monoid action} of
@math{M} on a @tech{set} @math{S} is an @tech{action} @math{α : M×S → S}
satisfying the following properties:

@itemlist[
  #:style 'ordered
  @item{Identity: @math{∀s ∈ S, α(1, s) = s}.}
  @item{Associativity: @math{∀m, n ∈ M, ∀s ∈ S, α(m∘n, s) = α(m, α(n, s))}.}
  ]

These properties ensure that the @tech{monoid action} respects the structure of
the @tech{monoid}, providing a coherent way to apply the @tech{elements} of
@math{M} to the @tech{elements} of @math{S}.

Another common way to denote a @tech{monoid action} is by writing @math{ms} to
represent @math{α(m, s)}. Using this notation, the properties of a
@tech{monoid action} can be restated as follows:

@itemlist[
  #:style 'ordered
  @item{Identity: @math{∀s ∈ S, 1s = s}.}
  @item{Associativity: @math{∀m, n ∈ M, ∀s ∈ S, (m∘n)s = m(ns)}.}
  ]

In this notation, the @tech{monoid action} is described more compactly,
emphasizing the direct application of @tech{elements} from the @tech{monoid}
@math{M} to the @tech{set} @math{S}. @math{S} is also called an @math{M-set} in
this way.

@subsubsection{Monoid Actions as Functors}

To further explore the connection between @tech{monoid actions} and @tech{functors},
we can @racket[curry] @math{α} and obtain a @tech{function} @math{F@_{α1}: M → [S → S]},
where @math{F@_{α1}(m)(s) = α(m, s) = ms}. Next, we can view @math{M} as an @tech{OOC}
@math{C(M)}, and then @racket[define] a @tech{𝐒𝐞𝐭-valued functor} @math{F@_{α}: C(M) → 𝐒𝐞𝐭},
where @math{F@_{α}(∗) = S} and @math{F@_{α}(m) = F@_{α1}(m) = α(m, -)}.

@image["scribblings/functor/images/act.svg"]{[picture] act.svg}

@bold{Exercise}: Prove that @math{F@_{α1}} is a @tech{monoid homomorphism}.

In this way, we can @racket[define] an @tech{action} of the @tech{monoid}
@math{M} on the @tech{object} @math{S} in the @tech{category} @math{𝒞} to be a
@tech{functor} @math{ρ : C(M) → 𝒞}, where @math{ρ(∗) = S}.

A @deftech{category action} of the @tech{category} @math{𝒞} in the @tech{category}
@math{𝒟} is just a @tech{functor} from @math{𝒞} to @math{𝒟}.

@subsubsection{Finite State Machine}

A @deftech{finite state machine} (@deftech{FSM}) @math{ℳ} is a mathematical model
used to design algorithms and systems that can be in one of a finite number of
@tech{states} at any given time. @math{ℳ} can be described as a @deftech{state diagram},
a @deftech{state table}, or a tuple @math{(A, S, s_0, φ)}:

@itemlist[
  @item{@math{A} (@deftech{alphabet}):
        The finite @tech{set} of all possible input @deftech{letter}s that
        @math{ℳ} can read.}
  @item{@math{S} (@deftech{state space}):
        The finite @tech{set} of all @deftech{state}s that @math{ℳ} can be in.}
  @item{@math{s_0} (@deftech{start state}):
        The @deftech{initial state} of @math{ℳ}.}
  @item{@math{φ} (@deftech{transition function}):
        An @tech{action} of @math{A} on @math{S} that defines the @tech{state}
        @deftech{transition} of @math{ℳ}.}
  ]

The @deftech{Kleene star} (@deftech{Kleene closure}) @math{A@^{*}} of a @tech{set}
@math{A} is the @tech{set} of all possible finite-length sequences of @tech{elements}
of @math{A}.

@bold{Exercise}: Think about the relationship between @tech{Kleene star} and
@tech{monoid}.

@math{φ : A×S → S} takes an input @tech{letter} and the current @tech{state} as
arguments and returns the next @tech{state}. By currying @math{φ}, we can view
any @tech{element} in @math{A} as a @tech{transition}. Similarly, we'd like to
find a way to represent a sequence of @tech{transitions}. We @racket[define] the
@tech{function} @math{φ@^{*}: A@^{*}×S → S}:

@itemlist[
  #:style 'ordered
  @item{@math{∀s ∈ S, φ@^{*}((), s) = s}}
  @item{@math{∀s ∈ S, ∀w ∈ A@^{*}, ∀a ∈ A, φ@^{*}((a)w, s) = φ(a, φ@^{*}(w, s))}}
]

@margin-note{
In this context, @tech{FSMs} are assumed to be @deftech{deterministic} by default.
If an @tech{FSM} is @deftech{nondeterministic}, its @math{φ} is a @tech{relation}
rather than a @tech{function}, so @math{φ@^{*}} is a @tech{monoid action} in
@tech{𝐑𝐞𝐥} rather than in @tech{𝐒𝐞𝐭}.
}

@bold{Exercise}: Prove that @math{φ@^{*}} is a @tech{monoid action} of
@math{A@^{*}} on @math{S} in @tech{𝐒𝐞𝐭}.

In addition to the @tech{monoid action} @math{φ@^{*}}, a @tech{FSM} @math{ℳ}
often employ a @deftech{run function} @math{ρ : A@^{*} → S}, which takes a
sequence from @math{A@^{*}} and returns a @deftech{final state} of @math{ℳ}
after processing the entire sequence, starting from the @tech{initial state}
@math{s_0}: @math{∀w ∈ A@^{*}, ρ(w) = φ@^{*}(w, s_0)}.

Here is a Racket example for the @tech{FSM} @math{ℳ_1}
@math{(A_1 = {x, y}, S_1 = {s_1, b_1}, s_1, φ_1)}, which expects the last
@tech{letter} to be not @math{y} (@math{s} means "start" and @math{b} means "bad"):

@image["scribblings/functor/images/ℳ_1.svg"]{[picture] ℳ_1.svg}

@centered{
@tabular[#:sep @hspace[1]
@list[
  @list[@bold{@math{S_1} \ @math{A_1}} @bold{@math{x}} @bold{@math{y}}]
  @list[@bold{@math{s_1}} @math{s_1} @math{b_1}]
  @list[@bold{@math{b_1}} @math{s_1} @math{b_1}]
]]
}

@racketfile{code/functor/FSM.rkt}

@bold{Exercise}: Try to implement another Racket example for the @tech{FSM}
@math{ℳ_2} @math{(A_2 = {x, y}, S_2 = {s_2, b_2, o_2}, s_2, φ_2)}, which expects
the first @tech{letter} to be @math{x} (@math{o} means "ok"):

@image["scribblings/functor/images/ℳ_2.svg"]{[picture] ℳ_2.svg}

@centered{
@tabular[#:sep @hspace[1]
@list[
  @list[@bold{@math{S_2} \ @math{A_2}} @bold{@math{x}} @bold{@math{y}}]
  @list[@bold{@math{s}} @math{o} @math{b}]
  @list[@bold{@math{b}} @math{b} @math{b}]
  @list[@bold{@math{o}} @math{o} @math{o}]
]]
}

@subsubsection{𝐒𝐞𝐭-Valued Functors as Typed Actions}

We've shown that @tech{monoid actions} can be viewed as @tech{functors} from
an @tech{OOC} to @tech{𝐒𝐞𝐭}. Can we view any @tech{𝐒𝐞𝐭-valued functor} as an
@tech{action} that generalizes @tech{monoid actions}? Indeed, we can! Such an
@tech{action} is called a @deftech{typed action}.

A @tech{typed action} of @math{M} on @math{S} involves a type @tech{set} @math{T}
and a @tech{function} @math{type : S → T}. This can be seen as @math{S} being
divided into @tech{subsets} according to their types, and the @tech{elements} of
@math{M} acting on these @tech{subsets}.

For a @tech{functor} @math{F : 𝒞 → 𝐒𝐞𝐭}, we can view it as a @tech{typed action}
in this way: @math{T = 𝒞_0}, @math{M = 𝒞_1}, and @math{S = ∐@_{t∈T}F(t)}, where
@math{F(t) = {s ∈ S | type(s) = t}}.

@image["scribblings/functor/images/typed-act.svg"]{[picture] typed-act.svg}

@subsubsection{Typed Finite State Machine}

@tech{FSMs} are typically characterized by their complete @tech{state tables},
meaning that for every @tech{state} and every input @tech{letter}, there is a
defined @tech{transition} to a @tech{state}.

In a @deftech{typed finite state machine} (@deftech{TFSM}), its @tech{state table}
does not need to be complete. Instead, its @tech{alphabet} and @tech{states} are
typed, meaning that only certain @tech{transitions} are valid. This introduces a
layer of flexibility and specificity in modeling @tech{state} @tech{transitions},
where not every @tech{state} needs to handle every possible input @tech{letter}.
In some contexts, certain @tech{states} might only handle a @tech{subset} of the
@tech{alphabet}, and any undefined @tech{transition} might signify an @racket[error]
or a special condition that needs separate handling.

@margin-note{
In a certain sense, a @tech{set} can be viewed as a @deftech{one-node graph},
where @tech{elements} are @tech{arrows} pointing from the only @tech{node} to
itself.
}

An @tech{FSM} can be represented by a tuple @math{(A, S, s_0, φ)}. In contrast,
a @tech{TFSM} @math{ℳ} can be represented by a tuple @math{(𝒢, S, s_0, φ)},
where:

@itemlist[
  @item{@math{𝒢} (@deftech{typed alphabet}):
        A @tech{graph} whose @tech{arrows} are @tech{letters} and @tech{nodes}
        are types.}
  @item{@math{S} (@tech{state space}):
        The finite @tech{set} of all @tech{states} that @math{ℳ} can be in.}
  @item{@math{s_0} (@tech{start state}):
        The initial @tech{state} of @math{ℳ}.}
  @item{@math{φ} (@deftech{typed transition function}):
        A @tech{graph homomorphism} from @math{𝒢} to @tech{𝐒𝐞𝐭}.}
  ]

We can use a @tech{state diagram} and a @tech{state table} to illustrate a
@tech{TFSM} @math{ℳ}, which can be viewed as a combination of multiple @tech{FSMs}
@math{ℳ_1} and @math{ℳ_2}
(@math{q} means "quit", @math{a} means "accept", and @math{r} means "reject"):

@image["scribblings/functor/images/ℳ.svg"]{[picture] ℳ.svg}

@centered{
@tabular[#:sep @hspace[1]
@list[
  @list[@bold{@math{S} \ @math{𝒢_1}}
             @bold{@math{1}} @bold{@math{2}}
             @bold{@math{q}}
             @bold{@math{x}} @bold{@math{y}}]

  @list[@bold{@math{s_0}} @math{s_1} @math{s_2} @math{} @math{} @math{}]
  @list[@bold{@math{a_0}} @math{s_1} @math{s_2} @math{} @math{} @math{}]
  @list[@bold{@math{r_0}} @math{s_1} @math{s_2} @math{} @math{} @math{}]

  @list[@bold{@math{s_1}} @math{} @math{} @math{a_0} @math{s_1} @math{b_1}]
  @list[@bold{@math{b_1}} @math{} @math{} @math{r_0} @math{s_1} @math{b_1}]

  @list[@bold{@math{s_2}} @math{} @math{} @math{r_0} @math{o_2} @math{b_2}]
  @list[@bold{@math{b_2}} @math{} @math{} @math{r_0} @math{b_2} @math{b_2}]
  @list[@bold{@math{o_2}} @math{} @math{} @math{a_0} @math{o_2} @math{o_2}]
]]
}

Let @math{S_0 = {s_0, a_0, r_0}}, @math{S_1 = {s_1, b_1}} and
@math{S_2 = {s_2, b_2, o_2}}. In the @tech{typed alphabet} @math{𝒢}, @math{S_1}
and @math{S_2} are the @tech{state spaces} of @math{ℳ_1} and @math{ℳ_2}
respectively:

@image["scribblings/functor/images/𝒢.svg"]{[picture] 𝒢.svg}

@margin-note{
For each @tech{node} @math{a}, there is a unique @tech{path} of length @math{0},
called the @deftech{empty path} at @math{a}.
}

Similar to @tech{FSMs}, @math{φ : 𝒢 → 𝐒𝐞𝐭} generates a @tech{typed action}
@math{φ* : F(𝒢) → 𝐒𝐞𝐭}. @math{F(𝒢)} is the @deftech{path category} of @math{𝒢},
where @tech{objects} are @tech{nodes} and @tech{morphisms} are @deftech{paths}
(a sequence of @tech{arrows} connected end to end).

@bold{Exercise}: View a @tech{graph} as a @tech/refer{vector} of @tech{nodes} and
@tech{arrows}, where each @tech{arrow} is represented by a three-element
@tech/refer{vector}, consisting of a @tech/refer{character}
(as the name of the @tech{arrow}), a @tech{source} @tech{node}, and a @tech{target}
@tech{node}, and each @tech{node} is represented as the @tech{arrow} from itself
to itself, with the name @racket[#\nul]. Try to @racket[define] the @tech{procedure}
@racket[F] that constructs the @tech{path category} of @math{𝒢}.

@bold{Exercise}: Try to @racket[define] the @tech{procedure} @racket[make-path],
which takes a @tech{graph}, a @tech{source} @tech{node} and a @tech/refer{string},
and returns the corresponding @tech{path}.

Here is how to implement @math{ℳ} in Racket:

@racketfile{code/functor/TFSM.rkt}
