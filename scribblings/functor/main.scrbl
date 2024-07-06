#lang scribble/manual

@(require (for-label ctp
                     (only-meta-in 0 (except-in typed/racket/no-check =))
                     racket/hash
                     rackunit
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

@margin-note{
The @tech{functors} discussed in this tutorial default to the @deftech{covariant functor}s.
A @deftech{contravariant functor} from @math{𝒞} to @math{𝒟} can be considered as
a @tech{covariant functor} from @math{𝒞@^{op}} to @math{𝒟}. See more in
@hyperlink["https://ncatlab.org/nlab/show/contravariant+functor"]{nLab}.
}

@itemlist[
  #:style 'ordered
  @item{Preservation of @tech{domain} and @tech{codomain}

        For any @tech{morphism} @math{f: a → b} in @math{𝒞}, there is a @tech{morphism}
        @math{F(f): F(a) → F(b)} in @math{𝒟}.

        @image["scribblings/functor/images/F-1.svg"]{[picture] F-1.svg}}
  @item{Preservation of @tech{identity morphisms}

        For any @tech{object} @math{a} in @math{𝒞}, @math{F(id_a) = id@_{F(a)}}.

        @image["scribblings/functor/images/F-2.svg"]{[picture] F-2.svg}}
  @item{Preservation of @tech{composable pairs}

        If @math{(f, g)} is a @tech{composable pair} in @math{𝒞}, then @math{(F(f), F(g))}
        is a @tech{composable pair} in @math{𝒟}, and @math{F(g∘f) = F(g)∘F(f)}.

        @image["scribblings/functor/images/F-3.svg"]{[picture] F-3.svg}}
  ]

Let @math{𝒞_2} be the collection of @tech{composable pairs} in @math{𝒞}. We can
describe @math{𝒞} with the following @tech{diagram}:

@image["scribblings/functor/images/cat.svg"]{[picture] cat.svg}

To illustrate the @tech{functor} @math{F}, consider a @tech{function}
@math{F_2: 𝒞_2 → 𝒟_2}. This allows us to describe @math{F} with the following
@tech{diagram}:

@image["scribblings/functor/images/functor.svg"]{[picture] functor.svg}

@margin-note{
The proof is left as an exercise.
}

We can see that each similarly labeled square in the @tech{diagram} is a
@tech{commutative square}. Additionally, the @tech{commutative squares} show the
properties of @tech{functors}.

The following example illustrates how to implement @tech{functors} in Racket:

@racketfile{code/functor/𝐌𝐚𝐭𝐫→𝐏𝐫𝐨𝐬𝐞𝐭.rkt}

@bold{Exercise}: Prove that @tech{functors} can be @tech[#:key "compose"]{composed}
and that this @tech{composition} is @tech{associative}.

@subsection{Category of Categories}

The @tech{category} of @tech{categories}, denoted as @deftech{𝐂𝐚𝐭}, forms a
higher-level structure where @tech{objects} are @tech{categories} and @tech{morphisms}
are @tech{functors} between them.

In practical implementations using Racket, we'll employ @tech{𝐏𝐫𝐨𝐜} to symbolize
@tech{𝐂𝐚𝐭}. This is because, in Racket, we implement @tech{functors} as
@tech{procedures}. Note that since the task of comparing @tech{procedure}
functionality can only be done by the programmer, we will avoid using @racket[=]
or just use it as pseudocode.

@racketblock[
(code:comment2 "Category of Categories")
(: dom (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) 𝒜)))
(: cod (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) ℬ)))
(: ∘ (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] ... [𝒵 : 𝐂𝐚𝐭]) (→ (× (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 ℬ 𝒞) ...) (→𝐂𝐚𝐭 𝒜 𝒵))))
(: ? (pred (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→𝐂𝐚𝐭 𝒜 ℬ))))
(: = (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] [𝒟 : 𝐂𝐚𝐭] ...) (→ (× (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 𝒞 𝒟) ...) Boolean)))

(code:comment "Categories")
(: 𝒜 𝐂𝐚𝐭) (? 𝒜) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→𝒜 a b)))")
(: ℬ 𝐂𝐚𝐭) (? ℬ) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→ℬ a b)))")
(: 𝒞 𝐂𝐚𝐭) (? 𝒞) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒞 a b)))")
(: 𝒟 𝐂𝐚𝐭) (? 𝒟) (code:comment# "(∀ ([a : 𝒟] [b : 𝒟]) (→ (→𝒟 a b) (→𝒟 a b)))")

(code:comment2 "Functors")
(: F (→𝐂𝐚𝐭 𝒜 ℬ)) (? F) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→ℬ (F a) (F b))))")
(: G (→𝐂𝐚𝐭 ℬ 𝒞)) (? G) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→𝒞 (G a) (G b))))")
(: H (→𝐂𝐚𝐭 𝒞 𝒟)) (? H) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒟 (H a) (H b))))")

(code:comment2 "Existence of composition")
(= ℬ (cod F) (dom G))
(= 𝒜 (dom (∘ G F)) (dom F))
(= 𝒞 (cod (∘ G F)) (cod G))

(code:comment2 "Associativity of composition")
(= (∘ H G F) (∘ (∘ H G) F) (∘ H (∘ G F)))

(code:comment2 "Existence of identity morphisms")
(= 𝒜 (dom 𝒜) (cod 𝒜))

(code:comment2 "Composition and identity morphisms")
(= F (∘ F (dom F)) (∘ (cod F) F))
]

Although we have given specific @secref{Category_Examples}, these examples are
just one way to implement the corresponding concepts. We can @racket[define]
these concepts in other ways as well. These different implementations of the same
concept can be seen as @tech{isomorphic} @tech{objects} in @tech{𝐂𝐚𝐭}.

Therefore, in the following sections, when we discuss specific @tech{categories},
their definitions might differ from the Racket code in the previous sections.
For instance, in the @secref{Category_of_Sets}, @tech{morphisms} of @tech{𝐒𝐞𝐭} are
defined as @racket[hash] tables, but essentially they are mappings and might be
defined as @tech{procedures} in later sections.

@subsubsection{Category of Monoids}

@margin-note{
A @deftech{monoidal homomorphism} @math{f: (A, ∘, a) → (B, ·, b)} is a
@tech{function} that preserves the @tech{monoid} structure:
@math{f(x∘y) = f(x)·f(y)} and @math{f(a) = b}.
}

The @tech{category} of @tech{monoids}, denoted as @deftech{𝐌𝐨𝐧}, where @tech{objects}
are @tech{monoids} and @tech{morphisms} are @tech{monoidal homomorphisms}.
@tech{𝐌𝐨𝐧} can be viewed as the @tech{category} of @tech{OOCs}, and
@tech{monoidal homomorphisms} can be viewed as @tech{functors} between @tech{OOCs}.

@subsection{Forgetful Functor}

A @deftech{forgetful functor} (@deftech{underlying functor} or @deftech{stripping functor})
is a type of @tech{functor} that forgets some or all of the structure of the
@tech{objects} and the structure-preserving @tech{functions} in its @tech{domain}
@tech{category}.

For example, if we forget @tech{morphisms} in a @tech{category}, then we get a
@tech{set}. Extending this idea, we get a @tech{forgetful functor} @math{U: 𝐂𝐚𝐭 → 𝐒𝐞𝐭},
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

@section{𝐒𝐞𝐭-Valued Functor}

A @deftech{𝐒𝐞𝐭-valued functor} on @math{𝒞} is a @tech{functor} from @math{𝒞} to
@tech{𝐒𝐞𝐭}. @tech{𝐒𝐞𝐭-valued functors} have theoretical importance due to the
@tech{Yoneda Lemma}, a fundamental result in @tech{category theory} that will be
introduced in detail in the next chapter.

@subsection{Powerset Functor}

The @deftech{powerset} of a @tech{set} @math{s}, denoted as @math{𝒫(s)}, is the
@tech{set} of all @tech{subsets} of @math{s}.

@racketfile{code/function/𝒫.rkt}

For a @tech{function} @math{f: a → b}, the @deftech{image} of @math{f}, denoted
as @math{im(f)}, is the @tech{subset} of @math{b}:
@math{im(f) := {f(x) | x ∈ a}}.

@image["scribblings/functor/images/f.svg"]{[picture] f.svg}

Let @math{a_0} be a @tech{subset} of @math{a} and @math{b_0} be a @tech{subset}
of @math{b}. There are different @deftech{powerset functor}s, all of which map
@math{s} to @math{𝒫(s)}.

@subsubsection{Direct Image Functor}

The @deftech{direct image} (@deftech{existential image}) of @math{a_0}, denoted
as @math{f@_{∗}(a_0)}, is the @tech{subset} of @math{b}:
@math{f@_{∗}(a_0) := {f(x) | x ∈ a_0}}.

The @deftech{direct image functor} (@deftech{existential image functor})
@math{𝒫@_{∗}} takes @math{f} to @math{f@_{∗}}: @math{𝒫@_{∗}(f) = f@_{∗}}.

@image["scribblings/functor/images/f_∗.svg"]{[picture] f_∗.svg}

@racketfile{code/functor/𝒫_∗.rkt}

@subsubsection{Preimage Functor}

The @deftech{preimage} (@deftech{inverse image}) of @math{b_0}, denoted as
@math{f@^{∗}(b_0)}, is the @tech{subset} of @math{a}:
@math{f@^{∗}(b_0) := {x ∈ a | f(x) ∈ b_0}}.

The @deftech{preimage functor} (@deftech{inverse image functor}) @math{𝒫@^{∗}}
takes @math{f} to @math{f@^{∗}}: @math{𝒫@^{∗}(f) = f@^{∗}}.

@image["scribblings/functor/images/f^∗.svg"]{[picture] f^∗.svg}

@racketfile{code/functor/𝒫^∗.rkt}

@subsubsection{Universal Image Functor}

The @deftech{universal image} of @math{a_0}, denoted as @math{f@_{!}(a_0)}, is
the @tech{subset} of @math{b}: @math{f@_{!}(a_0) := {y ∈ b | f@^{∗}({y}) ⊆ a_0}}.

The @deftech{universal image functor} @math{𝒫@_{!}} takes @math{f} to @math{f@_{!}}:
@math{𝒫@_{!}(f) = f@_{!}}.

@image["scribblings/functor/images/f_!.svg"]{[picture] f_!.svg}

@racketfile{code/functor/𝒫_!.rkt}

@subsection{Hom Functor}

For @tech{objects} @math{a} and @math{x} in @math{𝒞}, the @deftech{hom set},
denoted as @math{Hom@_{𝒞}(a, x)}, is the collection of all @tech{morphisms} from
@math{a} to @math{x}:
@math{Hom@_{𝒞}(a, x) := {f ∈ 𝒞_1 | dom@_{𝒞}(f) = a ∧ cod@_{𝒞}(f) = x}}.

For @tech{morphisms} @math{f: a → x}, @math{i: b → a} and @math{j: x → y}, we can
@racket[define] a @tech{function}
@math{Hom@_{𝒞}(i, j): Hom@_{𝒞}(a, x) → Hom@_{𝒞}(b, y)}, where
@math{Hom@_{𝒞}(i, j)(f) := j∘f∘i}.

@image["scribblings/functor/images/hom_1.svg"]{[picture] hom_1.svg}

Additionally, we can @racket[define] two other @tech{functions}:

@itemlist[
  #:style 'ordered
  @item{@math{Hom@_{𝒞}(a, j) := Hom@_{𝒞}(id@_{a}, j)}, where
        @math{Hom@_{𝒞}(a, j)(f) = j∘f}.

        @image["scribblings/functor/images/hom_2.svg"]{[picture] hom_2.svg}}
  @item{@math{Hom@_{𝒞}(i, x) := Hom@_{𝒞}(i, id@_{x})}, where
        @math{Hom@_{𝒞}(i, x)(f) = f∘i}.

        @image["scribblings/functor/images/hom_3.svg"]{[picture] hom_3.svg}}
  ]

These @tech{functions} provide a foundation for defining @deftech{hom functor}s.

@subsubsection{Covariant Hom Functor}

The @deftech{covariant hom functor} @math{Hom@_{𝒞}(a, -): 𝒞 → 𝐒𝐞𝐭} takes
@math{j} to @math{Hom@_{𝒞}(a, j)}.

@image["scribblings/functor/images/Hom_1.svg"]{[picture] Hom_1.svg}

@racketblock[
(: a 𝒞)
(: Hom𝒞 (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→𝐒𝐞𝐭 (→𝒞 a x) (→𝒞 a y)))))
(define (Hom𝒞 j) (λ (f) (∘𝒞 j f)))
]

@bold{Exercise}: Prove that @math{Hom@_{𝒞}(i, -)} is @bold{not} a @tech{functor}.

@racketblock[
(: b 𝒞) (: a 𝒞) (: i (→𝒞 b a))
(: Hom𝒞 (∀ ([x : 𝒞] [y : 𝒞]) (→ (→𝒞 x y) (→𝐒𝐞𝐭 (→𝒞 a x) (→𝒞 b y)))))
(define (Hom𝒞 j) (λ (f) (∘𝒞 j f i)))
]

@subsubsection{Contravariant Hom Functor}

The @deftech{contravariant hom functor} @math{Hom@_{𝒞}(-, x): 𝒞@^{op} → 𝐒𝐞𝐭} takes
@math{i} to @math{Hom@_{𝒞}(i, x)}.

@image["scribblings/functor/images/Hom_2.svg"]{[picture] Hom_2.svg}

@racketblock[
(: x 𝒞)
(: Hom𝒞 (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→𝐒𝐞𝐭 (→𝒞 a x) (→𝒞 b x)))))
(define (Hom𝒞 i) (λ (f) (∘𝒞 f i)))
]

@bold{Exercise}: Prove that @math{Hom@_{𝒞}(-, j)} is @bold{not} a @tech{functor}.

@racketblock[
(: x 𝒞) (: y 𝒞) (: j (→𝒞 x y))
(: Hom𝒞 (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 b a) (→𝐒𝐞𝐭 (→𝒞 a x) (→𝒞 b y)))))
(define (Hom𝒞 i) (λ (f) (∘𝒞 j f i)))
]

@subsubsection{Two-Variable Hom Functor}

The @deftech{two-variable hom functor} @math{Hom@_{𝒞}(-, -): 𝒞@^{op} × 𝒞 → 𝐒𝐞𝐭}
takes @math{i × j} to @math{Hom@_{𝒞}(i, j)}.

@image["scribblings/functor/images/Hom_3.svg"]{[picture] Hom_3.svg}

@racketblock[
(: Hom𝒞 (∀ ([a : 𝒞] [b : 𝒞] [x : 𝒞] [y : 𝒞]) (→ (× (→𝒞 b a) (→𝒞 x y)) (→𝐒𝐞𝐭 (→𝒞 a x) (→𝒞 b y)))))
(define (Hom𝒞 i j) (λ (f) (∘𝒞 j f i)))
]

@subsection{Cayley's Theorem}

@deftech{Cayley's theorem} in the context of @tech{category theory} states that
every @tech{small category} @math{𝒞} is @tech{isomorphic} to a @tech{subcategory}
of @math{𝐒𝐞𝐭}. This is a powerful result because it allows us to represent abstract
@tech{categories} concretely.

@deftech{Cayley representation} of @math{𝒞}:

@racketblock[
(: H (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝒞 b c) (→𝐒𝐞𝐭 (H b) (H c)))))
(define (H g) (λ (f) (∘𝒞 g f)))
]

@image["scribblings/functor/images/𝒞÷-.svg"]{[picture] 𝒞÷-.svg}

@racketblock[
(: 𝒞/- (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝒞 b c) (→𝐂𝐚𝐭 𝒞/b 𝒞/c))))
(define (𝒞/- g)
  (: 𝒞/g (∀ ([x : 𝒞/b] [y : 𝒞/b]) (→ (→𝒞/b x y) (→𝒞/c (∘𝒞 g x) (∘𝒞 g y)))))
  (define 𝒞/g
    (match-λ
      [`((,x) (,y ,z))
       `((,(∘𝒞 g x)) (,(∘𝒞 g y) ,z))]))
  𝒞/g)
]

@image["scribblings/functor/images/H_1.svg"]{[picture] H_1.svg}

@racketblock[
(: U (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝐂𝐚𝐭 𝒞/b 𝒞/c) (→𝐒𝐞𝐭 (H b) (H c)))))
(define (U 𝒞/g)
  (: Hg (∀ ([a : 𝒞]) (→ (→𝒞 a b) (→𝒞 a c))))
  (define (Hg f)
    (define b (cod𝒞 f))
    (define g (caar (𝒞/g `((,b) (,b ,b)))))
    (∘𝒞 g f))
  Hg)
]

@racketblock[
(: G (∀ ([b : 𝒞] [c : 𝒞]) (→ (→𝐒𝐞𝐭 (H b) (H c)) (→𝒞 b c))))
(define (G Hg)
  (define Hb (dom𝐒𝐞𝐭 Hg))
  (define f (get-an-element Hb))
  (define b (cod𝒞 f))
  (define g (Hg b))
  g)
]

Cayley representation of @math{𝒞^op}:

@racketblock[
(: H (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝒞 a b) (→𝐒𝐞𝐭 (H b) (H a)))))
(define (H f) (λ (g) (∘𝒞 g f)))
]

@image["scribblings/functor/images/-÷𝒞.svg"]{[picture] -÷𝒞.svg}

@racketblock[
(: -/𝒞 (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝒞 a b) (→𝐂𝐚𝐭 b/𝒞 a/𝒞))))
(define (-/𝒞 f)
  (: f/𝒞 (∀ ([x : b/𝒞] [y : b/𝒞]) (→ (→b/𝒞 x y) (→a/𝒞 (∘𝒞 x f) (∘𝒞 y f)))))
  (define f/𝒞
    (match-λ
      [`((,z ,x) (,y))
       `((,z ,(∘𝒞 x f)) (,(∘𝒞 y f)))]))
  f/𝒞)
]

@image["scribblings/functor/images/H_2.svg"]{[picture] H_2.svg}

@racketblock[
(: U (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝐂𝐚𝐭 b/𝒞 a/𝒞) (→𝐒𝐞𝐭 (H b) (H a)))))
(define (U f/𝒞)
  (: Hf (∀ ([c : 𝒞]) (→ (→𝒞 b c) (→𝒞 a c))))
  (define (Hf g)
    (define b (dom𝒞 g))
    (define f (caadr (f/𝒞 `((,b ,b) (,b)))))
    (∘𝒞 g f))
  Hf)
]

@racketblock[
(: G (∀ ([b : 𝒞] [a : 𝒞]) (→ (→𝐒𝐞𝐭 (H b) (H a)) (→𝒞 a b))))
(define (G Hf)
  (define Hb (dom𝐒𝐞𝐭 Hf))
  (define g (get-an-element Hb))
  (define b (dom𝒞 g))
  (define f (Hf b))
  f)
]

@subsection{Action}

@subsubsection{Monoid Action}

@subsubsection{Finite State Machine}

@subsubsection{Typed Action}

@section{Full and Faithful}

@section{Equivalence}

@subsection{Quotient Category}

@subsubsection{Congruence Class}

@subsubsection{Factorization of Functors}

@subsection{Subobject}
