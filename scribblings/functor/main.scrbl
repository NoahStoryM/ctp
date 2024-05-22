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
  @item{Preservation of @tech{composition}

        If @math{(f, g)} is a @tech{composable pair} in @math{𝒞}, then @math{(F(f), F(g))}
        is a @tech{composable pair} in @math{𝒟}, and @math{F(g∘f) = F(g)∘F(f)}.

        @image["scribblings/functor/images/F-3.svg"]{[picture] F-3.svg}}
  ]

The following example illustrates how to implement @tech{functors} in Racket:

@racketfile{functor/code/𝐌𝐚𝐭𝐫→𝐑𝐞𝐥.rkt}

@bold{Exercise}: Prove that @tech{functors} can be @tech[#:key "compose"]{composed}
and that this @tech{composition} is @tech{associative}.

@subsection{Category of Categories}

The @tech{category} of @tech{categories}, denoted as @deftech{𝐂𝐚𝐭}, forms a
higher-level structure where @tech{objects} are @tech{categories} and @tech{morphisms}
are @tech{functors} between those @tech{categories}.

In practical implementations using Racket, we'll employ @tech{𝐏𝐫𝐨𝐜} to symbolize
@tech{𝐒𝐞𝐭} and @tech{𝐂𝐚𝐭}. This is because, in Racket, we implement @tech{functions}
and @tech{functors} as @tech{procedures}. Note that since the task of comparing
@tech{procedure} functionality can only be done by the programmer, we will avoid
using @racket[=] or just use it as pseudocode.

@racketblock[
(code:comment2 "Category of Categories")
(: dom (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) 𝒜)))
(: cod (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) ℬ)))
(: ∘ (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] ... [𝒵 : 𝐂𝐚𝐭]) (→ (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 ℬ 𝒞) ... (→𝐂𝐚𝐭 𝒜 𝒵))))
(: ? (pred (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭]) (→𝐂𝐚𝐭 𝒜 ℬ))))
(: = (∀ ([𝒜 : 𝐂𝐚𝐭] [ℬ : 𝐂𝐚𝐭] [𝒞 : 𝐂𝐚𝐭] [𝒟 : 𝐂𝐚𝐭] ...) (→ (→𝐂𝐚𝐭 𝒜 ℬ) (→𝐂𝐚𝐭 𝒞 𝒟) ... Boolean)))

(code:comment "Categories")
(: 𝒜 𝐂𝐚𝐭) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→𝒜 a b)))")
(: ℬ 𝐂𝐚𝐭) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→ℬ a b)))")
(: 𝒞 𝐂𝐚𝐭) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒞 a b)))")
(: 𝒟 𝐂𝐚𝐭) (code:comment# "(∀ ([a : 𝒟] [b : 𝒟]) (→ (→𝒟 a b) (→𝒟 a b)))")

(code:comment2 "Functors")
(: F (→𝐂𝐚𝐭 𝒜 ℬ)) (code:comment# "(∀ ([a : 𝒜] [b : 𝒜]) (→ (→𝒜 a b) (→ℬ (F a) (F b))))")
(: G (→𝐂𝐚𝐭 ℬ 𝒞)) (code:comment# "(∀ ([a : ℬ] [b : ℬ]) (→ (→ℬ a b) (→𝒞 (G a) (G b))))")
(: H (→𝐂𝐚𝐭 𝒞 𝒟)) (code:comment# "(∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝒟 (H a) (H b))))")

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

@subsection{Forgetful Functor}

@subsection{Free Functor}

@subsubsection{The Map-Lifting Property}

@subsubsection{Universal Mapping Property}

@section{𝐒𝐞𝐭-Valued Functor}

@subsection{Powerset Functor}

@racketfile{functor/code/𝒫.rkt}

@subsection{Hom Functor}

@subsubsection{Covariant Hom Functor}

@subsubsection{Contravariant Hom Functor}

@subsubsection{Two-Variable Hom Functor}

@racketblock[
(: Hom𝒞 (∀ ([b : 𝒞] [a : 𝒞] [x : 𝒞] [y : 𝒞]) (→ (× (→𝒞 b a) (→𝒞 x y)) (→ (→𝒞 a x) (→𝒞 b y)))))
(define (Hom𝒞 i j) (λ (f) (∘𝒞 j f i)))
]

@subsection{Representable Functor}

@subsection{Cayley Representation}

@image["scribblings/functor/images/𝒞÷-.svg"]{[picture] 𝒞÷-.svg}

@racketblock[
(: 𝒞/- (∀ ([a : 𝒞] [b : 𝒞]) (→ (→𝒞 a b) (→𝐂𝐚𝐭 (𝒞/- a) (𝒞/- b)))))
(define (𝒞/- f)
  (: 𝒞/f (∀ ([x : 𝒞/a] [y : 𝒞/a]) (→ (→𝒞/a x y) (→𝒞/b (𝒞/f x) (𝒞/f y)))))
  (define (𝒞/f t)
    (match t
      [`((,x) (,y ,g))
       `((,(∘𝒞 f x)) (,(∘𝒞 f y) ,g))]))
  𝒞/f)
]
