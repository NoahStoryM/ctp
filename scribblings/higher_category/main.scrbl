#lang scribble/manual

@(module for-label typed/racket/base/no-check
   (require ctp
            (only-in typed/racket/base
                     require/typed
                     require/typed/provide)
            racket/promise
            racket/set
            math/array
            math/matrix
            rackunit)
   (provide (all-from-out ctp
                          typed/racket/base/no-check
                          typed/racket/base
                          racket/promise
                          racket/set
                          rackunit
                          math/array
                          math/matrix)))
@(require (for-label (only-meta-in 0 'for-label))
          "../ctp-utils.rkt")

@title[#:tag "_Higher_Category_"]{Higher Category}

In this @seclink["_Higher_Category_"]{chapter}, we'll explore more sophisticated
and generalized frameworks such as @tech{2-categories} and @tech{bicategories}.
These higher-dimensional constructs extend the notion of @tech{categories} by
introducing additional layers of structure, enabling us to model more complex
relationships and interactions between @tech{objects} and @tech{morphisms}
through a @deftech{higher category}.

@local-table-of-contents[]

@section{2-Category}

A @tech{set} (@deftech{0-category}) is defined by a @tech{collection} of
@tech{elements} (@deftech{0-cell}s). Extending this idea, a @tech{category}
(@deftech{1-category}) is defined by two @tech{collections}: @tech{objects}
(@tech{0-cells}) and @tech{morphisms} (@deftech{1-cell}s). Importantly, in a
@tech{category}, @tech{objects} can be seen as @tech{identity morphisms}, a
special case of @tech{morphisms}.

This natural progression leads us to consider whether we can extend our
abstraction to include @deftech{2-morphism}s (@deftech{2-cell}s). In other words,
we can think about constructing a @tech{2-category}, which is defined by not
only @tech{0-cells} and @tech{1-cells} but also @tech{2-cells}. Just as in a
@tech{1-category}, each @tech{0-cell} is an @deftech{identity 1-cell}, in a
@tech{2-category}, each @tech{1-cell} is an @deftech{identity 2-cell}.

We already have an example of such a structure: @tech{𝐂𝐚𝐭}. In @tech{𝐂𝐚𝐭},
@tech{categories} serve as @tech{0-cells}, @tech{functors} act as @tech{1-cells},
and @tech{natural transformations} provide the additional layer of abstraction
as @tech{2-cells}. This makes @tech{𝐂𝐚𝐭} a natural reference for understanding
the concept of @tech{2-categories}.

To formalize this idea, we look at how @tech{𝐂𝐚𝐭} operates. There are two distinct
@tech{composition} operations for @tech{natural transformations} within @tech{𝐂𝐚𝐭}:
@tech{horizontal composition} and @tech{vertical composition}. The interaction
between these two forms of @tech{composition} follows the @tech{interchange law}.
We can describe @tech{𝐂𝐚𝐭} in terms of three interrelated @tech{categories}: the
@tech{base category} @tech{𝐂𝐚𝐭ᵇ}, the @tech{horizontal category} @tech{𝐂𝐚𝐭ʰ},
and the @tech{vertical category} @tech{𝐂𝐚𝐭ᵛ}.

@margin-note{
Note that @math{𝒞_2} of a @tech{1-category} @math{𝒞} is the @tech{collection} of
@tech{composable pairs}.
}

Using these properties, we define a @deftech{2-category} @math{𝐂} as a structure
consisting of three @tech{collections}: @math{𝐂_i} of @deftech{i-morphism}s
(@deftech{i-cell}s) for @math{i = 0, 1, 2}. In @math{𝐂}, there are two ways to
@tech{compose} @tech{2-cells}: @tech{horizontal composition} and
@tech{vertical composition}, which satisfy the @tech{interchange law}.
Additionally, @math{𝐂} can be described in terms of three @tech{1-categories}:

@itemlist[
  @item{The @deftech{base category} @math{𝐂^b}:
        @math{𝐂^b_0 = 𝐂_0} and @math{𝐂^b_1 = 𝐂_1}.}
  @item{The @deftech{horizontal category} @math{𝐂^h}:
        @math{𝐂^h_0 = 𝐂_0} and @math{𝐂^h_1 = 𝐂_2}.}
  @item{The @deftech{vertical category} @math{𝐂^v}:
        @math{𝐂^v_0 = 𝐂_1} and @math{𝐂^v_1 = 𝐂_2}.}
  ]

An @tech{isomorphism} @math{α : F ⇒ G} in @math{𝐂^v} is called a
@deftech{2-isomorphism}, and @math{F} and @math{G} are @deftech{2-isomorphic} to
each other.

In a @tech{category} @math{𝒞}, the @tech{morphisms} from @math{a} to @math{x}
form a @tech{hom set} @math{𝒞(a, x)}. This structure naturally extends in a
@tech{2-category} @math{𝐂}: the @tech{1-cells} from @math{𝒜} to @math{𝒳} and
their corresponding @tech{2-cells} form a @deftech{hom category} @math{𝐂(𝒜, 𝒳)},
where the @tech{composition} of @tech{morphisms} is precisely the
@tech{vertical composition} of @tech{2-cells} in @math{𝐂}.

@bold{Exercise}: Show that every @tech{functor category} @math{[𝒞 → 𝒟]} is the
@tech{hom category} @math{𝐂𝐚𝐭(𝒞, 𝒟)}.

To verify the properties of @tech{2-categories}, we @racket[define] some
@tech{check} @tech{procedures} to automate the testing of essential properties
within a @tech{2-category}:

@racketfile{code/higher_category/check.rkt}

The following code demonstrates a @tech{2-category} example in Racket:

@racketfile{code/higher_category/PRel.rkt}

In a general @tech{2-category}, we may not know the specific internal structure
of the @tech{1-cells}. However, we can draw inspiration from the concept of
@tech{global elements}. In @tech{𝐂𝐚𝐭}, any @tech{category} @math{𝒞} is
@tech{isomorphic} to @math{𝒞^1}. This observation motivates us to define a similar
concept in any @tech{2-category} @math{𝐂} that contains a @tech{terminal object}
@tech{1}. Specifically, for any @tech{0-cell} @math{𝒞 : 𝐂}, we define the
@tech{1-cells} from @tech{1} to @math{𝒞} as the @deftech{global object}s of
@math{𝒞}, and the @tech{2-cells} between them as the @deftech{global morphism}s
of @math{𝒞}.

Having introduced the concept of @tech{2-categories}, we naturally consider the
mappings between @tech{2-categories}. Just as @deftech{0-functor}s (@tech{functions})
map between @tech{0-categories} and @deftech{1-functor}s (@tech{functors}) map
between @tech{1-categories} by preserving their structure, @tech{2-functors} map
between @tech{2-categories}, preserving the richer structure.

To define a @tech{2-functor}, we note that a @tech{2-category} @math{𝐂} consists
of three @tech{collections}: @math{𝐂_0}, @math{𝐂_1} and @math{𝐂_2}. Consequently,
a @deftech{2-functor} @math{F : 𝐂 → 𝐃} consists of three @tech{functions}:
@math{F_0 : 𝐂_0 → 𝐃_0}, @math{F_1 : 𝐂_1 → 𝐃_1}, and @math{F_2 : 𝐂_2 → 𝐃_2}.
Additionally, @math{F} can be described in terms of three @tech{1-functors}:

@itemlist[
  @item{The @deftech{base functor} @math{F^b : 𝐂^b → 𝐃^b}:
        @math{F^b_0 = F_0} and @math{F^b_1 = F_1}.}
  @item{The @deftech{horizontal functor} @math{F^h : 𝐂^h → 𝐃^h}:
        @math{F^h_0 = F_0} and @math{F^h_1 = F_2}.}
  @item{The @deftech{vertical functor} @math{F^v : 𝐂^v → 𝐃^v}:
        @math{F^v_0 = F_1} and @math{F^v_1 = F_2}.}
]

@subsection{Strict Monoidal Category}

A @deftech{strict monoidal category} @math{(𝒞, ⊗, I)} is a @tech{category}
@math{𝒞} equipped with a @deftech{tensor product} @deftech{⊗} and a
@deftech{tensor unit} @math{I}. The @tech{tensor product} is a @tech{functor}
@math{⊗ : 𝒞×𝒞 → 𝒞}, and the @tech{tensor unit} is a @deftech{unit object}
@math{I : 𝒞}, such that for all @tech{morphisms} @math{f, g, h} in @math{𝒞},
@math{(f⊗g)⊗h = f⊗(g⊗h)} and @math{f = f⊗id_I = id_I⊗f}.

@image["scribblings/higher_category/images/mon-cat.svg"]{[picture] mon-cat.svg}

@bold{Exercise}: Prove the @tech{interchange law}:
@math{(g_0⊗g_1)∘(f_0⊗f_1) = (g_0∘f_0)⊗(g_1∘f_1)}.

If @math{𝒞} is a @tech{discrete category}, i.e., a @tech{set}, then the
@tech{strict monoidal category} @math{(𝒞, ⊗, I)} reduces to a @deftech{monoidal set}.
In this case, @tech{⊗} becomes an @tech{associative binary operation} and @math{I}
becomes the @tech{identity element} of @math{𝒞}. This structure corresponds
exactly to what we call a @tech{monoid}. Hence, @tech{monoidal set} and
@tech{monoid} are the same concept.

Just as a @tech{one-object category} @math{𝒞} can be viewed as a @tech{monoid}
@math{(𝒞_1, ∘, id@_{∗})}, we extend this idea to view a @deftech{one-object 2-category}
@math{𝐂} as a @tech{strict monoidal category} @math{(𝐂^v, ∘, id^b@_{∗})}.
In this context, the @tech{vertical category} @math{𝐂^v} is equipped with the
@tech{horizontal composition} @math{∘}, which acts as the @tech{tensor product},
and the @tech{identity} @tech{1-cell} @math{id^b@_{∗}}, which serves as the
@tech{tensor unit}.

The following is an example of a @tech{strict monoidal category}:

@racketfile{code/higher_category/Matr.rkt}

@bold{Exercise}: Show that every @tech{endofunctor category} is a
@tech{strict monoidal category}.

@subsubsection{Strict Symmetric Monoidal Category}

A @deftech{strict symmetric monoidal category} @math{(𝒞, ⊗, I)} is a
@tech{strict monoidal category} that @tech{⊗} is @tech{symmetric}: for all
@tech{morphisms} @math{f, g} in @math{𝒞}, @math{f⊗g = g⊗f}.

@subsection{String Diagram}

Traditional @tech{diagrams} represent @tech{0-cells} as @tech{nodes},
@tech{1-cells} as single @tech{arrows} between these @tech{nodes}, and
@tech{2-cells} as double @tech{arrows} between these single @tech{arrows}.
In contrast, @deftech{string diagrams} provide a more intuitive and geometrical
representation:

@itemlist[
  @item{@tech{0-cell} is represented by a portion of a plane.}
  @item{@tech{1-cell} is represented by a @deftech{string} separating the plane in two.}
  @item{@tech{2-cell} is represented by an intersection of @tech{strings}.}
]

@tech{String diagrams} are a powerful tool for visualizing relationships between
@tech{i-cells} within a @tech{2-category} @math{𝐂}. Below, we illustrate a
@tech{2-cell} @math{α : F ⇒ G : 𝒞 → 𝒟 : 𝐂}, using both a traditional
@tech{diagram} and a corresponding @tech{string diagram}:

@margin-note{
By default, @tech{string diagrams} are read from right to left and from bottom
to top.
}

@image["scribblings/higher_category/images/alpha.svg"]{[picture] alpha.svg}

We also use special notations in @tech{string diagrams}:

@itemlist[
  @item{The @tech{identity} @tech{1-cell} is represented by a dashed line or may
        be omitted entirely for simplicity.}
  @item{Curved lines or arcs can also be used to represent @tech{1-cells}, and
        their intersection represents a @tech{2-cell}.}
  ]

The following two examples illustrate the special notations used in
@tech{string diagrams}. These @tech{string diagrams} show @tech{equivalent} but
visually distinct representations of the same structures.

The first one shows a @tech{2-cell} @math{α : G∘F ⇒ id@_{𝒞}}, where
@math{F : 𝒞 → 𝒟 : 𝐂} and @math{G : 𝒟 → 𝒞 : 𝐂}:

@image["scribblings/higher_category/images/alpha_0.svg"]{[picture] alpha_0.svg}
@image["scribblings/higher_category/images/alpha_1.svg"]{[picture] alpha_1.svg}
@image["scribblings/higher_category/images/alpha_2.svg"]{[picture] alpha_2.svg}
@image["scribblings/higher_category/images/alpha_3.svg"]{[picture] alpha_3.svg}
@image["scribblings/higher_category/images/alpha_4.svg"]{[picture] alpha_4.svg}

The second one shows two @tech{2-cells} @math{α : G∘F ⇒ id@_{𝒞}} and
@math{β : id@_{𝒟} ⇒ H∘G}, where @math{F : 𝒞 → 𝒟 : 𝐂}, @math{G : 𝒟 → 𝒞 : 𝐂},
and @math{H : 𝒞 → 𝒟 : 𝐂}:

@image["scribblings/higher_category/images/beta&alpha_0.svg"]{[picture] beta&alpha_0.svg}
@image["scribblings/higher_category/images/beta&alpha_1.svg"]{[picture] beta&alpha_1.svg}
@image["scribblings/higher_category/images/beta&alpha_2.svg"]{[picture] beta&alpha_2.svg}
@image["scribblings/higher_category/images/beta&alpha_3.svg"]{[picture] beta&alpha_3.svg}
@image["scribblings/higher_category/images/beta&alpha_4.svg"]{[picture] beta&alpha_4.svg}

The advantage of using @tech{string diagrams} lies in their simplicity when
representing complex structures in a @tech{2-category}. Instead of working with
layers of @tech{nodes} and @tech{arrows}, @tech{string diagrams} allow us to
represent these relationships in a clear, visual manner that highlights how each
part of the structure interacts with the others.

@bold{Exercise}: The following is a @tech{string diagram}, try to draw the
corresponding @tech{diagram}.

@image["scribblings/higher_category/images/str-diag.svg"]{[picture] str-diag.svg}

If there are no @tech{2-cells} in a @tech{string diagram}, we can further
compress it for simplicity. Specifically:

@itemlist[
  @item{@tech{0-cell} is represented by a line segment.}
  @item{@tech{1-cell} is represented by a point separating the line in two.}
]

This compressed representation is not limited to @math{𝐂^b} but can also be
extended to other @tech{1-categories}.

@subsection{Equivalence}

In a @tech{2-category} @math{𝐂}, @tech{equivalence} is a weaker version of
@tech{isomorphism}. For @tech{1-cells} @math{F: 𝒞 → 𝒟 : 𝐂} and @math{G: 𝒟 → 𝒞 : 𝐂},
if @math{id@_{𝒞} ≅ G∘F} and @math{F∘G ≅ id@_{𝒟}}, then @math{F} and @math{G} are
both @deftech{equivalence}s (often called be @deftech{weakly invertible}).

@image["scribblings/higher_category/images/eqv_1.svg"]{[picture] eqv_1.svg}
@image["scribblings/higher_category/images/eqv_2.svg"]{[picture] eqv_2.svg}

In this case, both @math{F} and @math{G} are @tech{inverses} up to
@tech{2-isomorphisms} @math{η : id@_{𝒞} ⇒ G∘F} and @math{ϵ : F∘G ⇒ id@_{𝒟}}.
@math{G} is a @deftech{pseudo-inverse} of @math{F}, and @math{F} is a
@tech{pseudo-inverse} of @math{G}. @math{𝒞} and @math{𝒟} are @deftech{equivalent}
to each other (@math{𝒞 @deftech{≃} 𝒟}) if there exists an @tech{equivalence}
between them.

@bold{Exercise}: Prove that if @math{η} and @math{ϵ} are @tech{identities}, then
@math{𝒞 ≅ 𝒟}.

@bold{Exercise}: Prove that @tech{≃} is an @tech{equivalence relation} over
@math{𝐂_0}.

@bold{Exercise}: Prove that every @tech{0-cell} is @tech{equivalent} to itself.

@bold{Exercise}: Prove that the @tech{pseudo-inverse} of an @tech{equivalence} is
not unique.

@subsubsection{Equivalence of Categories}

In a @tech{category}, we often focus on its "essential structure" by treating
@tech{isomorphic} @tech{objects} as the same. To formalize this idea, we
introduce the concept of a @tech{skeleton}.

A @deftech{skeleton} of a @tech{category} @math{𝒞} is a @tech{full subcategory},
denoted by @math{sk@_{𝒞}}, where any two @tech{isomorphic} @tech{objects} are
@tech{equal}. A @tech{category} is called a @deftech{@deftech{skeletal} category}
if all its @tech{isomorphisms} are @tech{automorphisms}. More strictly, a
@tech{category} is called a @deftech{@deftech{gaunt} category}
(@deftech{@deftech{stiff} category}) if all its @tech{isomorphisms} are
@tech{identities}.

@image["scribblings/higher_category/images/skel.svg"]{[picture] skel.svg}

A @tech{skeleton} @math{sk@_{𝒞}} comes with a @tech{functor} @math{S : 𝒞 → sk@_{𝒞}},
which is @tech{fully faithful}, and @tech{surjective} on @tech{objects}. This
means that @math{S} preserves the structure of @math{𝒞} while collapsing
@tech{isomorphic} @tech{objects} into a single entity. Conversely, by involving
the @tech{axiom of choice}, we can define an @tech{inclusion functor}
@math{I : sk@_{𝒞} → 𝒞}.

@bold{Exercise}: Prove @math{S∘I = id@_{sk@_{𝒞}}} and @math{I∘S ≅ id@_{𝒞}}.

By constructing a @tech{skeleton} @math{sk@_{𝒞}}, we capture the
"essential structure" of @math{𝒞}. A natural question arises: if
@math{sk@_{𝒞} ≅ sk@_{𝒟}}, what is the relationship between @math{𝒞} and @math{𝒟}?
This relationship is precisely @tech{equivalence}: since @math{𝒞 ≃ sk@_{𝒞}},
@math{sk@_{𝒞} ≅ sk@_{𝒟}}, and @math{sk@_{𝒟} ≃ 𝒟}, it follows by
@tech{transitivity} that @math{𝒞 ≃ 𝒟}.

Conversely, we can also show that if @math{𝒞 ≃ 𝒟}, then their @tech{skeletons}
are @tech{isomorphic} to each other. Assume we have @tech{functors}
@math{S : 𝒞 → sk@_{𝒞}} and @math{T : 𝒟 → sk@_{𝒟}}, as well as the
@tech{inclusion functors} @math{I : sk@_{𝒞} → 𝒞} and @math{J : sk@_{𝒟} → 𝒟},
and @tech{equivalences} @math{F : 𝒞 → 𝒟} and @math{G : 𝒟 → 𝒞}. We can then
construct @tech{functors} @math{TFI : sk@_{𝒞} → sk@_{𝒟}} and
@math{SGJ : sk@_{𝒟} → sk@_{𝒞}}.

@image["scribblings/higher_category/images/eqv-es.svg"]{[picture] eqv-es.svg}

These satisfy: @math{id@_{sk@_{𝒞}} = SI ≅ SGFI ≅ SGJTFI = SGJ∘TFI} and
@math{TFI∘SGJ = TFISGJ ≅ TFGJ ≅ TJ = id@_{sk@_{𝒟}}}. Thus, @math{sk@_{𝒞} ≃ sk@_{𝒟}}.
Moreover, by definition, all @tech{objects} in @math{sk@_{𝒞}} and @math{sk@_{𝒟}}
are only @tech{isomorphic} to themselves, so @math{sk@_{𝒞} ≅ sk@_{𝒟}}.

@margin-note{
This proposition relies on the @tech{axiom of choice}. To avoid this assumption,
@math{F} can be required to be @deftech{split essentially surjective}.
For further details, see
@hyperlink["https://ncatlab.org/nlab/show/split essentially surjective"]{nLab}.
}

@bold{Exercise}: Prove that a @tech{functor} @math{F} is @tech{weakly invertible}
iff it is @tech{fully faithful} and @tech{essentially surjective}.

@image["scribblings/higher_category/images/eqv.svg"]{[picture] eqv.svg}

@section{Bicategory}

By considering @tech{vertical composition}, we can examine whether two
@tech{1-cells} are @tech{isomorphic}. Since @tech{≅} serves as a weakened form of
@tech{=}, this motivates us to replace structures originally defined by the
equality of @tech{1-cells} with those defined via @tech{isomorphism}, thereby
obtaining weaker structures.

We apply this idea to the definition of a @tech{category}. In a @tech{category},
the @tech{composition} of @tech{morphisms} must satisfy the following lows:

@itemlist[
  @item{@math{(h∘g)∘f = h∘(g∘f)}}
  @item{@math{f = f∘id_a = id_b∘f}}
]

@margin-note{
A @tech{bicategory} is also called a @deftech{weak 2-category}, and
a @tech{2-category} is also called a @deftech{strict 2-category}。
}

To derive a weaker structure by using @tech{≅}, we require
@tech{vertical composition}. The @tech{horizontal composition} is then weakened,
leading to a @tech{bicategory}, where the @tech{horizontal composition} of
@tech{1-cells} must satisfy the following laws:

@itemlist[
  @item{@math{(h∘g)∘f ≅ h∘(g∘f)}}
  @item{@math{f ≅ f∘id_a ≅ id_b∘f}}
]

To formally define a @deftech{bicategory} @math{𝐁}, we proceed as follows:

@itemlist[
  #:style 'ordered
  @item{@tech{0-cells}

        The @tech{collection} of @tech{0-cells} is denoted as @math{𝐁_0}.}
  @item{@tech{Hom Categories}

        For each pair of @tech{0-cells} of @math{x, y} in @math{𝐁}, there exists
        a @tech{hom category} @math{𝐁(x, y)}. The @tech{objects} in each
        @tech{hom category} are called @tech{1-cells}, denoted as @math{𝐁_1},
        and the @tech{morphisms} are called @tech{2-cells}
        (@tech{identity morphisms} are called @tech{identity 2-cells}), denoted
        as @math{𝐁_2}. The @tech{vertical composition} of @tech{2-cells} is given
        by the @tech{composition} within each @tech{hom category}.}
  @margin-note{
  In practice, the @tech{identity 1-cell} at @math{x} is often denoted
  simply as @math{id_x}, omitting explicit reference to the @tech{functor}.
  }
  @item{@tech{Identity 1-cells}

        For each @tech{0-cell} @math{x} in @math{𝐁}, there is an associated
        @tech{identity 1-cell}. Specifically, there is a @tech{functor}

        @centered{@math{id_x : 1 → 𝐁(x, x)}}

        that maps @tech{∗} to the @tech{identity 1-cell}
        at @math{x}.}
  @item{@tech{Horizontal Composition}

        For each triple of @tech{0-cells} @math{x, y, z} in @math{𝐁}, there is a
        @tech{functor}

        @centered{@math{c_xyz : 𝐁(y, z) × 𝐁(x, y) → 𝐁(x, z)}}

        called the @tech{horizontal composition}.}
  @item{@deftech{Associator}

        For each quadruple of @tech{0-cells} @math{w, x, y, z} in @math{𝐁}, there
        is a @tech{natural isomorphism}

        @centered{@math{a_wxyz : c_wxz∘(c_xyz×id@_{𝐁(w, x)}) ⇒ c_wyz∘(id@_{𝐁(y, z)}×c_wxy)
                               : 𝐁(y, z) × 𝐁(x, y) × 𝐁(w, x) → 𝐁(w, z)}}

        called the @tech{associator}.}
  @item{@deftech{Unitor}s

        For each pair of @tech{0-cells} @math{x, y} in @math{𝐁}, there are
        @tech{natural isomorphisms}

        @centered{@math{l_xy : c_xyy∘(id_y×id@_{𝐁(x, y)}) ⇒ id@_{𝐁(x, y)}
                             : 𝐁(x, y) → 𝐁(x, y)}}

        and

        @centered{@math{r_xy : c_xxy∘(id@_{𝐁(x, y)}×id_x) ⇒ id@_{𝐁(x, y)}
                             : 𝐁(x, y) → 𝐁(x, y)}}

        called the @deftech{left unitor} and the @deftech{right unitor} respectively.}
]

@;; To ensure the compatibility of the above data, they are required to satisfy the
@;; following two axioms:
@;;
@;; @itemlist[
@;;   #:style 'ordered
@;;   @item{@deftech{Unity Axiom}
@;;
@;;         }
@;;   @item{@deftech{Pentagon Axiom}
@;;
@;;         }
@;; ]

@;; @subsection{Monoidal Category}
@;;
@;; @subsubsection{Symmetric Monoidal Category}
@;;
@;; @subsubsection{Monoid Object}
@;;
@;; @section{Enriched Category}
@;;
@;; @subsection{Enrich Over}
