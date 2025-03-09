#lang scribble/manual

@(module for-label racket/base
   (require ctp racket/contract)
   (provide (all-from-out ctp racket/base racket/contract)))
@(require (for-label 'for-label))

@title{Appendix}
@defmodule[ctp]

@defproc[(dom [m morphism/c]) morphism/c]{
See @tech{dom}.
}

@defproc[(cod [m morphism/c]) morphism/c]{
See @tech{cod}.
}

@defproc[(domᵇ [m morphism/c]) morphism/c]{
See @tech{domᵇ}.
}

@defproc[(codᵇ [m morphism/c]) morphism/c]{
See @tech{codᵇ}.
}

@defproc[(domʰ [m morphism/c]) morphism/c]{
See @tech{domʰ}.
}

@defproc[(codʰ [m morphism/c]) morphism/c]{
See @tech{codʰ}.
}

@defproc[(domᵛ [m morphism/c]) morphism/c]{
See @tech{domᵛ}.
}

@defproc[(codᵛ [m morphism/c]) morphism/c]{
See @tech{codᵛ}.
}

@defproc[(∘ [m morphism/c] ...) morphism/c]{
See @tech{∘}.
}

@defproc[(⨾ [m morphism/c] ...) morphism/c]{
See @tech{⨾}.
}

@defproc[(∙ [m morphism/c] ...) morphism/c]{
See @tech{∙}.
}

@defproc[(⊗ [m morphism/c] ...) morphism/c]{
See @tech{⊗}.
}

@defproc[(? [v any/c]) boolean?]{
See @tech{?}.
}

@defproc[(= [m morphism/c] ...+) boolean?]{
See @tech{=}.
}

@defproc[(∼ [m morphism/c] ...+) boolean?]{
See @tech{∼}.
}

@defproc[(† [𝒞 category/c]) category/c]{
See @tech{†}.
}

@defproc[(÷ [∼ ∼/c]) (-> category/c category/c)]{
See @tech{÷}.
}

@defproc[(⊆ [? ?/c]) (-> category/c category/c)]{
See @tech{⊆}.
}

@defproc[(× [v any/c] ...) any]{
An alias for @racket[values].
}
