#lang scribble/manual

@(require (for-label ctp (except-in racket/base =) racket/contract))

@title{Appendix}
@defmodule[ctp]

@defproc[(dom [m morphism/c]) morphism/c]{
See @tech{dom}.
}

@defproc[(cod [m morphism/c]) morphism/c]{
See @tech{cod}.
}

@defproc[(∘ [m morphism/c] ...) morphism/c]{
See @tech{∘}.
}

@defproc[(⨾ [m morphism/c] ...) morphism/c]{
See @tech{⨾}.
}

@defproc[(src [m morphism/c]) morphism/c]{
See @tech{src}.
}

@defproc[(tgt [m morphism/c]) morphism/c]{
See @tech{tgt}.
}

@defproc[(∙ [m morphism/c] ...) morphism/c]{
See @tech{∙}.
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
