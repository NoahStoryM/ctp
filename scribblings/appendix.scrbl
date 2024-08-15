#lang scribble/manual

@(require (for-label ctp (except-in racket/base =) racket/contract))

@title{Appendix}
@defmodule[ctp]

@defproc[(dom [m morphism?]) morphism?]{
See @tech{dom}.
}

@defproc[(cod [m morphism?]) morphism?]{
See @tech{cod}.
}

@defproc[(∘ [m morphism?] ...) morphism?]{
See @tech{∘}.
}

@defproc[(src [m morphism?]) morphism?]{
See @tech{src}.
}

@defproc[(tgt [m morphism?]) morphism?]{
See @tech{tgt}.
}

@defproc[(· [m morphism?] ...) morphism?]{
See @tech{·}.
}

@defproc[(? [v any/c]) boolean?]{
See @tech{?}.
}

@defproc[(= [m morphism?] ...+) boolean?]{
See @tech{=}.
}

@defproc[(× [v any/c] ...) any]{
An alias for @racket[values].
}
