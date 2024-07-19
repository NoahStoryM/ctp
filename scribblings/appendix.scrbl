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

@defproc[(? [v any/c]) boolean?]{
See @tech{?}.
}

@defproc[(= [m morphism?] ...+) boolean?]{
See @tech{=}.
}

@defproc[(× [v any/c] ...) any]{
An alias for @racket[values].
}
