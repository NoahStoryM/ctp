#lang scribble/manual

@(require (for-label ctp (except-in racket/base =) racket/contract))

@title{Appendix}
@defmodule[ctp]

@defproc[(dom [m procedure?]) procedure?]{
See @tech{dom}.
}

@defproc[(cod [m procedure?]) procedure?]{
See @tech{cod}.
}

@defproc[(∘ [m procedure?] ...) procedure?]{
See @tech{∘}.
}

@defproc[(? [v any/c]) boolean?]{
See @tech{?}.
}

@defproc[(= [m0 procedure?] [m1 procedure?] ...) boolean?]{
See @tech{=}.
}

@defproc[(× [v any/c] ...) any]{
An alias for @racket[values].
}
