#lang scribble/manual

@(require (for-label ctp
                     (except-in racket/base =)
                     racket/contract))

@title{Appendix}
@defmodule[ctp]

@defstruct*[composition ([procedure* (listof procedure?)] [body procedure?])]{
A structure type for procedures created by @racket[∘].
}

@defproc[(dom [m morphism?]) morphism?]{
Is the @tech{dom} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(cod [m morphism?]) morphism?]{
Is the @tech{cod} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(∘ [m morphism?] ...) morphism?]{
Is the @tech{∘} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(? [v any/c]) boolean?]{
Is the @tech{?} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(= [m morphism?] ...) boolean?]{
Is the @tech{=} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(× [v any/c] ...) any]{
An alias for @racket[values].
}

@defproc[(· [m morphism?] ...) morphism?]{
Is the @tech{·} in @tech{𝐏𝐫𝐨𝐜}.
}

@defproc[(¬ [dom procedure?]
            [cod procedure?]
            [∘ procedure?]
            [? procedure?]
            [= procedure?])
         (values procedure? procedure? procedure? procedure? procedure?)]{
See @tech{¬}.
}

@defproc[(dom× [dom procedure?] ...) procedure?]{
See @tech{dom×}.
}

@defproc[(cod× [cod procedure?] ...) procedure?]{
See @tech{cod×}.
}

@defproc[(∘× [∘ procedure?] ...) procedure?]{
See @tech{∘×}.
}

@defproc[(?× [? procedure?] ...) procedure?]{
See @tech{?×}.
}

@defproc[(=× [= procedure?] ...) procedure?]{
See @tech{=×}.
}

@defproc[(Arr [dom procedure?]
              [cod procedure?]
              [∘ procedure?]
              [? procedure?]
              [= procedure?])
         (values procedure? procedure? procedure? procedure? procedure?)]{
See @tech{Arr}.
}

@defproc[(Sli [dom procedure?]
              [cod procedure?]
              [∘ procedure?]
              [? procedure?]
              [= procedure?])
         (values procedure? procedure? procedure? procedure? procedure?)]{
See @tech{Sli}.
}

@defproc[(¬Sli [dom procedure?]
               [cod procedure?]
               [∘ procedure?]
               [? procedure?]
               [= procedure?])
         (values procedure? procedure? procedure? procedure? procedure?)]{
See @tech{¬Sli}.
}
