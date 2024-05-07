#lang scribble/manual

@(require (for-label ctp
                     racket/base
                     racket/contract))

@title{Appendix}
@defmodule[ctp]

@defproc[(dom [m morphism?]) morphism?]{
Is the @tech{dom} in the @secref["Category_of_Procedures"].
}

@defproc[(cod [m morphism?]) morphism?]{
Is the @tech{cod} in the @secref["Category_of_Procedures"].
}

@defproc[(∘ [m morphism?] ...) morphism?]{
Is the @tech{∘} in the @secref["Category_of_Procedures"].
}

@defproc[(? [m any/c]) boolean?]{
Is the @tech{?} in the @secref["Category_of_Procedures"].
}

@defproc[(· [m morphism?] ...) morphism?]{
Is the @tech{·} in the @secref["Category_of_Procedures"].
}

@defproc[(¬ [dom procedure?]
            [cod procedure?]
            [∘ procedure?]
            [? procedure?]
            [= procedure?])
         (values procedure? procedure? procedure? procedure? procedure?)]{
See @tech{¬}.
}

@defproc[(× [m any/c] ...) list?]{
An alias for @racket[list].
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
