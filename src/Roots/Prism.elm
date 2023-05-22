module Roots.Prism exposing
    ( Prism
    , preview
    , review
    )

{-| Prism.

@docs Prism
@docs preview review

-}


type alias Prism s a =
    ( s -> Maybe a, a -> s )


preview : Prism s a -> s -> Maybe a
preview ( f, _ ) =
    f


review : Prism s a -> a -> s
review ( _, f ) =
    f
