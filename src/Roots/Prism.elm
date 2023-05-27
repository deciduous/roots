module Roots.Prism exposing
    ( Prism
    , preview
    , review
    )

{-| Prism.

@docs Prism
@docs preview
@docs review

-}


{-| A prism.
-}
type alias Prism s a =
    ( s -> Maybe a, a -> s )


{-| Inspect a value.
-}
preview : Prism s a -> s -> Maybe a
preview ( f, _ ) =
    f


{-| Construct a value.
-}
review : Prism s a -> a -> s
review ( _, f ) =
    f
