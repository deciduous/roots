module Roots.Prism exposing
    ( Prism, Prism_
    , prism
    , preview
    , review
    )

{-| Prism.

@docs Prism, Prism_
@docs prism
@docs preview
@docs review

-}

import Roots.Internal.Prism as Prism


{-| A prism.
-}
type alias Prism s t a b =
    Prism.Prism s t a b


type alias Prism_ s a =
    Prism s s a a


{-| Construct a prism.
-}
prism : (s -> Maybe a) -> (b -> s) -> Prism s s a b
prism =
    Prism.Prism


{-| Inspect a value.
-}
preview : Prism s t a b -> s -> Maybe a
preview (Prism.Prism pv _) =
    pv


{-| Construct a value.
-}
review : Prism_ s a -> a -> s
review (Prism.Prism _ rv) =
    rv
