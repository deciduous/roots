module Roots.Lens exposing
    ( Lens, Lens_
    , lens
    , view
    , set, over
    )

{-|

@docs Lens, Lens_
@docs lens
@docs view
@docs set, over

-}

import Roots.Internal.Lens as Lens


{-| A lens.
-}
type alias Lens s t a b =
    Lens.Lens s t a b


type alias Lens_ s a =
    Lens s s a a


{-| Construct a lens.
-}
lens : (s -> a) -> (s -> b -> t) -> Lens s t a b
lens =
    Lens.Lens


over : Lens s t a b -> (a -> b) -> s -> t
over (Lens.Lens v s) f x =
    s x (f (v x))


set : Lens s t a b -> b -> s -> t
set (Lens.Lens _ s) b x =
    s x b


view : Lens_ s a -> s -> a
view (Lens.Lens v _) =
    v
