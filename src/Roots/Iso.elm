module Roots.Iso exposing
    ( Iso, Iso_
    , iso
    , view, review
    , set, over
    , toPrism
    )

{-|

@docs Iso, Iso_
@docs iso
@docs view, review
@docs set, over
@docs toPrism

-}

import Roots.Internal.Iso as Iso
import Roots.Internal.Prism exposing (Prism(..))


{-| An isomorphism.
-}
type alias Iso s t a b =
    Iso.Iso s t a b


type alias Iso_ s a =
    Iso s s a a


{-| Construct an isomorphism.
-}
iso : (s -> a) -> (b -> t) -> Iso s t a b
iso =
    Iso.Iso


over : Iso s t a b -> (a -> b) -> s -> t
over (Iso.Iso v rv) f =
    v >> f >> rv


review : Iso_ s a -> a -> s
review (Iso.Iso _ rv) =
    rv


set : Iso s t a b -> b -> s -> t
set i b =
    over i (always b)


toPrism : Iso s t a b -> Prism s t a b
toPrism (Iso.Iso v rv) =
    Prism (\t -> Just (v t)) rv


view : Iso_ s a -> s -> a
view (Iso.Iso v _) =
    v
