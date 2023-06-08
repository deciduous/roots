module Roots.Iso exposing
    ( Iso, Iso_
    , iso
    , view, review
    , set, over
    , then_
    , toLens, toPrism
    )

{-|

@docs Iso, Iso_
@docs iso
@docs view, review
@docs set, over
@docs then_
@docs toLens, toPrism

-}

import Roots.Internal.Iso as Iso
import Roots.Internal.Lens exposing (Lens(..))
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
over (Iso.Iso sa bt) ab s =
    bt (ab (sa s))


review : Iso_ s a -> a -> s
review (Iso.Iso _ as_) =
    as_


set : Iso s t a b -> b -> s -> t
set (Iso.Iso _ bt) b _ =
    bt b


{-| Cast to a lens.
-}
toLens : Iso s t a b -> Lens s t a b
toLens (Iso.Iso sa bt) =
    Lens sa (always bt)


then_ : Iso x y a b -> Iso s t x y -> Iso s t a b
then_ (Iso.Iso xa by) (Iso.Iso sx yt) =
    Iso.Iso (\s -> xa (sx s)) (\b -> yt (by b))


{-| Cast to a prism.
-}
toPrism : Iso s t a b -> Prism s t a b
toPrism (Iso.Iso sa bt) =
    Prism (sa >> Ok) bt


view : Iso_ s a -> s -> a
view (Iso.Iso sa _) =
    sa
