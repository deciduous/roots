module Roots.Lens exposing
    ( Lens, Lens_
    , lens
    , view
    , set, over
    , then_
    , toAffineTraversal
    )

{-|

@docs Lens, Lens_
@docs lens
@docs view
@docs set, over
@docs then_
@docs toAffineTraversal

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
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
over (Lens.Lens sa sbt) ab s =
    sbt s (ab (sa s))


set : Lens s t a b -> b -> s -> t
set (Lens.Lens _ sbt) b s =
    sbt s b


then_ : Lens x y a b -> Lens s t x y -> Lens s t a b
then_ (Lens.Lens xa xby) (Lens.Lens sx syt) =
    Lens.Lens (\x -> xa (sx x)) (\s b -> syt s (xby (sx s) b))


{-| Cast to an affine traversal.
-}
toAffineTraversal : Lens s t a b -> AffineTraversal s t a b
toAffineTraversal (Lens.Lens sa sbt) =
    AffineTraversal.affineTraversal (sa >> Ok) sbt


view : Lens s t a b -> s -> a
view (Lens.Lens sa _) =
    sa
