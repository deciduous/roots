module Roots.Prism exposing
    ( Prism, Prism_
    , prism
    , preview
    , review
    , then_
    , toAffineTraversal
    )

{-| Prism.

@docs Prism, Prism_
@docs prism
@docs preview
@docs review
@docs then_
@docs toAffineTraversal

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
import Roots.Internal.Prism as Prism


{-| A prism.
-}
type alias Prism s t a b =
    Prism.Prism s t a b


type alias Prism_ s a =
    Prism s s a a


{-| Construct a prism.
-}
prism : (s -> Result t a) -> (b -> t) -> Prism s t a b
prism =
    Prism.Prism


{-| Inspect a value.
-}
preview : Prism s t a b -> s -> Maybe a
preview (Prism.Prism sta _) =
    sta >> Result.toMaybe


{-| Construct a value.
-}
review : Prism_ s a -> a -> s
review (Prism.Prism _ as_) =
    as_


then_ : Prism x y a b -> Prism s t x y -> Prism s t a b
then_ (Prism.Prism xya by) (Prism.Prism stx yt) =
    Prism.Prism
        (\s ->
            case stx s of
                Err t ->
                    Err t

                Ok x ->
                    case xya x of
                        Err y ->
                            Err (yt y)

                        Ok a ->
                            Ok a
        )
        (\b -> yt (by b))


{-| Cast to an affine traversal.
-}
toAffineTraversal : Prism s t a b -> AffineTraversal s t a b
toAffineTraversal (Prism.Prism sta bt) =
    AffineTraversal.affineTraversal sta (always bt)
