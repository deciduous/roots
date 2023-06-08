module Roots.AffineTraversal exposing
    ( AffineTraversal, AffineTraversal_
    , affineTraversal
    , preview
    , set, over
    , then_
    )

{-| AffineTraversal.

@docs AffineTraversal, AffineTraversal_
@docs affineTraversal
@docs preview
@docs set, over
@docs then_

-}

import Roots.Internal.AffineTraversal as AffineTraversal


{-| An affine traversal.
-}
type alias AffineTraversal s t a b =
    AffineTraversal.AffineTraversal s t a b


{-| An affine traversal.
-}
type alias AffineTraversal_ s a =
    AffineTraversal s s a a


{-| Construct an affine traversal.
-}
affineTraversal : (s -> Result t a) -> (s -> b -> t) -> AffineTraversal s t a b
affineTraversal =
    AffineTraversal.AffineTraversal


{-| Inspect a value.
-}
preview : AffineTraversal s t a b -> s -> Maybe a
preview (AffineTraversal.AffineTraversal sta _) s =
    Result.toMaybe (sta s)


over : AffineTraversal s t a b -> (a -> b) -> s -> t
over (AffineTraversal.AffineTraversal sta sbt) ab s =
    case sta s of
        Err t ->
            t

        Ok a ->
            sbt s (ab a)


set : AffineTraversal s t a b -> b -> s -> t
set (AffineTraversal.AffineTraversal _ sbt) b s =
    sbt s b


then_ :
    AffineTraversal x y a b
    -> AffineTraversal s t x y
    -> AffineTraversal s t a b
then_ (AffineTraversal.AffineTraversal xya xby) (AffineTraversal.AffineTraversal stx syt) =
    AffineTraversal.AffineTraversal
        (\s ->
            case stx s of
                Err t ->
                    Err t

                Ok x ->
                    case xya x of
                        Err y ->
                            Err (syt s y)

                        Ok a ->
                            Ok a
        )
        (\s b ->
            case stx s of
                Err t ->
                    t

                Ok x ->
                    syt s (xby x b)
        )
