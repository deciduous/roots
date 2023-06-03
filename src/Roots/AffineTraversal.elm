module Roots.AffineTraversal exposing
    ( AffineTraversal, AffineTraversal_
    , affineTraversal
    , preview
    , set, over
    )

{-| AffineTraversal.

@docs AffineTraversal, AffineTraversal_
@docs affineTraversal
@docs preview
@docs set, over

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
preview (AffineTraversal.AffineTraversal pv _) x =
    case pv x of
        Err _ ->
            Nothing

        Ok a ->
            Just a


over : AffineTraversal s t a b -> (a -> b) -> s -> t
over (AffineTraversal.AffineTraversal pv s) f x =
    case pv x of
        Err t ->
            t

        Ok a ->
            s x (f a)


set : AffineTraversal s t a b -> b -> s -> t
set (AffineTraversal.AffineTraversal _ s) b x =
    s x b
