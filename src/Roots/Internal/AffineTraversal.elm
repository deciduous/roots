module Roots.Internal.AffineTraversal exposing (AffineTraversal(..))


type AffineTraversal s t a b
    = AffineTraversal (s -> Result t a) (s -> b -> t)
