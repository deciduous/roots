module Roots.Internal.Lens exposing (Lens(..))


type Lens s t a b
    = Lens (s -> a) (s -> b -> t)
