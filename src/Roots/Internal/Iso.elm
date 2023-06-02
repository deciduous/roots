module Roots.Internal.Iso exposing (Iso(..))


type Iso s t a b
    = Iso (s -> a) (b -> t)
