module Roots.Internal.Prism exposing (Prism(..))


type Prism s t a b
    = Prism (s -> Maybe a) (b -> t)
