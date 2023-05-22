module Roots.List1 exposing (List1(..))

{-| List1.

@docs List1

-}


type List1 a
    = List1 a (List a)
