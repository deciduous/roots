module Roots.List1 exposing (List1(..))

{-| List1.

@docs List1

-}


{-| A non-empty list.
-}
type List1 a
    = List1 a (List a)
