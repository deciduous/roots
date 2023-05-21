module Roots.Int exposing (gen)

import Random


{-| Generate an int between -2147483648 and 2147483647.
-}
gen : Int -> Int -> Random.Generator Int
gen =
    Random.int
