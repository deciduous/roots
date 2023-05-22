module Roots.Float exposing (gen)

{-| Float.

@docs gen

-}

import Random


gen : Float -> Float -> Random.Generator Float
gen =
    Random.float
