module Roots.Float exposing (gen)

import Random


gen : Float -> Float -> Random.Generator Float
gen =
    Random.float
