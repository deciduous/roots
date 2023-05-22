module Roots.Generator exposing
    ( Generator
    , map
    , map2
    , map3
    , map4
    , map5
    , oneOf
    , then_
    )

{-| Generator.

@docs oneOf
@docs map map2 map3 map4 map5
@docs then_

-}

import Random
import Roots.List1 exposing (List1(..))


type alias Generator a =
    Random.Generator a


then_ : (a -> Generator b) -> Generator a -> Generator b
then_ =
    Random.andThen


map : (a -> b) -> Generator a -> Generator b
map =
    Random.map


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 =
    Random.map2


map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 =
    Random.map3


map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 =
    Random.map4


map5 :
    (a -> b -> c -> d -> e -> f)
    -> Generator a
    -> Generator b
    -> Generator c
    -> Generator d
    -> Generator e
    -> Generator f
map5 =
    Random.map5


oneOf : List1 ( Float, a ) -> Generator a
oneOf xs0 =
    case xs0 of
        List1 x xs ->
            Random.weighted x xs
