module Roots.Generator exposing
    ( Generator
    , oneOf
    , map
    , map2
    , map3
    , map4
    , map5
    , then_
    )

{-|

@docs Generator
@docs oneOf
@docs map
@docs map2
@docs map3
@docs map4
@docs map5
@docs then_

-}

import Random
import Roots.List1 exposing (List1(..))


{-| A generator of random values.
-}
type alias Generator a =
    Random.Generator a


{-| Then.
-}
then_ : (a -> Generator b) -> Generator a -> Generator b
then_ =
    Random.andThen


{-| Map a function over a generator.
-}
map : (a -> b) -> Generator a -> Generator b
map =
    Random.map


{-| Map a function over two generators.
-}
map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 =
    Random.map2


{-| Map a function over three generators.
-}
map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 =
    Random.map3


{-| Map a function over four generators.
-}
map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 =
    Random.map4


{-| Map a function over five generators.
-}
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


{-| Construct a generator that generates one of a collection of weighted values.
-}
oneOf : List1 ( Float, a ) -> Generator a
oneOf xs0 =
    case xs0 of
        List1 x xs ->
            Random.weighted x xs
