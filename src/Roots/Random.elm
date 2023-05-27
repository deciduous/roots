module Roots.Random exposing
    ( Random
    , Seed
    , run
    , run_
    , generate
    , map
    , map2
    , pure
    , seed
    , then_
    )

{-| Random.

@docs Random
@docs Seed
@docs run
@docs run_
@docs generate
@docs map
@docs map2
@docs pure
@docs seed
@docs then_

-}

import Random
import Roots.Generator exposing (Generator)
import Roots.List1 exposing (List1(..))


{-| A computation that returns a random value.
-}
type alias Random a =
    Seed -> ( a, Seed )


{-| A random seed.
-}
type alias Seed =
    Random.Seed


{-| Construct a random seed from an integer.
-}
seed : Int -> Seed
seed =
    Random.initialSeed


{-| Run a random computation with an initial seed, returning the value and next seed.
-}
run : Seed -> Random a -> ( a, Seed )
run s ra =
    ra s


{-| Run a random computation with an initial seed, returning the value.
-}
run_ : Seed -> Random a -> a
run_ s ra =
    let
        ( x, _ ) =
            ra s
    in
    x


{-| Pure.
-}
pure : a -> Random a
pure x s =
    ( x, s )


{-| Map a function over a random computation.
-}
map : (a -> b) -> Random a -> Random b
map f ra seed0 =
    let
        ( x, seed1 ) =
            ra seed0
    in
    ( f x, seed1 )


{-| Map a function over two random computations.
-}
map2 : (a -> b -> c) -> Random a -> Random b -> Random c
map2 f ra rb seed0 =
    let
        ( a, seed1 ) =
            ra seed0

        ( b, seed2 ) =
            rb seed1
    in
    ( f a b, seed2 )


{-| Then.
-}
then_ : (a -> Random b) -> Random a -> Random b
then_ f ra seed0 =
    let
        ( a, seed1 ) =
            ra seed0
    in
    f a seed1


{-| Generate a random value from a generator.
-}
generate : Generator a -> Random a
generate =
    Random.step
