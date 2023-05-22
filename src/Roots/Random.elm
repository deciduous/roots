module Roots.Random exposing
    ( Random
    , Seed
    , generate
    , map
    , map2
    , pure
    , run
    , run_
    , seed
    , then_
    )

{-| Random.

@docs Random Seed
@docs generate map map2 pure run run_ seed then_

-}

import Random
import Roots.Generator exposing (Generator)
import Roots.List1 exposing (List1(..))


type alias Random a =
    Seed -> ( a, Seed )


type alias Seed =
    Random.Seed


seed : Int -> Seed
seed =
    Random.initialSeed


run : Seed -> Random a -> ( a, Seed )
run s ra =
    ra s


run_ : Seed -> Random a -> a
run_ s ra =
    let
        ( x, _ ) =
            ra s
    in
    x


pure : a -> Random a
pure x s =
    ( x, s )


map : (a -> b) -> Random a -> Random b
map f ra seed0 =
    let
        ( x, seed1 ) =
            ra seed0
    in
    ( f x, seed1 )


map2 : (a -> b -> c) -> Random a -> Random b -> Random c
map2 f ra rb seed0 =
    let
        ( a, seed1 ) =
            ra seed0

        ( b, seed2 ) =
            rb seed1
    in
    ( f a b, seed2 )


then_ : (a -> Random b) -> Random a -> Random b
then_ f ra seed0 =
    let
        ( a, seed1 ) =
            ra seed0
    in
    f a seed1


generate : Generator a -> Random a
generate =
    Random.step
