module Roots.Array exposing
    ( cons
    , intersperse
    , last
    , member
    , sample
    , sampleN
    , singleton
    )

{-| Array.

@docs cons intersperse last member sample sampleN singleton

-}

import Array exposing (Array)
import Random
import Roots.List as List
import Roots.Random exposing (Random)


cons : a -> Array a -> Array a
cons x =
    Array.append (singleton x)


intersperse : a -> Array a -> Array a
intersperse x xs =
    Array.fromList (List.intersperse x (Array.toList xs))


last : Array a -> Maybe a
last xs =
    Array.get (Array.length xs - 1) xs


member : a -> Array a -> Bool
member x xs =
    let
        loop i =
            case Array.get i xs of
                Nothing ->
                    False

                Just y ->
                    if x == y then
                        True

                    else
                        loop (i + 1)
    in
    loop 0


sample : a -> Array a -> Random a
sample x0 xs =
    if Array.length xs == 0 then
        Roots.Random.pure x0

    else
        Roots.Random.map
            (\ix ->
                case Array.get ix xs of
                    Nothing ->
                        x0

                    -- impossible
                    Just x ->
                        x
            )
            (Random.step (Random.int 0 (Array.length xs - 1)))


{-| Sample without replacement.
-}
sampleN : Int -> Array a -> Random (List a)
sampleN n =
    -- FIXME more efficient implementation.
    Array.toList >> List.shuffle >> Roots.Random.map (List.take n)


singleton : a -> Array a
singleton x =
    Array.initialize 1 (\_ -> x)
