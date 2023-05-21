module Roots.Array exposing
    ( cons
    , intersperse
    , last
    , member
    , sample
    , sampleN
    , singleton
    )

import Array exposing (Array)
import Random
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


sampleN : Int -> Array a -> Random (List a)
sampleN n xs =
    if Array.length xs == 0 then
        Roots.Random.pure []

    else
        sampleN_ n xs


sampleN_ : Int -> Array a -> Random (List a)
sampleN_ n xs =
    Roots.Random.map
        (\ixs -> List.filterMap (\ix -> Array.get ix xs) ixs)
        (Random.step (Random.list n (Random.int 0 (Array.length xs - 1))))


singleton : a -> Array a
singleton x =
    Array.initialize 1 (\_ -> x)
