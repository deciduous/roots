module Roots.Array exposing
    ( singleton, cons
    , last, uncons
    , dropLeft, dropRight
    , member, findFirst
    , concat, intersperse
    , sample, sampleN
    )

{-| Array.

@docs singleton, cons
@docs last, uncons
@docs dropLeft, dropRight
@docs member, findFirst
@docs concat, intersperse
@docs sample, sampleN

-}

import Array exposing (Array)
import Random
import Roots.List as List
import Roots.Random exposing (Random)


{-| Concatenate an array of arrays.
-}
concat : Array (Array a) -> Array a
concat =
    Array.foldl (\xs acc -> Array.append acc xs) Array.empty


{-| Cons an element onto the left.
-}
cons : a -> Array a -> Array a
cons x =
    Array.append (singleton x)


{-| Drop elements from the left.
-}
dropLeft : Int -> Array a -> Array a
dropLeft n xs =
    if n <= 0 then
        xs

    else
        let
            len =
                Array.length xs
        in
        if n >= len then
            Array.empty

        else
            Array.slice n len xs


{-| Drop elements from the right.
-}
dropRight : Int -> Array a -> Array a
dropRight n xs =
    if n <= 0 then
        xs

    else if n >= Array.length xs then
        Array.empty

    else
        Array.slice 0 (negate n) xs


{-| Find the first element that satisfies a predicate.
-}
findFirst : (a -> Maybe b) -> Array a -> Maybe b
findFirst f xs =
    let
        len =
            Array.length xs

        go i =
            if i >= len then
                Nothing

            else
                case Array.get i xs of
                    Nothing ->
                        go (i + 1)

                    Just x ->
                        case f x of
                            Nothing ->
                                go (i + 1)

                            Just y ->
                                Just y
    in
    go 0


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


{-| Sample an element, or return a default element if the array is empty.
-}
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


{-| Make a one-element array.
-}
singleton : a -> Array a
singleton x =
    Array.initialize 1 (\_ -> x)


uncons : Array a -> Maybe ( a, Array a )
uncons xs =
    Maybe.map (\x -> ( x, dropLeft 1 xs )) (Array.get 0 xs)
