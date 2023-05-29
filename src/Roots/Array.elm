module Roots.Array exposing (cons, dropLeft, intersperse, last, member, sample, sampleN, singleton, uncons)

{-| Array.

@docs cons, dropLeft, intersperse, last, member, sample, sampleN, singleton, uncons

-}

import Array exposing (Array)
import Random
import Roots.List as List
import Roots.Random exposing (Random)


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
