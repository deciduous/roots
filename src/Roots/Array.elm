module Roots.Array exposing
    ( singleton, cons
    , last, uncons, unsnoc
    , member, findFirst, all, any
    , delete
    , dropLeft, dropRight
    , Modification(..), modifyFirst
    , concatArrays, concatStrings, intersperse
    , sort, sortBy, sortWith
    , sample, sampleN, shuffle
    , indexAffineTraversal
    )

{-| Array.

@docs singleton, cons
@docs last, uncons, unsnoc
@docs member, findFirst, all, any
@docs delete
@docs dropLeft, dropRight
@docs Modification, modifyFirst
@docs concatArrays, concatStrings, intersperse
@docs sort, sortBy, sortWith
@docs sample, sampleN, shuffle
@docs indexLens

-}

import Array exposing (Array)
import Random
import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal_)
import Roots.List as List
import Roots.Random exposing (Random)


{-| Determine whether every element satisfies a predicate.
-}
all : (a -> Bool) -> Array a -> Bool
all f =
    Array.foldl (\x acc -> acc && f x) True


{-| Determine whether any element satisfies a predicate.
-}
any : (a -> Bool) -> Array a -> Bool
any f =
    Array.foldl (\x acc -> acc || f x) False


{-| Concatenate an array of arrays.
-}
concatArrays : Array (Array a) -> Array a
concatArrays =
    Array.foldl (\xs acc -> Array.append acc xs) Array.empty


{-| Concatenate an array of strings.
-}
concatStrings : Array String -> String
concatStrings =
    Array.foldl (\xs acc -> String.append acc xs) ""


{-| Cons an element onto the left.
-}
cons : a -> Array a -> Array a
cons x =
    Array.append (singleton x)


{-| Delete an element at an index.
-}
delete : Int -> Array a -> Array a
delete i xs =
    if i < 0 then
        xs

    else
        let
            len =
                Array.length xs
        in
        if i >= len then
            xs

        else
            Array.append (Array.slice 0 i xs) (Array.slice (i + 1) len xs)


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


type Modification a
    = Drop
    | Keep a


{-| Modify the first element of a list that matches a predicate, and return a value if the modification was made.
-}
modifyFirst : (a -> Maybe ( Modification a, b )) -> Array a -> ( Array a, Maybe b )
modifyFirst f xs =
    let
        go i =
            case Array.get i xs of
                Nothing ->
                    ( xs, Nothing )

                Just x ->
                    case f x of
                        Nothing ->
                            go (i + 1)

                        Just ( Drop, r ) ->
                            ( delete i xs, Just r )

                        Just ( Keep y, r ) ->
                            ( Array.set i y xs, Just r )
    in
    go 0


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


indexAffineTraversal : Int -> AffineTraversal_ (Array a) a
indexAffineTraversal i =
    AffineTraversal.affineTraversal
        (\xs ->
            case Array.get i xs of
                Nothing ->
                    Err xs

                Just x ->
                    Ok x
        )
        (\xs x -> Array.set i x xs)


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


{-| Shuffle.
-}
shuffle : Array a -> Random (Array a)
shuffle =
    Array.toList >> List.shuffle >> Roots.Random.map Array.fromList


{-| Sort an array.

Implementation note: this round-trips through the list type.

-}
sort : Array comparable -> Array comparable
sort =
    Array.toList >> List.sort >> Array.fromList


{-| Sort an array by a derived value.

Implementation note: this round-trips through the list type.

-}
sortBy : (a -> comparable) -> Array a -> Array a
sortBy f =
    Array.toList >> List.sortBy f >> Array.fromList


{-| Sort an array with an explicit ordering.

Implementation note: this round-trips through the list type.

-}
sortWith : (a -> a -> Order) -> Array a -> Array a
sortWith f =
    Array.toList >> List.sortWith f >> Array.fromList


uncons : Array a -> Maybe ( a, Array a )
uncons xs =
    Maybe.map (\x -> ( x, dropLeft 1 xs )) (Array.get 0 xs)


unsnoc : Array a -> Maybe ( Array a, a )
unsnoc xs =
    Maybe.map (\x -> ( dropRight 1 xs, x )) (Array.get (Array.length xs - 1) xs)
