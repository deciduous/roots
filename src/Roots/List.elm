module Roots.List exposing
    ( Modification(..)
    , asSingleton
    , chunk
    , deleteFirst
    , dropWhile
    , findFirst
    , indexed
    , isSingleton
    , mapAndReverse
    , mapMaybe
    , modify
    , modifyFirst
    , modifyFirst_
    , overLast
    , splitAt
    , uncons
    , unsnoc
    )

import List


asSingleton : List a -> Maybe a
asSingleton xs =
    case xs of
        [ x ] ->
            Just x

        _ ->
            Nothing


chunk : Int -> List a -> List (List a)
chunk n xs =
    if n <= 0 then
        []

    else
        chunk_ n xs


chunk_ : Int -> List a -> List (List a)
chunk_ n xs =
    case splitAt_ n xs of
        ( [], [] ) ->
            []

        ( ys, zs ) ->
            ys :: chunk_ n zs


{-| Delete the first occurrence of an element.
-}
deleteFirst : (a -> Bool) -> List a -> List a
deleteFirst p =
    let
        loop xs =
            case xs of
                [] ->
                    []

                y :: ys ->
                    if p y then
                        ys

                    else
                        y :: loop ys
    in
    loop


dropWhile : (a -> Bool) -> List a -> List a
dropWhile f xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if f y then
                dropWhile f ys

            else
                xs


findFirst : (a -> Maybe b) -> List a -> Maybe b
findFirst f xs0 =
    case xs0 of
        [] ->
            Nothing

        x :: xs ->
            case f x of
                Nothing ->
                    findFirst f xs

                Just y ->
                    Just y


indexed : List a -> List ( Int, a )
indexed =
    List.indexedMap (\n x -> ( n, x ))


isSingleton : List a -> Bool
isSingleton xs =
    case xs of
        [ _ ] ->
            True

        _ ->
            False


mapAndReverse : (a -> b) -> List a -> List b
mapAndReverse f =
    List.foldl (\x -> (::) (f x)) []


mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f xs =
    case xs of
        [] ->
            []

        y :: ys ->
            let
                zs =
                    mapMaybe f ys
            in
            case f y of
                Nothing ->
                    zs

                Just z ->
                    z :: zs


modify : Int -> (a -> ( Maybe a, b )) -> List a -> ( List a, Maybe b )
modify n f xs0 =
    case xs0 of
        [] ->
            ( [], Nothing )

        x :: xs ->
            case n of
                0 ->
                    case f x of
                        ( Nothing, b ) ->
                            ( xs, Just b )

                        ( Just y, b ) ->
                            ( y :: xs, Just b )

                _ ->
                    let
                        ( ys, b ) =
                            modify (n - 1) f xs
                    in
                    ( x :: ys, b )


type Modification a
    = Drop
    | Keep a


{-| Modify the first element of a list that matches a predicate.
-}
modifyFirst : (a -> Maybe ( Modification a, b )) -> List a -> ( List a, Maybe b )
modifyFirst f xs0 =
    case xs0 of
        [] ->
            ( [], Nothing )

        x :: xs ->
            case f x of
                Nothing ->
                    let
                        ( ys, b ) =
                            modifyFirst f xs
                    in
                    ( x :: ys, b )

                Just ( Drop, b ) ->
                    ( xs, Just b )

                Just ( Keep y, b ) ->
                    ( y :: xs, Just b )


modifyFirst_ : (a -> Maybe (Modification a)) -> List a -> List a
modifyFirst_ f xs0 =
    case xs0 of
        [] ->
            []

        x :: xs ->
            case f x of
                Nothing ->
                    x :: modifyFirst_ f xs

                Just Drop ->
                    xs

                Just (Keep y) ->
                    y :: xs


overLast : (a -> a) -> List a -> List a
overLast f xs0 =
    case xs0 of
        [] ->
            []

        [ x ] ->
            [ f x ]

        x :: xs ->
            x :: overLast f xs


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    if n <= 0 then
        ( [], xs )

    else
        splitAt_ n xs


splitAt_ : Int -> List a -> ( List a, List a )
splitAt_ n xs0 =
    case xs0 of
        [] ->
            ( [], [] )

        x :: xs ->
            if n == 1 then
                ( [ x ], xs )

            else
                let
                    ( ys, zs ) =
                        splitAt_ (n - 1) xs
                in
                ( x :: ys, zs )


uncons : List a -> Maybe ( a, List a )
uncons xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            Just ( y, ys )


unsnoc : List a -> Maybe ( List a, a )
unsnoc xs =
    case xs of
        [] ->
            Nothing

        y :: ys ->
            case unsnoc ys of
                Nothing ->
                    Just ( [], y )

                Just ( zs, z ) ->
                    Just ( y :: zs, z )
