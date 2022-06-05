module Roots.List exposing
    ( chunk
    , indexed
    , modify
    , modifyFirst
    , splitAt
    )

import List


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


indexed : List a -> List ( Int, a )
indexed =
    List.indexedMap (\n x -> ( n, x ))


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


modifyFirst : (a -> Bool) -> (a -> ( Maybe a, b )) -> List a -> ( List a, Maybe b )
modifyFirst p f xs0 =
    case xs0 of
        [] ->
            ( [], Nothing )

        x :: xs ->
            if p x then
                case f x of
                    ( Nothing, b ) ->
                        ( xs, Just b )

                    ( Just y, b ) ->
                        ( y :: xs, Just b )

            else
                let
                    ( ys, b ) =
                        modifyFirst p f xs
                in
                ( x :: ys, b )


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
