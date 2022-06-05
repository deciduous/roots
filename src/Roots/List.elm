module Roots.List exposing
    ( chunk
    , splitAt
    )


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
