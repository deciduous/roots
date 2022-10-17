module Roots.Array exposing
    ( intersperse
    , member
    )

import Array exposing (Array)


intersperse : a -> Array a -> Array a
intersperse x xs =
    Array.fromList (List.intersperse x (Array.toList xs))


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
