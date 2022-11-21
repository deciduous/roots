module Roots.Array exposing
    ( cons
    , intersperse
    , member
    , singleton
    )

import Array exposing (Array)


cons : a -> Array a -> Array a
cons x =
    Array.append (singleton x)


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


singleton : a -> Array a
singleton x =
    Array.initialize 1 (\_ -> x)
