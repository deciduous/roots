module Roots.Array exposing
    ( cons
    , intersperse
    , last
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


singleton : a -> Array a
singleton x =
    Array.initialize 1 (\_ -> x)
