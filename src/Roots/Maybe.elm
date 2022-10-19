module Roots.Maybe exposing (..)


isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        Just _ ->
            False
