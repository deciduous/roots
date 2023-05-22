module Roots.Maybe exposing
    ( isJust
    , isNothing
    )

{-| Maybe.

@docs isJust isNothing

-}


isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just _ ->
            True


isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        Just _ ->
            False
