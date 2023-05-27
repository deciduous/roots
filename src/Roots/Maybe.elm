module Roots.Maybe exposing
    ( isJust
    , isNothing
    )

{-| Maybe.

@docs isJust
@docs isNothing

-}


{-| Is this value a Just?
-}
isJust : Maybe a -> Bool
isJust x =
    case x of
        Nothing ->
            False

        Just _ ->
            True

{-| Is this value a Nothing?
-}
isNothing : Maybe a -> Bool
isNothing x =
    case x of
        Nothing ->
            True

        Just _ ->
            False
