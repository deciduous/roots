module Roots.Maybe exposing
    ( isNothing, isJust
    , when
    )

{-| Maybe.

@docs isNothing, isJust
@docs when

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


when : Bool -> a -> Maybe a
when b x =
    if b then
        Just x

    else
        Nothing
