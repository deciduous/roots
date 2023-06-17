module Roots.Maybe exposing
    ( isNothing, isJust
    , when
    , nothingPrism, justPrism
    )

{-| Maybe.

@docs isNothing, isJust
@docs when
@docs nothingPrism, justPrism

-}

import Roots.Prism as Prism exposing (Prism, Prism_)


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


justPrism : Prism (Maybe a) (Maybe b) a b
justPrism =
    Prism.prism
        (\ma ->
            case ma of
                Nothing ->
                    Err Nothing

                Just a ->
                    Ok a
        )
        Just


nothingPrism : Prism_ (Maybe a) ()
nothingPrism =
    Prism.prism
        (\ma ->
            case ma of
                Nothing ->
                    Ok ()

                Just _ ->
                    Err ma
        )
        (\() -> Nothing)


when : Bool -> a -> Maybe a
when b x =
    if b then
        Just x

    else
        Nothing
