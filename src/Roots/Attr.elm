module Roots.Attr exposing
    ( Attr, none
    , if_, when
    )

{-| Attributes.

@docs Attr, none
@docs if_, when

-}

import Html


type alias Attr a =
    List (Html.Attribute a)


none : Attr a
none =
    []


if_ : Bool -> List (Attr a) -> Attr a
if_ b x =
    if b then
        List.concat x

    else
        none


when : Maybe a -> (a -> List (Attr b)) -> Attr b
when mx f =
    case mx of
        Just x ->
            List.concat (f x)

        Nothing ->
            none
