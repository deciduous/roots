module Roots.Attr exposing
    ( Attr, class, id, none
    , if_, when
    )

{-| Attributes.

@docs Attr, class, id, none
@docs if_, when

-}

import Html.Attributes
import Roots.Internal.Attr as Attr


type alias Attr a =
    Attr.Attr a


class : String -> Attr a
class s =
    Attr.One (Html.Attributes.class s)


id : String -> Attr a
id s =
    Attr.One (Html.Attributes.id s)


none : Attr a
none =
    Attr.Zero


if_ : Bool -> List (Attr a) -> Attr a
if_ b x =
    if b then
        Attr.concat x

    else
        Attr.Zero


when : Maybe a -> (a -> List (Attr b)) -> Attr b
when mx f =
    case mx of
        Just x ->
            Attr.concat (f x)

        Nothing ->
            Attr.Zero
