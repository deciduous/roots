module Roots.Attr exposing
    ( Attr, class, href, id, none, onclick, oninput, src
    , if_, when
    )

{-| Attributes.

@docs Attr, class, href, id, none, onclick, oninput, src
@docs if_, when

-}

import Html.Attributes
import Html.Events
import Roots.Internal.Attr as Attr


type alias Attr a =
    Attr.Attr a


class : String -> Attr a
class s =
    Attr.One (Html.Attributes.class s)


href : String -> Attr a
href s =
    Attr.One (Html.Attributes.href s)


id : String -> Attr a
id s =
    Attr.One (Html.Attributes.id s)


none : Attr a
none =
    Attr.Zero


onclick : a -> Attr a
onclick x =
    Attr.One (Html.Events.onClick x)


oninput : (String -> a) -> Attr a
oninput x =
    Attr.One (Html.Events.onInput x)


src : String -> Attr a
src s =
    Attr.One (Html.Attributes.src s)


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
