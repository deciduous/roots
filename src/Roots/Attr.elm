module Roots.Attr exposing
    ( Attr, class, href, id, inputmode, min, none, onclick, onfocus, oninput, placeholder, src, type_, value
    , if_, when
    )

{-| Attributes.

@docs Attr, class, href, id, inputmode, min, none, onclick, onfocus, oninput, placeholder, src, type_, value
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


inputmode : String -> Attr a
inputmode s =
    Attr.One (Html.Attributes.attribute "inputmode" s)


id : String -> Attr a
id s =
    Attr.One (Html.Attributes.id s)


min : String -> Attr a
min s =
    Attr.One (Html.Attributes.min s)


none : Attr a
none =
    Attr.Zero


onclick : a -> Attr a
onclick x =
    Attr.One (Html.Events.onClick x)


onfocus : a -> Attr a
onfocus x =
    Attr.One (Html.Events.onFocus x)


oninput : (String -> a) -> Attr a
oninput x =
    Attr.One (Html.Events.onInput x)


placeholder : String -> Attr a
placeholder s =
    Attr.One (Html.Attributes.placeholder s)


src : String -> Attr a
src s =
    Attr.One (Html.Attributes.src s)


type_ : String -> Attr a
type_ s =
    Attr.One (Html.Attributes.type_ s)


value : String -> Attr a
value s =
    Attr.One (Html.Attributes.value s)


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
