module Roots.Attr exposing
    ( Attr, autocomplete, checked, class, disabled, for, href, id, inputmode, min, none, placeholder, spellcheck, src, type_, value
    , onclick, onfocus, oninput, onkeydown, onkeyup
    , if_, when
    )

{-| Attributes.

@docs Attr, autocomplete, checked, class, disabled, for, href, id, inputmode, min, none, placeholder, spellcheck, src, type_, value
@docs onclick, onfocus, oninput, onkeydown, onkeyup
@docs if_, when

-}

import Html.Attributes
import Html.Events
import Json.Decode
import Roots.Internal.Attr as Attr


type alias Attr a =
    Attr.Attr a


autocomplete : Bool -> Attr a
autocomplete x =
    Attr.One (Html.Attributes.autocomplete x)


checked : Bool -> Attr a
checked b =
    Attr.One (Html.Attributes.checked b)


class : String -> Attr a
class s =
    Attr.One (Html.Attributes.class s)


disabled : Bool -> Attr a
disabled b =
    Attr.One (Html.Attributes.disabled b)


for : String -> Attr a
for s =
    Attr.One (Html.Attributes.for s)


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


onkeydown : String -> a -> Attr a
onkeydown key x =
    Attr.One
        (Html.Events.on "keydown"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key1 ->
                        if key == key1 then
                            Json.Decode.succeed x

                        else
                            Json.Decode.fail ""
                    )
            )
        )


onkeyup : String -> a -> Attr a
onkeyup key x =
    Attr.One
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key1 ->
                        if key == key1 then
                            Json.Decode.succeed x

                        else
                            Json.Decode.fail ""
                    )
            )
        )


placeholder : String -> Attr a
placeholder s =
    Attr.One (Html.Attributes.placeholder s)


spellcheck : Bool -> Attr a
spellcheck x =
    Attr.One (Html.Attributes.spellcheck x)


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
