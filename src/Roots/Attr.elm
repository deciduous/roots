module Roots.Attr exposing (..)

import Html
import Html.Attributes
import Html.Events
import Json.Decode


inputmode : String -> Html.Attribute a
inputmode =
    Html.Attributes.attribute "inputmode"


none : Html.Attribute a
none =
    Html.Attributes.class ""


onclick : a -> Html.Attribute a
onclick =
    Html.Events.onClick


onfocus : a -> Html.Attribute a
onfocus =
    Html.Events.onFocus


oninput : (String -> a) -> Html.Attribute a
oninput =
    Html.Events.onInput


onkeydown : String -> a -> Html.Attribute a
onkeydown key x =
    Html.Events.on "keydown"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key1 ->
                    if key == key1 then
                        Json.Decode.succeed x

                    else
                        Json.Decode.fail ""
                )
        )


onkeyup : String -> a -> Html.Attribute a
onkeyup key x =
    Html.Events.on "keyup"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key1 ->
                    if key == key1 then
                        Json.Decode.succeed x

                    else
                        Json.Decode.fail ""
                )
        )


if_ : Bool -> Html.Attribute a -> Html.Attribute a
if_ b x =
    if b then
        x

    else
        Html.Attributes.class ""


when : Maybe a -> (a -> Html.Attribute b) -> Html.Attribute b
when mx f =
    case mx of
        Just x ->
            f x

        Nothing ->
            Html.Attributes.class ""
