module Roots.Html exposing (if_, none, when)

import Html exposing (Html)


none : Html a
none =
    Html.text ""


if_ : Bool -> Html a -> Html a
if_ b x =
    if b then
        x

    else
        Html.text ""


when : Maybe a -> (a -> Html b) -> Html b
when mx f =
    case mx of
        Just x ->
            f x

        Nothing ->
            Html.text ""
