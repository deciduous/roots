module Roots.Html exposing
    ( Html, a, div, none, text
    , if_, when
    , toHtml
    , img
    )

{-| Html.

@docs Html, a, div, none, text
@docs if_, when
@docs toHtml

-}

import Html
import Roots.Attr exposing (Attr)


type Html a
    = Zero
    | One (Html.Html a)
    | Many (List (Html.Html a))


compile : List (Html a) -> List (Html.Html a)
compile =
    List.foldr
        (\html acc ->
            case html of
                Zero ->
                    acc

                One html1 ->
                    html1 :: acc

                Many html1 ->
                    html1 ++ acc
        )
        []


a : List (Attr a) -> List (Html a) -> Html a
a x y =
    One (Html.a (List.concat x) (compile y))


img : List (Attr a) -> List (Html a) -> Html a
img x y =
    One (Html.img (List.concat x) (compile y))


div : List (Attr a) -> List (Html a) -> Html a
div x y =
    One (Html.div (List.concat x) (compile y))


none : Html a
none =
    Zero


text : String -> Html a
text s =
    One (Html.text s)


if_ : Bool -> List (Html a) -> Html a
if_ b x =
    if b then
        Many (compile x)

    else
        none


when : Maybe a -> (a -> List (Html b)) -> Html b
when mx f =
    case mx of
        Just x ->
            Many (compile (f x))

        Nothing ->
            none


toHtml : Html a -> List (Html.Html a)
toHtml html =
    case html of
        Zero ->
            []

        One html1 ->
            [ html1 ]

        Many html1 ->
            html1
