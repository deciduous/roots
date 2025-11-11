module Roots.Html exposing
    ( Html, a, div, input, none, span, text
    , if_, when
    , toHtml
    , img
    )

{-| Html.

@docs Html, a, div, input, none, span, text
@docs if_, when
@docs toHtml

-}

import Html
import Html.Lazy
import Roots.Attr exposing (Attr)
import Roots.Internal.Attr as Attr


type Html a
    = Zero
    | One (Html.Html a)
    | Many (List (Html.Html a))


toHtml : List (Html a) -> List (Html.Html a)
toHtml =
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


concat : List (Html a) -> Html a
concat x =
    case toHtml x of
        [] ->
            Zero

        [ y ] ->
            One y

        y ->
            Many y


a : List (Attr a) -> List (Html a) -> Html a
a x y =
    One (Html.a (Attr.compile x) (toHtml y))


img : List (Attr a) -> List (Html a) -> Html a
img x y =
    One (Html.img (Attr.compile x) (toHtml y))


div : List (Attr a) -> List (Html a) -> Html a
div x y =
    One (Html.div (Attr.compile x) (toHtml y))


input : List (Attr a) -> List (Html a) -> Html a
input x y =
    One (Html.input (Attr.compile x) (toHtml y))


none : Html a
none =
    Zero


span : List (Attr a) -> List (Html a) -> Html a
span x y =
    One (Html.span (Attr.compile x) (toHtml y))


text : String -> Html a
text s =
    One (Html.text s)


if_ : Bool -> List (Html a) -> Html a
if_ b x =
    if b then
        concat x

    else
        Zero


when : Maybe a -> (a -> List (Html b)) -> Html b
when mx f =
    case mx of
        Just x ->
            concat (f x)

        Nothing ->
            Zero


lazy : (a -> Html b) -> (a -> Html b)
lazy f x =
    One (Html.Lazy.lazy (\x1 -> Debug.todo "") x)
