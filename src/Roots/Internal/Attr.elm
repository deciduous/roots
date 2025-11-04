module Roots.Internal.Attr exposing
    ( Attr(..)
    , compile
    , concat
    )

import Html


type Attr a
    = Zero
    | One (Html.Attribute a)
    | Many (List (Html.Attribute a))


compile : List (Attr a) -> List (Html.Attribute a)
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


concat : List (Attr a) -> Attr a
concat x =
    case compile x of
        [] ->
            Zero

        [ y ] ->
            One y

        y ->
            Many y
