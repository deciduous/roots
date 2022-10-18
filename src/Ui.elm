module Ui exposing
    ( Attr
    , El
    , bold
    , border
    , bottom
    , centerX
    , centerY
    , col
    , color
    , el
    , fontColor
    , height
    , italic
    , left
    , maxHeight
    , maxWidth
    , none
    , onClick
    , padding
    , padding4
    , paragraph
    , pointer
    , rgb
    , right
    , row
    , size
    , spacing
    , text
    , toHtml
    , top
    , unselectable
    , width
    )

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes


type alias Attr a =
    List (Element.Attribute a)


type alias El a =
    Element.Element a


type alias Color =
    Element.Color


toHtml : El a -> Html a
toHtml =
    Element.layout []



------------------------------------------------------------------------------------------------------------------------
-- Container elements


el : List (Attr a) -> El a -> El a
el attrs =
    Element.el (List.concat attrs)


col : List (Attr a) -> List (El a) -> El a
col attrs =
    Element.column (List.concat attrs)


row : List (Attr a) -> List (El a) -> El a
row attrs =
    Element.row (List.concat attrs)



------------------------------------------------------------------------------------------------------------------------
-- Basic elements


none : El a
none =
    Element.none


text : String -> El a
text =
    Element.text


paragraph : List (Attr a) -> List (El a) -> El a
paragraph attrs =
    Element.paragraph (List.concat attrs)



------------------------------------------------------------------------------------------------------------------------
-- Positioning


bottom : Attr a
bottom =
    [ Element.alignBottom ]


left : Attr a
left =
    [ Element.alignLeft ]


right : Attr a
right =
    [ Element.alignRight ]


top : Attr a
top =
    [ Element.alignTop ]


centerX : Attr a
centerX =
    [ Element.centerX ]


centerY : Attr a
centerY =
    [ Element.centerY ]



------------------------------------------------------------------------------------------------------------------------
-- Sizing / padding / spacing


height : Int -> Attr a
height px =
    [ Element.height (Element.px px) ]


maxHeight : Attr a
maxHeight =
    [ Element.height Element.fill ]


width : Int -> Attr a
width px =
    [ Element.width (Element.px px) ]


maxWidth : Attr a
maxWidth =
    [ Element.width Element.fill ]


padding : Int -> Int -> Int -> Int -> Attr a
padding a b c d =
    [ Element.paddingEach { top = a, right = b, bottom = c, left = d } ]


padding4 : Int -> Attr a
padding4 px =
    [ Element.padding px ]


spacing : Int -> Attr a
spacing px =
    [ Element.spacing px ]



------------------------------------------------------------------------------------------------------------------------
-- Font


fontColor : Color -> Attr a
fontColor c =
    [ Font.color c ]


bold : Attr a
bold =
    [ Font.bold ]


italic : Attr a
italic =
    [ Font.italic ]


size : Int -> Attr a
size px =
    [ Font.size px ]



------------------------------------------------------------------------------------------------------------------------
-- Border


border : Color -> Int -> Attr a
border c px =
    [ Border.color c, Border.solid, Border.width px ]



------------------------------------------------------------------------------------------------------------------------
-- Color


color : Color -> Attr a
color c =
    [ Background.color c ]


rgb : Int -> Int -> Int -> Color
rgb =
    Element.rgb255



------------------------------------------------------------------------------------------------------------------------
-- Misc


pointer : Attr a
pointer =
    [ Element.pointer ]


unselectable : Attr a
unselectable =
    [ Element.htmlAttribute (Html.Attributes.style "user-select" "none") ]



------------------------------------------------------------------------------------------------------------------------
-- Events


onClick : a -> Attr a
onClick x =
    [ Events.onClick x ]
