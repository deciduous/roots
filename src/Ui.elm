module Ui exposing
    ( Attr
    , Color
    , El
    , Font
    , attrIf
    , attrWhen
    , background
    , bold
    , border
    , border4
    , bottom
    , centerX
    , centerY
    , col
    , cursorText
    , el
    , elIf
    , elWhen
    , fontCenter
    , fontColor
    , fontFamily
    , height
    , inFront
    , italic
    , left
    , link
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
    , roundedCorners
    , row
    , size
    , spacing
    , strikethrough
    , text
    , toHtml
    , top
    , underline
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
import Html.Events
import Json.Decode


type alias Attr a =
    List (Element.Attribute a)


type alias Color =
    Element.Color


type alias El a =
    Element.Element a


type alias Font =
    Font.Font


toHtml : El a -> Html a
toHtml =
    Element.layout []



------------------------------------------------------------------------------------------------------------------------
-- Conditional elements/attributes


attrIf : Bool -> List (Attr a) -> Attr a
attrIf b x =
    if b then
        List.concat x

    else
        []


attrWhen : Maybe a -> (a -> List (Attr b)) -> Attr b
attrWhen mx f =
    case mx of
        Nothing ->
            []

        Just x ->
            List.concat (f x)


elIf : Bool -> El a -> El a
elIf b x =
    if b then
        x

    else
        Element.none


elWhen : Maybe a -> (a -> El b) -> El b
elWhen mx f =
    case mx of
        Nothing ->
            Element.none

        Just x ->
            f x



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


link : List (Attr a) -> { label : El a, url : String } -> El a
link attrs =
    Element.link (List.concat attrs)



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
-- Positioning


inFront : El a -> Attr a
inFront x =
    [ Element.inFront x ]


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


fontFamily : List Font -> Attr a
fontFamily xs =
    [ Font.family xs ]


size : Int -> Attr a
size px =
    [ Font.size px ]


fontColor : Color -> Attr a
fontColor c =
    [ Font.color c ]


bold : Attr a
bold =
    [ Font.bold ]


italic : Attr a
italic =
    [ Font.italic ]


underline : Attr a
underline =
    [ Font.underline ]


strikethrough : Attr a
strikethrough =
    [ Font.strike ]


fontCenter : Attr a
fontCenter =
    [ Font.center ]



------------------------------------------------------------------------------------------------------------------------
-- Border


border : Color -> Int -> Int -> Int -> Int -> Attr a
border co a b c d =
    [ Border.color co, Border.solid, Border.widthEach { top = a, right = b, bottom = c, left = d } ]


border4 : Color -> Int -> Attr a
border4 c px =
    [ Border.color c, Border.solid, Border.width px ]


roundedCorners : Int -> Attr a
roundedCorners px =
    [ Border.rounded px ]



------------------------------------------------------------------------------------------------------------------------
-- Color


background : Color -> Attr a
background c =
    [ Background.color c ]


rgb : Int -> Int -> Int -> Color
rgb =
    Element.rgb255



------------------------------------------------------------------------------------------------------------------------
-- Misc


cursorText : Attr a
cursorText =
    [ Element.htmlAttribute (Html.Attributes.style "cursor" "text") ]


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


onEnter : a -> Attr a
onEnter x =
    [ Element.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed x

                        else
                            Json.Decode.fail ""
                    )
            )
        )
    ]
