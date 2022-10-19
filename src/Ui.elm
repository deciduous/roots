module Ui exposing
    ( Attr
    , Color
    , El
    , Font
    , Option
    , Svg
    , above
    , attr
    , attrIf
    , attrWhen
    , autocomplete
    , background
    , behind
    , below
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
    , focusStyle
    , fontCenter
    , fontColor
    , fontFamily
    , height
    , id
    , inFrontOf
    , italic
    , lazy
    , lazy2
    , lazy3
    , lazy4
    , lazy5
    , left
    , link
    , maxHeight
    , maxWidth
    , none
    , onClick
    , onDoubleClick
    , onEnter
    , onFocus
    , onLoseFocus
    , onMouseDown
    , onMouseEnter
    , onMouseLeave
    , onMouseMove
    , onMouseUp
    , padding
    , padding4
    , paragraph
    , pointer
    , rgb
    , right
    , roundedCorners
    , row
    , sansSerif
    , size
    , spacing
    , strikethrough
    , svg
    , text
    , toHtml
    , top
    , typeface
    , underline
    , unselectable
    , width
    )

import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Svg


type alias Attr a =
    List (Element.Attribute a)


type alias Color =
    Element.Color


type alias El a =
    Element.Element a


type alias Font =
    Font.Font


type alias Option =
    Element.Option


type alias Svg a =
    Svg.Svg a


toHtml : List Option -> El a -> Html a
toHtml options =
    Element.layoutWith { options = options } []


focusStyle : Element.FocusStyle -> Option
focusStyle =
    Element.focusStyle



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


svg : List (Html.Attribute a) -> List (Svg a) -> El a
svg xs ys =
    Element.html (Svg.svg xs ys)


attr : String -> String -> Attr a
attr k v =
    [ Element.htmlAttribute (Html.Attributes.attribute k v) ]


attr_ : Html.Attribute a -> Attr a
attr_ x =
    [ Element.htmlAttribute x ]



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


above : El a -> Attr a
above x =
    [ Element.above x ]


below : El a -> Attr a
below x =
    [ Element.below x ]


inFrontOf : El a -> Attr a
inFrontOf x =
    [ Element.inFront x ]


behind : El a -> Attr a
behind x =
    [ Element.behindContent x ]


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


sansSerif : Font
sansSerif =
    Font.sansSerif


typeface : String -> Font
typeface =
    Font.typeface


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


autocomplete : Bool -> Attr a
autocomplete b =
    attr_ (Html.Attributes.autocomplete b)


cursorText : Attr a
cursorText =
    [ Element.htmlAttribute (Html.Attributes.style "cursor" "text") ]


id : String -> Attr a
id x =
    attr_ (Html.Attributes.id x)


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


onDoubleClick : a -> Attr a
onDoubleClick x =
    [ Events.onDoubleClick x ]


onMouseDown : a -> Attr a
onMouseDown x =
    [ Events.onMouseDown x ]


onFocus : a -> Attr a
onFocus x =
    [ Events.onFocus x ]


onLoseFocus : a -> Attr a
onLoseFocus x =
    [ Events.onLoseFocus x ]


onMouseUp : a -> Attr a
onMouseUp x =
    [ Events.onMouseUp x ]


onMouseEnter : a -> Attr a
onMouseEnter x =
    [ Events.onMouseEnter x ]


onMouseLeave : a -> Attr a
onMouseLeave x =
    [ Events.onMouseLeave x ]


onMouseMove : a -> Attr a
onMouseMove x =
    [ Events.onMouseMove x ]


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
-- Lazy elements


lazy =
    Lazy.lazy


lazy2 =
    Lazy.lazy2


lazy3 =
    Lazy.lazy3


lazy4 =
    Lazy.lazy4


lazy5 =
    Lazy.lazy5
