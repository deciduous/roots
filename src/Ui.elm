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
    , image
    , inFrontOf
    , italic
    , lazy
    , lazy2
    , lazy3
    , lazy4
    , left
    , link
    , maxHeight
    , maxWidth
    , monospace
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
    , serif
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


type alias Attr r a =
    r -> List (Element.Attribute a)


type alias Color =
    Element.Color


type alias El r a =
    r -> Element.Element a


type alias Font =
    Font.Font


type alias Option =
    Element.Option


type alias Svg a =
    Svg.Svg a


toHtml : List Option -> List (Attr r a) -> El r a -> r -> Html a
toHtml options attrs elem r =
    Element.layoutWith { options = options } (List.concat (List.map (\a -> a r) attrs)) (elem r)


focusStyle : Element.FocusStyle -> Option
focusStyle =
    Element.focusStyle



------------------------------------------------------------------------------------------------------------------------
-- Basic elements


none : El r a
none _ =
    Element.none


text : String -> El r a
text s _ =
    Element.text s


paragraph : List (Attr r a) -> List (El r a) -> El r a
paragraph attrs es r =
    Element.paragraph (List.concat (List.map (\a -> a r) attrs)) (List.map (\e -> e r) es)


link : List (Attr r a) -> { label : El r a, url : String } -> El r a
link attrs { label, url } r =
    Element.link (List.concat (List.map (\a -> a r) attrs)) { label = label r, url = url }


svg : List (Html.Attribute a) -> List (Svg a) -> El r a
svg xs ys _ =
    Element.html (Svg.svg xs ys)


attr : String -> String -> Attr r a
attr k v _ =
    [ Element.htmlAttribute (Html.Attributes.attribute k v) ]


attr_ : Html.Attribute a -> Attr r a
attr_ x _ =
    [ Element.htmlAttribute x ]



------------------------------------------------------------------------------------------------------------------------
-- Container elements


el : List (Attr r a) -> El r a -> El r a
el attrs e r =
    Element.el (List.concat (List.map (\a -> a r) attrs)) (e r)


col : List (Attr r a) -> List (El r a) -> El r a
col attrs es r =
    Element.column (List.concat (List.map (\a -> a r) attrs)) (List.map (\e -> e r) es)


row : List (Attr r a) -> List (El r a) -> El r a
row attrs es r =
    Element.row (List.concat (List.map (\a -> a r) attrs)) (List.map (\e -> e r) es)



------------------------------------------------------------------------------------------------------------------------
-- Positioning


above : El r a -> Attr r a
above e r =
    [ Element.above (e r) ]


below : El r a -> Attr r a
below e r =
    [ Element.below (e r) ]


inFrontOf : El r a -> Attr r a
inFrontOf e r =
    [ Element.inFront (e r) ]


behind : El r a -> Attr r a
behind e r =
    [ Element.behindContent (e r) ]


bottom : Attr r a
bottom _ =
    [ Element.alignBottom ]


left : Attr r a
left _ =
    [ Element.alignLeft ]


right : Attr r a
right _ =
    [ Element.alignRight ]


top : Attr r a
top _ =
    [ Element.alignTop ]


centerX : Attr r a
centerX _ =
    [ Element.centerX ]


centerY : Attr r a
centerY _ =
    [ Element.centerY ]



------------------------------------------------------------------------------------------------------------------------
-- Sizing / padding / spacing


height : Int -> Attr r a
height px _ =
    [ Element.height (Element.px px) ]


maxHeight : Attr r a
maxHeight _ =
    [ Element.height Element.fill ]


width : Int -> Attr r a
width px _ =
    [ Element.width (Element.px px) ]


maxWidth : Attr r a
maxWidth _ =
    [ Element.width Element.fill ]


padding : Int -> Int -> Int -> Int -> Attr r a
padding a b c d _ =
    [ Element.paddingEach { top = a, right = b, bottom = c, left = d } ]


padding4 : Int -> Attr r a
padding4 px _ =
    [ Element.padding px ]


spacing : Int -> Attr r a
spacing px _ =
    [ Element.spacing px ]



------------------------------------------------------------------------------------------------------------------------
-- Font


fontFamily : List Font -> Attr r a
fontFamily xs _ =
    [ Font.family xs ]


serif : Font
serif =
    Font.serif


sansSerif : Font
sansSerif =
    Font.sansSerif


monospace : Font
monospace =
    Font.monospace


typeface : String -> Font
typeface =
    Font.typeface


size : Int -> Attr r a
size px _ =
    [ Font.size px ]


fontColor : Color -> Attr r a
fontColor c _ =
    [ Font.color c ]


bold : Attr r a
bold _ =
    [ Font.bold ]


italic : Attr r a
italic _ =
    [ Font.italic ]


underline : Attr r a
underline _ =
    [ Font.underline ]


strikethrough : Attr r a
strikethrough _ =
    [ Font.strike ]


fontCenter : Attr r a
fontCenter _ =
    [ Font.center ]



------------------------------------------------------------------------------------------------------------------------
-- Border


border : Color -> Int -> Int -> Int -> Int -> Attr r a
border co a b c d _ =
    [ Border.color co
    , Border.solid
    , Border.widthEach { top = a, right = b, bottom = c, left = d }
    ]


border4 : Color -> Int -> Attr r a
border4 c px _ =
    [ Border.color c
    , Border.solid
    , Border.width px
    ]


roundedCorners : Int -> Attr r a
roundedCorners px _ =
    [ Border.rounded px ]



------------------------------------------------------------------------------------------------------------------------
-- Color


background : Color -> Attr r a
background c _ =
    [ Background.color c ]


rgb : Int -> Int -> Int -> Color
rgb =
    Element.rgb255



------------------------------------------------------------------------------------------------------------------------
-- Misc


autocomplete : Bool -> Attr r a
autocomplete b =
    attr_ (Html.Attributes.autocomplete b)


cursorText : Attr r a
cursorText _ =
    [ Element.htmlAttribute (Html.Attributes.style "cursor" "text") ]


id : String -> Attr r a
id x =
    attr_ (Html.Attributes.id x)


pointer : Attr r a
pointer _ =
    [ Element.pointer ]


unselectable : Attr r a
unselectable _ =
    [ Element.htmlAttribute (Html.Attributes.style "user-select" "none") ]



------------------------------------------------------------------------------------------------------------------------
-- Images


image : List (Attr r a) -> { src : String, description : String } -> El r a
image attrs img r =
    Element.image (List.concat (List.map (\a -> a r) attrs)) img



------------------------------------------------------------------------------------------------------------------------
-- Events


onClick : a -> Attr r a
onClick x _ =
    [ Events.onClick x ]


onDoubleClick : a -> Attr r a
onDoubleClick x _ =
    [ Events.onDoubleClick x ]


onMouseDown : a -> Attr r a
onMouseDown x _ =
    [ Events.onMouseDown x ]


onFocus : a -> Attr r a
onFocus x _ =
    [ Events.onFocus x ]


onLoseFocus : a -> Attr r a
onLoseFocus x _ =
    [ Events.onLoseFocus x ]


onMouseUp : a -> Attr r a
onMouseUp x _ =
    [ Events.onMouseUp x ]


onMouseEnter : a -> Attr r a
onMouseEnter x _ =
    [ Events.onMouseEnter x ]


onMouseLeave : a -> Attr r a
onMouseLeave x _ =
    [ Events.onMouseLeave x ]


onMouseMove : a -> Attr r a
onMouseMove x _ =
    [ Events.onMouseMove x ]


onEnter : a -> Attr r a
onEnter x _ =
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


attrIf : Bool -> List (Attr r a) -> Attr r a
attrIf b attrs r =
    if b then
        List.concat (List.map (\a -> a r) attrs)

    else
        []


attrWhen : Maybe a -> (a -> List (Attr r b)) -> Attr r b
attrWhen mattrs f r =
    case mattrs of
        Nothing ->
            []

        Just attrs ->
            List.concat (List.map (\a -> a r) (f attrs))


elIf : Bool -> El r a -> El r a
elIf b e r =
    if b then
        e r

    else
        Element.none


elWhen : Maybe a -> (a -> El r b) -> El r b
elWhen me f r =
    case me of
        Nothing ->
            Element.none

        Just e ->
            f e r



------------------------------------------------------------------------------------------------------------------------
-- Lazy elements


lazy : (a -> El r b) -> a -> El r b
lazy =
    Lazy.lazy2


lazy2 : (a -> b -> El r c) -> a -> b -> El r c
lazy2 =
    Lazy.lazy3


lazy3 : (a -> b -> c -> El r d) -> a -> b -> c -> El r d
lazy3 =
    Lazy.lazy4


lazy4 : (a -> b -> c -> d -> El r e) -> a -> b -> c -> d -> El r e
lazy4 =
    Lazy.lazy5
