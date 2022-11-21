module Roots.Ui exposing
    ( none, text, paragraph, paragraphs
    , Attr, Color, El, Font, Option, Svg, above, attr, attrEnv, attrIf, attrWhen, autocomplete, background, behind, below, bold, border, border4, bottom, centerX, centerY, col, cursorText, el, elIf, elWhen, focusStyle, fontCenter, fontColor, fontFamily, height, id, image, inFrontOf, italic, lazy, lazy2, lazy3, lazy4, left, lineHeight, link, maxHeight, maxWidth, monospace, onClick, onDoubleClick, onEnter, onFocus, onLoseFocus, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseUp, padding, padding4, pointer, rgb, right, roundedCorners, row, sansSerif, serif, size, spacing, strikethrough, svg, toHtml, top, typeface, underline, unselectable, width
    )

{-|


# Basic elements

@docs none, text, paragraph, paragraphs, link, svg

-}

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
    List (Attr_ r a)


type Attr_ r a
    = A0 (Element.Attribute a)
    | A1 (r -> Element.Attribute a)
    | A2 (r -> Attr r a)
    | LineHeight Int
    | Size Int


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
    Element.layoutWith { options = options } (toAttrs r attrs) (elem r)


toAttrs : r -> List (Attr r a) -> List (Element.Attribute a)
toAttrs r =
    List.concatMap (toAttrs1 r)


toAttrs1 : r -> Attr r a -> List (Element.Attribute a)
toAttrs1 r =
    List.concatMap (toAttrs_ r)


toAttrs_ : r -> Attr_ r a -> List (Element.Attribute a)
toAttrs_ r a0 =
    case a0 of
        A0 a1 ->
            [ a1 ]

        A1 a1 ->
            [ a1 r ]

        A2 a1 ->
            toAttrs1 r (a1 r)

        -- You're not supposed to get here where it matters (in a paragraph), because we intercept attrs with both
        -- lineHeight and size and use it to compute paragraph spacing.
        LineHeight _ ->
            []

        Size px ->
            [ Font.size px ]


toElems : r -> List (El r a) -> List (Element.Element a)
toElems r es =
    List.map (\e -> e r) es


focusStyle : Element.FocusStyle -> Option
focusStyle =
    Element.focusStyle



------------------------------------------------------------------------------------------------------------------------
-- Basic elements


none : El r a
none _ =
    Element.none


text : List (Attr r a) -> String -> El r a
text attrs s r =
    Element.el (toAttrs r attrs) (Element.text s)


paragraph : List (Attr r a) -> List (El r a) -> El r a
paragraph attrs0 es r =
    let
        attrs1 =
            List.concat attrs0

        attrs2 =
            case pluckParagraphSpacing attrs1 of
                Nothing ->
                    attrs1

                Just ( lh, s ) ->
                    A0 (Element.spacing (lh - s)) :: attrs1
    in
    Element.paragraph (List.concatMap (toAttrs_ r) attrs2) (toElems r es)


paragraphs : List (Attr r a) -> List (List (El r a)) -> El r a
paragraphs attrs0 es r =
    let
        attrs1 =
            List.concat attrs0

        attrs2 =
            List.concatMap (toAttrs_ r) attrs1

        paragraphSpacing =
            pluckParagraphSpacing attrs1

        columnAttrs =
            case paragraphSpacing of
                Nothing ->
                    attrs2

                Just ( lh, _ ) ->
                    Element.spacing lh :: attrs2

        paragraphAttrs =
            case paragraphSpacing of
                Nothing ->
                    []

                Just ( lh, s ) ->
                    [ Element.spacing (lh - s) ]
    in
    Element.column
        columnAttrs
        (List.map (\e -> Element.paragraph paragraphAttrs (toElems r e)) es)


{-| Try to pluck paragraph spacing (line height, size) out of a list of attributes.
-}
pluckParagraphSpacing : List (Attr_ r a) -> Maybe ( Int, Int )
pluckParagraphSpacing =
    let
        loopWithLineHeight : Int -> List (Attr_ r a) -> Maybe ( Int, Int )
        loopWithLineHeight lh attrs0 =
            case attrs0 of
                -- eh? line height without size
                [] ->
                    Nothing

                a0 :: attrs ->
                    case a0 of
                        Size s ->
                            Just ( lh, s )

                        _ ->
                            loopWithLineHeight lh attrs

        loopWithSize : Int -> List (Attr_ r a) -> Maybe ( Int, Int )
        loopWithSize s attrs0 =
            case attrs0 of
                [] ->
                    Nothing

                a0 :: attrs ->
                    case a0 of
                        LineHeight lh ->
                            Just ( lh, s )

                        _ ->
                            loopWithSize s attrs

        loop : List (Attr_ r a) -> Maybe ( Int, Int )
        loop attrs0 =
            case attrs0 of
                [] ->
                    Nothing

                a0 :: attrs ->
                    case a0 of
                        LineHeight lh ->
                            loopWithLineHeight lh attrs

                        Size s ->
                            loopWithSize s attrs

                        _ ->
                            loop attrs
    in
    loop


link : List (Attr r a) -> { label : String, url : String } -> El r a
link attrs { label, url } r =
    Element.link (toAttrs r attrs) { label = Element.text label, url = url }


svg : List (Html.Attribute a) -> List (Svg a) -> El r a
svg xs ys _ =
    Element.html (Svg.svg xs ys)


attr : String -> String -> Attr r a
attr k v =
    attr_ (Html.Attributes.attribute k v)


attr_ : Html.Attribute a -> Attr r a
attr_ x =
    [ A0 (Element.htmlAttribute x) ]



------------------------------------------------------------------------------------------------------------------------
-- Container elements


el : List (Attr r a) -> El r a -> El r a
el attrs e r =
    Element.el (toAttrs r attrs) (e r)


col : List (Attr r a) -> List (El r a) -> El r a
col attrs es r =
    Element.column (toAttrs r attrs) (toElems r es)


row : List (Attr r a) -> List (El r a) -> El r a
row attrs es r =
    Element.row (toAttrs r attrs) (toElems r es)


attrEnv : (r -> Attr r a) -> Attr r a
attrEnv f =
    [ A2 f ]



------------------------------------------------------------------------------------------------------------------------
-- Positioning


above : El r a -> Attr r a
above e =
    [ A1 (\r -> Element.above (e r)) ]


below : El r a -> Attr r a
below e =
    [ A1 (\r -> Element.below (e r)) ]


inFrontOf : El r a -> Attr r a
inFrontOf e =
    [ A1 (\r -> Element.inFront (e r)) ]


behind : El r a -> Attr r a
behind e =
    [ A1 (\r -> Element.behindContent (e r)) ]


bottom : Attr r a
bottom =
    [ A0 Element.alignBottom ]


left : Attr r a
left =
    [ A0 Element.alignLeft ]


right : Attr r a
right =
    [ A0 Element.alignRight ]


top : Attr r a
top =
    [ A0 Element.alignTop ]


centerX : Attr r a
centerX =
    [ A0 Element.centerX ]


centerY : Attr r a
centerY =
    [ A0 Element.centerY ]



------------------------------------------------------------------------------------------------------------------------
-- Sizing / padding / spacing


height : Int -> Attr r a
height px =
    [ A0 (Element.height (Element.px px)) ]


maxHeight : Attr r a
maxHeight =
    [ A0 (Element.height Element.fill) ]


width : Int -> Attr r a
width px =
    [ A0 (Element.width (Element.px px)) ]


maxWidth : Attr r a
maxWidth =
    [ A0 (Element.width Element.fill) ]


padding : Int -> Int -> Int -> Int -> Attr r a
padding a b c d =
    [ A0 (Element.paddingEach { top = a, right = b, bottom = c, left = d }) ]


padding4 : Int -> Attr r a
padding4 px =
    [ A0 (Element.padding px) ]


spacing : Int -> Attr r a
spacing px =
    [ A0 (Element.spacing px) ]



------------------------------------------------------------------------------------------------------------------------
-- Font


fontFamily : List Font -> Attr r a
fontFamily xs =
    [ A0 (Font.family xs) ]


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
size px =
    [ Size px ]


{-| Set the line height of a paragraph. This attribute has no effect if it is not paired with @size@, because both
attributes are needed to compute the paragraph's spacing.
-}
lineHeight : Int -> Attr r a
lineHeight px =
    [ LineHeight px ]


fontColor : Color -> Attr r a
fontColor c =
    [ A0 (Font.color c) ]


bold : Attr r a
bold =
    [ A0 Font.bold ]


italic : Attr r a
italic =
    [ A0 Font.italic ]


underline : Attr r a
underline =
    [ A0 Font.underline ]


strikethrough : Attr r a
strikethrough =
    [ A0 Font.strike ]


fontCenter : Attr r a
fontCenter =
    [ A0 Font.center ]



------------------------------------------------------------------------------------------------------------------------
-- Border


border : Color -> Int -> Int -> Int -> Int -> Attr r a
border co a b c d =
    [ A0 (Border.color co)
    , A0 Border.solid
    , A0 (Border.widthEach { top = a, right = b, bottom = c, left = d })
    ]


border4 : Color -> Int -> Attr r a
border4 c px =
    [ A0 (Border.color c)
    , A0 Border.solid
    , A0 (Border.width px)
    ]


roundedCorners : Int -> Attr r a
roundedCorners px =
    [ A0 (Border.rounded px) ]



------------------------------------------------------------------------------------------------------------------------
-- Color


background : Color -> Attr r a
background c =
    [ A0 (Background.color c) ]


rgb : Int -> Int -> Int -> Color
rgb =
    Element.rgb255



------------------------------------------------------------------------------------------------------------------------
-- Misc


autocomplete : Bool -> Attr r a
autocomplete b =
    attr_ (Html.Attributes.autocomplete b)


cursorText : Attr r a
cursorText =
    [ A0 (Element.htmlAttribute (Html.Attributes.style "cursor" "text")) ]


id : String -> Attr r a
id x =
    attr_ (Html.Attributes.id x)


pointer : Attr r a
pointer =
    [ A0 Element.pointer ]


unselectable : Attr r a
unselectable =
    [ A0 (Element.htmlAttribute (Html.Attributes.style "user-select" "none")) ]



------------------------------------------------------------------------------------------------------------------------
-- Images


image : List (Attr r a) -> { src : String, description : String } -> El r a
image attrs img r =
    Element.image (toAttrs r attrs) img



------------------------------------------------------------------------------------------------------------------------
-- Events


onClick : a -> Attr r a
onClick x =
    [ A0 (Events.onClick x) ]


onDoubleClick : a -> Attr r a
onDoubleClick x =
    [ A0 (Events.onDoubleClick x) ]


onMouseDown : a -> Attr r a
onMouseDown x =
    [ A0 (Events.onMouseDown x) ]


onFocus : a -> Attr r a
onFocus x =
    [ A0 (Events.onFocus x) ]


onLoseFocus : a -> Attr r a
onLoseFocus x =
    [ A0 (Events.onLoseFocus x) ]


onMouseUp : a -> Attr r a
onMouseUp x =
    [ A0 (Events.onMouseUp x) ]


onMouseEnter : a -> Attr r a
onMouseEnter x =
    [ A0 (Events.onMouseEnter x) ]


onMouseLeave : a -> Attr r a
onMouseLeave x =
    [ A0 (Events.onMouseLeave x) ]


onMouseMove : a -> Attr r a
onMouseMove x =
    [ A0 (Events.onMouseMove x) ]


onEnter : a -> Attr r a
onEnter x =
    [ A0
        (Element.htmlAttribute
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
        )
    ]



------------------------------------------------------------------------------------------------------------------------
-- Conditional elements/attributes


attrIf : Bool -> List (Attr r a) -> Attr r a
attrIf b attrs =
    if b then
        List.concat attrs

    else
        []


attrWhen : Maybe a -> (a -> List (Attr r b)) -> Attr r b
attrWhen mattrs f =
    case mattrs of
        Nothing ->
            []

        Just attrs ->
            List.concat (f attrs)


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
