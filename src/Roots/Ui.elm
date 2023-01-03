module Roots.Ui exposing
    ( El, Attr, none, text, paragraph, link, svg, br
    , el, col, row, elEnv, attrEnv
    , checkbox, textBox, Label, labelAbove, labelBelow, labelLeft, labelRight, labelHidden
    , above, onRight, below, onLeft, inFrontOf, behind, top, right, bottom, left, centerX, centerY
    , height, maxHeight, width, maxWidth, padding, padding4, spacing
    , Font, fontFamily, serif, sansSerif, monospace, typeface, size, lineHeight, fontColor, bold, italic, underline, strikethrough, fontCenter, fontLeft, fontRight, fontJustify
    , Color, Option, Svg, Touch, TouchEvent, attr, attrIf, attrWhen, autocomplete, background, border, border4, cursorText, elIf, elWhen, focusStyle, id, image, lazy, lazy2, lazy3, lazy4, onClick, onDoubleClick, onEnter, onFocus, onLoseFocus, onMouseDown, onMouseEnter, onMouseLeave, onMouseMove, onMouseUp, onTouchCancel, onTouchEnd, onTouchMove, onTouchStart, pointer, rgb, roundedCorners, toHex, toHtml, unselectable
    )

{-|


# Basic elements

@docs El, Attr, none, text, paragraph, link, svg, br


# Container elements

@docs el, col, row, elEnv, attrEnv


# Input elements

@docs checkbox, textBox, Label, labelAbove, labelBelow, labelLeft, labelRight, labelHidden


# Positioning

@docs above, onRight, below, onLeft, inFrontOf, behind, top, right, bottom, left, centerX, centerY


# Sizing / padding / spacing

@docs height, maxHeight, width, maxWidth, padding, padding4, spacing


# Fonts

@docs Font, fontFamily, serif, sansSerif, monospace, typeface, size, lineHeight, fontColor, bold, italic, underline, strikethrough, fontCenter, fontLeft, fontRight, fontJustify

-}

import Array exposing (Array)
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Roots exposing (T11(..), T4(..), ifte)
import Roots.Json as Json
import Svg


type alias Attr r a =
    List (Attr_ r a)


type Attr_ r a
    = A0 (Element.Attribute a)
    | A1 (r -> Element.Attribute a)
    | A2 (r -> Attr r a)
    | LineHeight Int
    | Size Int


type El r a
    = E0 (Element.Element a)
    | E1 (r -> Element.Element a)
    | E2 (r -> El r a)


type alias Font =
    Font.Font


type alias Option =
    Element.Option


type alias Svg a =
    Svg.Svg a


toHtml : List Option -> List (Attr r a) -> El r a -> r -> Html a
toHtml options attrs e r =
    Element.layoutWith { options = options } (toAttrs r attrs) (toElem r e)


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


toElem : r -> El r a -> Element.Element a
toElem r e0 =
    case e0 of
        E0 e ->
            e

        E1 e ->
            e r

        E2 e ->
            toElem r (e r)


toElems : r -> List (El r a) -> List (Element.Element a)
toElems r =
    List.map (toElem r)


focusStyle : Element.FocusStyle -> Option
focusStyle =
    Element.focusStyle



------------------------------------------------------------------------------------------------------------------------
-- Basic elements


none : El r a
none =
    E0 Element.none


text : List (Attr r a) -> String -> El r a
text attrs s =
    E1 (\r -> Element.el (toAttrs r attrs) (Element.text s))


paragraph : List (Attr r a) -> List (El r a) -> El r a
paragraph attrs0 es =
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
    E1 (\r -> Element.paragraph (List.concatMap (toAttrs_ r) attrs2) (toElems r es))


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


link : List (Attr r a) -> { label : El r a, newTab : Bool, url : String } -> El r a
link attrs { label, newTab, url } =
    E1
        (\r ->
            ifte
                newTab
                Element.newTabLink
                Element.link
                (toAttrs r attrs)
                { label = toElem r label, url = url }
        )


svg : List (Html.Attribute a) -> List (Svg a) -> El r a
svg xs ys =
    E0 (Element.html (Svg.svg xs ys))


br : El r a
br =
    E0 (Element.html (Html.br [] []))


attr : String -> String -> Attr r a
attr k v =
    attr_ (Html.Attributes.attribute k v)


attr_ : Html.Attribute a -> Attr r a
attr_ x =
    [ A0 (Element.htmlAttribute x) ]



------------------------------------------------------------------------------------------------------------------------
-- Container elements


el : List (Attr r a) -> El r a -> El r a
el attrs e =
    E1 (\r -> Element.el (toAttrs r attrs) (toElem r e))


col : List (Attr r a) -> List (El r a) -> El r a
col attrs es =
    E1 (\r -> Element.column (toAttrs r attrs) (toElems r es))


row : List (Attr r a) -> List (El r a) -> El r a
row attrs es =
    E1 (\r -> Element.row (toAttrs r attrs) (toElems r es))


elEnv : (r -> El r a) -> El r a
elEnv =
    E2


attrEnv : (r -> Attr r a) -> Attr r a
attrEnv f =
    [ A2 f ]



------------------------------------------------------------------------------------------------------------------------
-- Input elements


checkbox :
    List (Attr r a)
    ->
        { checked : Bool
        , icon : Bool -> El r a
        , label : Label r a
        , onChange : Bool -> a
        }
    -> El r a
checkbox attrs c =
    E1
        (\r ->
            Input.checkbox
                (toAttrs r attrs)
                { checked = c.checked
                , icon = \b -> toElem r (c.icon b)
                , label = c.label r
                , onChange = c.onChange
                }
        )


textBox :
    List (Attr r a)
    ->
        { label : Label r a
        , onChange : String -> a
        , placeholder : Maybe { attrs : List (Attr r a), el : El r a }
        , text : String
        }
    -> El r a
textBox attrs t =
    E1
        (\r ->
            Input.text
                (toAttrs r attrs)
                { label = t.label r
                , onChange = t.onChange
                , placeholder = Maybe.map (\p -> Input.placeholder (toAttrs r p.attrs) (toElem r p.el)) t.placeholder
                , text = t.text
                }
        )


type alias Label r a =
    r -> Input.Label a


labelAbove : List (Attr r a) -> El r a -> Label r a
labelAbove attrs e r =
    Input.labelAbove (toAttrs r attrs) (toElem r e)


labelBelow : List (Attr r a) -> El r a -> Label r a
labelBelow attrs e r =
    Input.labelBelow (toAttrs r attrs) (toElem r e)


labelLeft : List (Attr r a) -> El r a -> Label r a
labelLeft attrs e r =
    Input.labelLeft (toAttrs r attrs) (toElem r e)


labelRight : List (Attr r a) -> El r a -> Label r a
labelRight attrs e r =
    Input.labelRight (toAttrs r attrs) (toElem r e)


labelHidden : String -> Label r a
labelHidden s _ =
    Input.labelHidden s



------------------------------------------------------------------------------------------------------------------------
-- Positioning


above : El r a -> Attr r a
above e =
    [ A1 (\r -> Element.above (toElem r e)) ]


onRight : El r a -> Attr r a
onRight e =
    [ A1 (\r -> Element.onRight (toElem r e)) ]


below : El r a -> Attr r a
below e =
    [ A1 (\r -> Element.below (toElem r e)) ]


onLeft : El r a -> Attr r a
onLeft e =
    [ A1 (\r -> Element.onLeft (toElem r e)) ]


inFrontOf : El r a -> Attr r a
inFrontOf e =
    [ A1 (\r -> Element.inFront (toElem r e)) ]


behind : El r a -> Attr r a
behind e =
    [ A1 (\r -> Element.behindContent (toElem r e)) ]


top : Attr r a
top =
    [ A0 Element.alignTop ]


right : Attr r a
right =
    [ A0 Element.alignRight ]


bottom : Attr r a
bottom =
    [ A0 Element.alignBottom ]


left : Attr r a
left =
    [ A0 Element.alignLeft ]


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
    [ A0 (Font.color (toColor c)) ]


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


fontLeft : Attr r a
fontLeft =
    [ A0 Font.alignLeft ]


fontRight : Attr r a
fontRight =
    [ A0 Font.alignRight ]


fontJustify : Attr r a
fontJustify =
    [ A0 Font.justify ]



------------------------------------------------------------------------------------------------------------------------
-- Border


border : Color -> Int -> Int -> Int -> Int -> Attr r a
border c p0 p1 p2 p3 =
    [ A0 (Border.color (toColor c))
    , A0 Border.solid
    , A0 (Border.widthEach { top = p0, right = p1, bottom = p2, left = p3 })
    ]


border4 : Color -> Int -> Attr r a
border4 c px =
    [ A0 (Border.color (toColor c))
    , A0 Border.solid
    , A0 (Border.width px)
    ]


roundedCorners : Int -> Attr r a
roundedCorners px =
    [ A0 (Border.rounded px) ]



------------------------------------------------------------------------------------------------------------------------
-- Color


type Color
    = Color
        { r : Int
        , g : Int
        , b : Int
        }


toColor : Color -> Element.Color
toColor (Color { r, g, b }) =
    Element.rgb255 r g b


background : Color -> Attr r a
background c =
    [ A0 (Background.color (toColor c)) ]


rgb : Int -> Int -> Int -> Color
rgb r g b =
    Color { r = clamp 0 255 r, g = clamp 0 255 g, b = clamp 0 255 b }


{-| Convert a color into a hexadecimal string (without a leading #).
-}
toHex : Color -> String
toHex (Color { r, g, b }) =
    toHex0 r ++ toHex0 g ++ toHex0 b


toHex0 : Int -> String
toHex0 n =
    if n < 16 then
        "0" ++ hexChar n

    else
        hexChar (n // 16) ++ hexChar (modBy 16 n)


hexChar : Int -> String
hexChar n =
    case n of
        0 ->
            "0"

        1 ->
            "1"

        2 ->
            "2"

        3 ->
            "3"

        4 ->
            "4"

        5 ->
            "5"

        6 ->
            "6"

        7 ->
            "7"

        8 ->
            "8"

        9 ->
            "9"

        10 ->
            "a"

        11 ->
            "b"

        12 ->
            "c"

        13 ->
            "d"

        14 ->
            "e"

        _ ->
            "f"



------------------------------------------------------------------------------------------------------------------------
-- Misc


autocomplete : Bool -> Attr r a
autocomplete b =
    attr_ (Html.Attributes.autocomplete b)


cursorText : Attr r a
cursorText =
    attr_ (Html.Attributes.style "cursor" "text")


id : String -> Attr r a
id x =
    attr_ (Html.Attributes.id x)


pointer : Attr r a
pointer =
    [ A0 Element.pointer ]


unselectable : Attr r a
unselectable =
    attr_ (Html.Attributes.style "user-select" "none")



------------------------------------------------------------------------------------------------------------------------
-- Images


image : List (Attr r a) -> { src : String, description : String } -> El r a
image attrs img =
    E1 (\r -> Element.image (toAttrs r attrs) img)



------------------------------------------------------------------------------------------------------------------------
-- Events


onClick : a -> Attr r a
onClick message =
    [ A0 (Events.onClick message) ]


onDoubleClick : a -> Attr r a
onDoubleClick message =
    [ A0 (Events.onDoubleClick message) ]


onEnter : a -> Attr r a
onEnter message =
    on "keyup"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key ->
                    if key == "Enter" then
                        Json.Decode.succeed message

                    else
                        Json.Decode.fail ""
                )
        )


onFocus : a -> Attr r a
onFocus message =
    [ A0 (Events.onFocus message) ]


onLoseFocus : a -> Attr r a
onLoseFocus message =
    [ A0 (Events.onLoseFocus message) ]


onMouseDown : a -> Attr r a
onMouseDown message =
    [ A0 (Events.onMouseDown message) ]


onMouseEnter : a -> Attr r a
onMouseEnter message =
    [ A0 (Events.onMouseEnter message) ]


onMouseLeave : a -> Attr r a
onMouseLeave message =
    [ A0 (Events.onMouseLeave message) ]


onMouseMove : a -> Attr r a
onMouseMove message =
    [ A0 (Events.onMouseMove message) ]


onMouseUp : a -> Attr r a
onMouseUp message =
    [ A0 (Events.onMouseUp message) ]


{-| <https://developer.mozilla.org/en-US/docs/Web/API/Touch>
-}
type alias Touch =
    { clientX : Float
    , clientY : Float
    , force : Float
    , identifier : Int
    , pageX : Float
    , pageY : Float
    , radiusX : Float
    , radiusY : Float
    , rotationAngle : Float
    , screenX : Int
    , screenY : Int
    }


touchDecoder : Json.Decoder Touch
touchDecoder =
    let
        fromTuple clientX clientY force identifier pageX pageY radiusX radiusY rotationAngle screenX screenY =
            { clientX = clientX
            , clientY = clientY
            , force = force
            , identifier = identifier
            , pageX = pageX
            , pageY = pageY
            , radiusX = radiusX
            , radiusY = radiusY
            , rotationAngle = rotationAngle
            , screenX = screenX
            , screenY = screenY
            }

        toTuple x =
            T11
                x.clientX
                x.clientY
                x.force
                x.identifier
                x.pageX
                x.pageY
                x.radiusX
                x.radiusY
                x.rotationAngle
                x.screenX
                x.screenY
    in
    Json.toDecoder
        (Json.object11
            fromTuple
            toTuple
            ( "clientX", Json.float )
            ( "clientY", Json.float )
            ( "force", Json.float )
            ( "identifier", Json.int )
            ( "pageX", Json.float )
            ( "pageY", Json.float )
            ( "radiusX", Json.float )
            ( "radiusY", Json.float )
            ( "rotationAngle", Json.float )
            ( "screenX", Json.int )
            ( "screenY", Json.int )
        )


touchListDecoder : Json.Decoder (Array Touch)
touchListDecoder =
    Json.Decode.field "length" Json.Decode.int
        |> Json.Decode.andThen
            (\length ->
                let
                    loop : Array Touch -> Int -> Json.Decoder (Array Touch)
                    loop acc i =
                        if i == length then
                            Json.Decode.succeed acc

                        else
                            Json.Decode.field (String.fromInt i) touchDecoder
                                |> Json.Decode.andThen (\touch -> loop (Array.push touch acc) (i + 1))
                in
                loop Array.empty 0
            )


{-| <https://developer.mozilla.org/en-US/docs/Web/API/TouchEvent>
-}
type alias TouchEvent =
    { changedTouches : Array Touch
    , ctrlKey : Bool
    , metaKey : Bool
    , shiftKey : Bool
    , targetTouches : Array Touch
    , touches : Array Touch
    }


touchEventDecoder : Json.Decoder TouchEvent
touchEventDecoder =
    Json.Decode.map6
        (\changedTouches ctrlKey metaKey shiftKey targetTouches touches ->
            { changedTouches = changedTouches
            , ctrlKey = ctrlKey
            , metaKey = metaKey
            , shiftKey = shiftKey
            , targetTouches = targetTouches
            , touches = touches
            }
        )
        (Json.Decode.field "changedTouches" touchListDecoder)
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)
        (Json.Decode.field "shiftKey" Json.Decode.bool)
        (Json.Decode.field "targetTouches" touchListDecoder)
        (Json.Decode.field "touches" touchListDecoder)


onTouchCancel : (TouchEvent -> a) -> Attr r a
onTouchCancel toMessage =
    on "touchcancel" (Json.Decode.map toMessage touchEventDecoder)


onTouchEnd : (TouchEvent -> a) -> Attr r a
onTouchEnd toMessage =
    on "touchend" (Json.Decode.map toMessage touchEventDecoder)


onTouchMove : (TouchEvent -> a) -> Attr r a
onTouchMove toMessage =
    on "touchmove" (Json.Decode.map toMessage touchEventDecoder)


onTouchStart : (TouchEvent -> a) -> Attr r a
onTouchStart toMessage =
    on "touchstart" (Json.Decode.map toMessage touchEventDecoder)


on : String -> Json.Decoder a -> Attr r a
on event decoder =
    attr_ (Html.Events.on event decoder)



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
elIf b e =
    if b then
        e

    else
        E0 Element.none


elWhen : Maybe a -> (a -> El r b) -> El r b
elWhen me f =
    case me of
        Nothing ->
            E0 Element.none

        Just e ->
            f e



------------------------------------------------------------------------------------------------------------------------
-- Lazy elements


lazy : (a -> El r b) -> a -> El r b
lazy f a0 =
    E1 (Lazy.lazy2 (\a r -> toElem r (f a)) a0)


lazy2 : (a -> b -> El r c) -> a -> b -> El r c
lazy2 f a0 b0 =
    E1 (Lazy.lazy3 (\a b r -> toElem r (f a b)) a0 b0)


lazy3 : (a -> b -> c -> El r d) -> a -> b -> c -> El r d
lazy3 f a0 b0 c0 =
    E1 (Lazy.lazy4 (\a b c r -> toElem r (f a b c)) a0 b0 c0)


lazy4 : (a -> b -> c -> d -> El r e) -> a -> b -> c -> d -> El r e
lazy4 f a0 b0 c0 d0 =
    E1 (Lazy.lazy5 (\a b c d r -> toElem r (f a b c d)) a0 b0 c0 d0)
