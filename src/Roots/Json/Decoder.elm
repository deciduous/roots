module Roots.Json.Decoder exposing
    ( null
    , tuple2
    , PropertyDecoder, property, optionalProperty
    , object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
    , enum
    , arrayVariant, objectVariant
    )

{-| Json.

@docs null
@docs tuple2
@docs PropertyDecoder, property, optionalProperty
@docs object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
@docs enum
@docs arrayVariant, objectVariant

-}

import Json.Decode exposing (Decoder)
import Roots exposing (..)


null : Decoder ()
null =
    Json.Decode.null ()


tuple2 : Decoder a -> Decoder b -> Decoder ( a, b )
tuple2 da db =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 da)
        (Json.Decode.index 1 db)


type alias PropertyDecoder a =
    Decoder a


property : String -> Decoder a -> PropertyDecoder a
property =
    Json.Decode.field


optionalProperty : String -> Decoder a -> PropertyDecoder (Maybe a)
optionalProperty key decoder =
    Json.Decode.maybe (Json.Decode.field key decoder)


object1 : (a -> b) -> PropertyDecoder a -> Decoder b
object1 =
    Json.Decode.map


object2 : (a -> b -> c) -> PropertyDecoder a -> PropertyDecoder b -> Decoder c
object2 =
    Json.Decode.map2


object3 :
    (a -> b -> c -> d)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> Decoder d
object3 =
    Json.Decode.map3


object4 :
    (a -> b -> c -> d -> e)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> Decoder e
object4 =
    Json.Decode.map4


object5 :
    (a -> b -> c -> d -> e -> f)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> Decoder f
object5 =
    Json.Decode.map5


object6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> Decoder g
object6 =
    Json.Decode.map6


object7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> PropertyDecoder g
    -> Decoder h
object7 =
    Json.Decode.map7


object8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> PropertyDecoder g
    -> PropertyDecoder h
    -> Decoder i
object8 =
    Json.Decode.map8


object9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> PropertyDecoder g
    -> PropertyDecoder h
    -> PropertyDecoder i
    -> Decoder j
object9 f0 pa pb pc pd pe pf pg ph pi =
    Json.Decode.map8
        (\a b c d e f g ( h, i ) -> f0 a b c d e f g h i)
        pa
        pb
        pc
        pd
        pe
        pf
        pg
        (Json.Decode.map2 (\h i -> ( h, i )) ph pi)


object10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> PropertyDecoder g
    -> PropertyDecoder h
    -> PropertyDecoder i
    -> PropertyDecoder j
    -> Decoder k
object10 f0 pa pb pc pd pe pf pg ph pi pj =
    Json.Decode.map8
        (\a b c d e f g ( h, i, j ) -> f0 a b c d e f g h i j)
        pa
        pb
        pc
        pd
        pe
        pf
        pg
        (Json.Decode.map3 (\h i j -> ( h, i, j )) ph pi pj)


object11 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l)
    -> PropertyDecoder a
    -> PropertyDecoder b
    -> PropertyDecoder c
    -> PropertyDecoder d
    -> PropertyDecoder e
    -> PropertyDecoder f
    -> PropertyDecoder g
    -> PropertyDecoder h
    -> PropertyDecoder i
    -> PropertyDecoder j
    -> PropertyDecoder k
    -> Decoder l
object11 f0 pa pb pc pd pe pf pg ph pi pj pk =
    Json.Decode.map8
        (\a b c d e f g (T4 h i j k) -> f0 a b c d e f g h i j k)
        pa
        pb
        pc
        pd
        pe
        pf
        pg
        (Json.Decode.map4 T4 ph pi pj pk)


enum : Decoder t -> List ( t, a ) -> Decoder a
enum typeDecoder valueDecoders =
    Json.Decode.oneOf
        (List.map
            (\( typ, value ) ->
                typeDecoder
                    |> Json.Decode.andThen
                        (\typ2 ->
                            if typ == typ2 then
                                Json.Decode.succeed value

                            else
                                Json.Decode.fail ""
                        )
            )
            valueDecoders
        )


arrayVariant : Decoder t -> List ( t, Decoder a ) -> Decoder a
arrayVariant typeDecoder valueDecoders =
    Json.Decode.oneOf
        (List.map
            (\( typ, valueDecoder ) ->
                Json.Decode.index 0 typeDecoder
                    |> Json.Decode.andThen
                        (\typ2 ->
                            if typ == typ2 then
                                Json.Decode.index 1 valueDecoder

                            else
                                Json.Decode.fail ""
                        )
            )
            valueDecoders
        )


objectVariant : String -> Decoder t -> String -> List ( t, Decoder a ) -> Decoder a
objectVariant typeKey typeDecoder valueKey valueDecoders =
    Json.Decode.oneOf
        (List.map
            (\( typ, valueDecoder ) ->
                Json.Decode.field typeKey typeDecoder
                    |> Json.Decode.andThen
                        (\typ2 ->
                            if typ == typ2 then
                                Json.Decode.field valueKey valueDecoder

                            else
                                Json.Decode.fail ""
                        )
            )
            valueDecoders
        )
