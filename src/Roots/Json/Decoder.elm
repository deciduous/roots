module Roots.Json.Decoder exposing
    ( null
    , tuple2
    , PropertyDecoder, property, optionalProperty
    , object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
    , VariantDecoder, objectVariantDecoder
    , variant2, variant3, variant4, variant5, variant6
    )

{-| Json.

@docs null
@docs tuple2
@docs PropertyDecoder, property, optionalProperty
@docs object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
@docs VariantDecoder, objectVariantDecoder
@docs variant2, variant3, variant4, variant5, variant6

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


type alias VariantDecoder t a =
    { typeDecoder : Decoder t
    , valueDecoder : Decoder a -> Decoder a
    }


{-| A two-key object decoder.
-}
objectVariantDecoder : String -> String -> Decoder t -> VariantDecoder t a
objectVariantDecoder typeKey valueKey typeDecoder =
    { typeDecoder = Json.Decode.field typeKey typeDecoder
    , valueDecoder = Json.Decode.field valueKey
    }


variant2 :
    VariantDecoder t c
    -> ( t, a -> c, Decoder a )
    -> ( t, b -> c, Decoder b )
    -> (t -> String)
    -> Decoder c
variant2 variantDecoder ( t0, p0, d0 ) ( t1, p1, d1 ) unrecognized =
    variantDecoder.typeDecoder
        |> Json.Decode.andThen
            (\typ ->
                variantDecoder.valueDecoder
                    (if typ == t0 then
                        Json.Decode.map p0 d0

                     else if typ == t1 then
                        Json.Decode.map p1 d1

                     else
                        Json.Decode.fail (unrecognized typ)
                    )
            )


variant3 :
    VariantDecoder t d
    -> ( t, a -> d, Decoder a )
    -> ( t, b -> d, Decoder b )
    -> ( t, c -> d, Decoder c )
    -> (t -> String)
    -> Decoder d
variant3 variantDecoder ( t0, p0, d0 ) ( t1, p1, d1 ) ( t2, p2, d2 ) unrecognized =
    variantDecoder.typeDecoder
        |> Json.Decode.andThen
            (\typ ->
                variantDecoder.valueDecoder
                    (if typ == t0 then
                        Json.Decode.map p0 d0

                     else if typ == t1 then
                        Json.Decode.map p1 d1

                     else if typ == t2 then
                        Json.Decode.map p2 d2

                     else
                        Json.Decode.fail (unrecognized typ)
                    )
            )


variant4 :
    VariantDecoder t e
    -> ( t, a -> e, Decoder a )
    -> ( t, b -> e, Decoder b )
    -> ( t, c -> e, Decoder c )
    -> ( t, d -> e, Decoder d )
    -> (t -> String)
    -> Decoder e
variant4 variantDecoder ( t0, p0, d0 ) ( t1, p1, d1 ) ( t2, p2, d2 ) ( t3, p3, d3 ) unrecognized =
    variantDecoder.typeDecoder
        |> Json.Decode.andThen
            (\typ ->
                variantDecoder.valueDecoder
                    (if typ == t0 then
                        Json.Decode.map p0 d0

                     else if typ == t1 then
                        Json.Decode.map p1 d1

                     else if typ == t2 then
                        Json.Decode.map p2 d2

                     else if typ == t3 then
                        Json.Decode.map p3 d3

                     else
                        Json.Decode.fail (unrecognized typ)
                    )
            )


variant5 :
    VariantDecoder t f
    -> ( t, a -> f, Decoder a )
    -> ( t, b -> f, Decoder b )
    -> ( t, c -> f, Decoder c )
    -> ( t, d -> f, Decoder d )
    -> ( t, e -> f, Decoder e )
    -> (t -> String)
    -> Decoder f
variant5 variantDecoder ( t0, p0, d0 ) ( t1, p1, d1 ) ( t2, p2, d2 ) ( t3, p3, d3 ) ( t4, p4, d4 ) unrecognized =
    variantDecoder.typeDecoder
        |> Json.Decode.andThen
            (\typ ->
                variantDecoder.valueDecoder
                    (if typ == t0 then
                        Json.Decode.map p0 d0

                     else if typ == t1 then
                        Json.Decode.map p1 d1

                     else if typ == t2 then
                        Json.Decode.map p2 d2

                     else if typ == t3 then
                        Json.Decode.map p3 d3

                     else if typ == t4 then
                        Json.Decode.map p4 d4

                     else
                        Json.Decode.fail (unrecognized typ)
                    )
            )


variant6 :
    VariantDecoder t g
    -> ( t, a -> g, Decoder a )
    -> ( t, b -> g, Decoder b )
    -> ( t, c -> g, Decoder c )
    -> ( t, d -> g, Decoder d )
    -> ( t, e -> g, Decoder e )
    -> ( t, f -> g, Decoder f )
    -> (t -> String)
    -> Decoder g
variant6 variantDecoder ( t0, p0, d0 ) ( t1, p1, d1 ) ( t2, p2, d2 ) ( t3, p3, d3 ) ( t4, p4, d4 ) ( t5, p5, d5 ) unrecognized =
    variantDecoder.typeDecoder
        |> Json.Decode.andThen
            (\typ ->
                variantDecoder.valueDecoder
                    (if typ == t0 then
                        Json.Decode.map p0 d0

                     else if typ == t1 then
                        Json.Decode.map p1 d1

                     else if typ == t2 then
                        Json.Decode.map p2 d2

                     else if typ == t3 then
                        Json.Decode.map p3 d3

                     else if typ == t4 then
                        Json.Decode.map p4 d4

                     else if typ == t5 then
                        Json.Decode.map p5 d5

                     else
                        Json.Decode.fail (unrecognized typ)
                    )
            )
