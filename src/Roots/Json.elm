module Roots.Json exposing
    ( Codec
    , Decoder
    , Value
    , VariantCodec
    , array
    , int
    , list
    , object0
    , object2
    , object4
    , object8
    , objectVariantCodec
    , string
    , toDecoder
    , toString
    , toValue
    , tuple2
    , variant3
    )

import Array
import Json.Decode
import Json.Encode
import Roots exposing (..)
import Roots.Prism as Prism exposing (Prism)


type alias Decoder a =
    Json.Decode.Decoder a


type alias Value =
    Json.Encode.Value


toDecoder : Codec a -> Decoder a
toDecoder (Codec codec) =
    codec.decoder


toString : Codec a -> a -> String
toString (Codec codec) val =
    Json.Encode.encode 0 (codec.encoder val)


toValue : Codec a -> a -> Value
toValue (Codec codec) =
    codec.encoder


type Codec a
    = Codec
        { decoder : Decoder a
        , encoder : a -> Value
        }


int : Codec Int
int =
    Codec
        { decoder = Json.Decode.int
        , encoder = Json.Encode.int
        }


string : Codec String
string =
    Codec
        { decoder = Json.Decode.string
        , encoder = Json.Encode.string
        }


array : Codec a -> Codec (Array a)
array (Codec codec) =
    Codec
        { decoder = Json.Decode.array codec.decoder
        , encoder = Json.Encode.array codec.encoder
        }


list : Codec a -> Codec (List a)
list (Codec codec) =
    Codec
        { decoder = Json.Decode.list codec.decoder
        , encoder = Json.Encode.list codec.encoder
        }


tuple2 : Codec a -> Codec b -> Codec ( a, b )
tuple2 (Codec ca) (Codec cb) =
    Codec
        { decoder =
            Json.Decode.map2 (\a b -> ( a, b ))
                (Json.Decode.index 0 ca.decoder)
                (Json.Decode.index 1 cb.decoder)
        , encoder =
            \( a, b ) ->
                Json.Encode.array
                    identity
                    (Array.initialize 2
                        (\i ->
                            if i == 0 then
                                ca.encoder a

                            else
                                cb.encoder b
                        )
                    )
        }


object0 : Codec ()
object0 =
    Codec
        { decoder = Json.Decode.succeed ()
        , encoder = \_ -> Json.Encode.object []
        }


object2 : (a -> b -> c) -> (c -> ( a, b )) -> ( String, Codec a ) -> ( String, Codec b ) -> Codec c
object2 f0 f1 ( ka, Codec ca ) ( kb, Codec cb ) =
    Codec
        { decoder = Json.Decode.map2 f0 (Json.Decode.field ka ca.decoder) (Json.Decode.field kb cb.decoder)
        , encoder =
            \c ->
                let
                    ( a, b ) =
                        f1 c
                in
                Json.Encode.object [ ( ka, ca.encoder a ), ( kb, cb.encoder b ) ]
        }


object4 :
    (a -> b -> c -> d -> e)
    -> (e -> T4 a b c d)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> Codec e
object4 f0 f1 ( ka, Codec ca ) ( kb, Codec cb ) ( kc, Codec cc ) ( kd, Codec cd ) =
    Codec
        { decoder =
            Json.Decode.map4
                f0
                (Json.Decode.field ka ca.decoder)
                (Json.Decode.field kb cb.decoder)
                (Json.Decode.field kc cc.decoder)
                (Json.Decode.field kd cd.decoder)
        , encoder =
            \e ->
                case f1 e of
                    T4 a b c d ->
                        Json.Encode.object
                            [ ( ka, ca.encoder a )
                            , ( kb, cb.encoder b )
                            , ( kc, cc.encoder c )
                            , ( kd, cd.encoder d )
                            ]
        }


object8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> (i -> T8 a b c d e f g h)
    -> ( String, Codec a )
    -> ( String, Codec b )
    -> ( String, Codec c )
    -> ( String, Codec d )
    -> ( String, Codec e )
    -> ( String, Codec f )
    -> ( String, Codec g )
    -> ( String, Codec h )
    -> Codec i
object8 f0 f1 ( ka, Codec ca ) ( kb, Codec cb ) ( kc, Codec cc ) ( kd, Codec cd ) ( ke, Codec ce ) ( kf, Codec cf ) ( kg, Codec cg ) ( kh, Codec ch ) =
    Codec
        { decoder =
            Json.Decode.map8
                f0
                (Json.Decode.field ka ca.decoder)
                (Json.Decode.field kb cb.decoder)
                (Json.Decode.field kc cc.decoder)
                (Json.Decode.field kd cd.decoder)
                (Json.Decode.field ke ce.decoder)
                (Json.Decode.field kf cf.decoder)
                (Json.Decode.field kg cg.decoder)
                (Json.Decode.field kh ch.decoder)
        , encoder =
            \i ->
                case f1 i of
                    T8 a b c d e f g h ->
                        Json.Encode.object
                            [ ( ka, ca.encoder a )
                            , ( kb, cb.encoder b )
                            , ( kc, cc.encoder c )
                            , ( kd, cd.encoder d )
                            , ( ke, ce.encoder e )
                            , ( kf, cf.encoder f )
                            , ( kg, cg.encoder g )
                            , ( kh, ch.encoder h )
                            ]
        }


type alias VariantCodec t a =
    { typeCodec : Codec t
    , valueDecoder : Decoder a -> Decoder a
    , encoder : t -> Value -> Value
    }


{-| A two-key object codec.
-}
objectVariantCodec : String -> String -> Codec t -> VariantCodec t a
objectVariantCodec typeKey valueKey (Codec typeCodec) =
    { typeCodec = Codec typeCodec
    , valueDecoder = Json.Decode.field valueKey
    , encoder =
        \typ value ->
            Json.Encode.object
                [ ( typeKey, typeCodec.encoder typ )
                , ( valueKey, value )
                ]
    }


{-| An attempt at a nicer variant-encoding function than 'custom'.
-}
variant3 :
    VariantCodec t d
    -> ( t, Prism d a, Codec a )
    -> ( t, Prism d b, Codec b )
    -> ( t, Prism d c, Codec c )
    -> (t -> String)
    -> Codec d
variant3 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) oink =
    Codec
        { decoder =
            variantCodec.typeCodec
                |> toDecoder
                |> Json.Decode.andThen
                    (\typ ->
                        variantCodec.valueDecoder
                            (if typ == t0 then
                                Json.Decode.map (Prism.review p0) c0.decoder

                             else if typ == t1 then
                                Json.Decode.map (Prism.review p1) c1.decoder

                             else if typ == t2 then
                                Json.Decode.map (Prism.review p2) c2.decoder

                             else
                                Json.Decode.fail (oink typ)
                            )
                    )
        , encoder =
            \value ->
                case Prism.preview p0 value of
                    Just inner ->
                        variantCodec.encoder t0 (c0.encoder inner)

                    Nothing ->
                        case Prism.preview p1 value of
                            Just inner ->
                                variantCodec.encoder t1 (c1.encoder inner)

                            Nothing ->
                                case Prism.preview p2 value of
                                    Just inner ->
                                        variantCodec.encoder t2 (c2.encoder inner)

                                    Nothing ->
                                        -- This case is impossible for correctly written client code, for which
                                        -- we'll always have *some* constructor comparison return Just. But if the
                                        -- client erroneously doesn't tell us how to encode a value with the given
                                        -- tag, I guess just give back a null.
                                        Json.Encode.null
        }
