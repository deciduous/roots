module Roots.Json exposing
    ( Codec
    , Decoder
    , Value
    , array
    , encode
    , int
    , list
    , object2
    , object8
    , string
    )

import Json.Decode
import Json.Encode
import Roots exposing (Array, T8(..))


type alias Decoder a =
    Json.Decode.Decoder a


type alias Value =
    Json.Encode.Value


encode : Value -> String
encode =
    Json.Encode.encode 0


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
