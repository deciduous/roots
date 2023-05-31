module Roots.Json exposing
    ( Codec, toDecoder, toString, toValue
    , Decoder, Error, Value
    , bool, int, float, string, null
    , tuple2
    , list, array
    , PropertyCodec, property, optionalProperty
    , object0, object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
    , VariantCodec, objectVariantCodec
    , variant2, variant3, variant4
    )

{-| Json.

@docs Codec, toDecoder, toString, toValue
@docs Decoder, Error, Value
@docs bool, int, float, string, null
@docs tuple2
@docs list, array
@docs PropertyCodec, property, optionalProperty
@docs object0, object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
@docs VariantCodec, objectVariantCodec
@docs variant2, variant3, variant4

-}

import Array
import Json.Decode
import Json.Encode
import Roots exposing (..)
import Roots.Json.Decoder exposing (PropertyDecoder, VariantDecoder)
import Roots.List as List
import Roots.Prism as Prism exposing (Prism)


type alias Decoder a =
    Json.Decode.Decoder a


type alias Error =
    Json.Decode.Error


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


bool : Codec Bool
bool =
    Codec
        { decoder = Json.Decode.bool
        , encoder = Json.Encode.bool
        }


int : Codec Int
int =
    Codec
        { decoder = Json.Decode.int
        , encoder = Json.Encode.int
        }


float : Codec Float
float =
    Codec
        { decoder = Json.Decode.float
        , encoder = Json.Encode.float
        }


string : Codec String
string =
    Codec
        { decoder = Json.Decode.string
        , encoder = Json.Encode.string
        }


null : Codec ()
null =
    Codec
        { decoder = Roots.Json.Decoder.null
        , encoder = \_ -> Json.Encode.null
        }


tuple2 : Codec a -> Codec b -> Codec ( a, b )
tuple2 (Codec ca) (Codec cb) =
    Codec
        { decoder = Roots.Json.Decoder.tuple2 ca.decoder cb.decoder
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


list : Codec a -> Codec (List a)
list (Codec codec) =
    Codec
        { decoder = Json.Decode.list codec.decoder
        , encoder = Json.Encode.list codec.encoder
        }


array : Codec a -> Codec (Array a)
array (Codec codec) =
    Codec
        { decoder = Json.Decode.array codec.decoder
        , encoder = Json.Encode.array codec.encoder
        }


type alias PropertyCodec a =
    { decoder : PropertyDecoder a
    , encoder : a -> Maybe ( String, Value )
    }


property : String -> Codec a -> PropertyCodec a
property key (Codec codec) =
    { decoder = Roots.Json.Decoder.property key codec.decoder
    , encoder = \x -> Just ( key, codec.encoder x )
    }


optionalProperty : String -> Codec a -> PropertyCodec (Maybe a)
optionalProperty key (Codec codec) =
    { decoder = Roots.Json.Decoder.optionalProperty key codec.decoder
    , encoder = Maybe.map (\x -> ( key, codec.encoder x ))
    }


object0 : Codec ()
object0 =
    Codec
        { decoder = Json.Decode.succeed ()
        , encoder = \_ -> Json.Encode.object []
        }


object1 : (a -> b) -> (b -> a) -> PropertyCodec a -> Codec b
object1 f0 f1 pa =
    Codec
        { decoder = Roots.Json.Decoder.object1 f0 pa.decoder
        , encoder = \b -> Json.Encode.object (List.catMaybes [ pa.encoder (f1 b) ])
        }


object2 : (a -> b -> c) -> (c -> ( a, b )) -> PropertyCodec a -> PropertyCodec b -> Codec c
object2 f0 f1 pa pb =
    Codec
        { decoder = Roots.Json.Decoder.object2 f0 pa.decoder pb.decoder
        , encoder =
            \c ->
                let
                    ( a, b ) =
                        f1 c
                in
                Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b ])
        }


object3 :
    (a -> b -> c -> d)
    -> (d -> ( a, b, c ))
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> Codec d
object3 f0 f1 pa pb pc =
    Codec
        { decoder = Roots.Json.Decoder.object3 f0 pa.decoder pb.decoder pc.decoder
        , encoder =
            \d ->
                let
                    ( a, b, c ) =
                        f1 d
                in
                Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b, pc.encoder c ])
        }


object4 :
    (a -> b -> c -> d -> e)
    -> (e -> T4 a b c d)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> Codec e
object4 f0 f1 pa pb pc pd =
    Codec
        { decoder = Roots.Json.Decoder.object4 f0 pa.decoder pb.decoder pc.decoder pd.decoder
        , encoder =
            \e ->
                case f1 e of
                    T4 a b c d ->
                        Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b, pc.encoder c, pd.encoder d ])
        }


object5 :
    (a -> b -> c -> d -> e -> f)
    -> (f -> T5 a b c d e)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> Codec f
object5 f0 f1 pa pb pc pd pe =
    Codec
        { decoder = Roots.Json.Decoder.object5 f0 pa.decoder pb.decoder pc.decoder pd.decoder pe.decoder
        , encoder =
            \f ->
                case f1 f of
                    T5 a b c d e ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                ]
                            )
        }


object6 :
    (a -> b -> c -> d -> e -> f -> g)
    -> (g -> T6 a b c d e f)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> Codec g
object6 f0 f1 pa pb pc pd pe pf =
    Codec
        { decoder = Roots.Json.Decoder.object6 f0 pa.decoder pb.decoder pc.decoder pd.decoder pe.decoder pf.decoder
        , encoder =
            \g ->
                case f1 g of
                    T6 a b c d e f ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                ]
                            )
        }


object7 :
    (a -> b -> c -> d -> e -> f -> g -> h)
    -> (h -> T7 a b c d e f g)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> Codec h
object7 f0 f1 pa pb pc pd pe pf pg =
    Codec
        { decoder = Roots.Json.Decoder.object7 f0 pa.decoder pb.decoder pc.decoder pd.decoder pe.decoder pf.decoder pg.decoder
        , encoder =
            \h ->
                case f1 h of
                    T7 a b c d e f g ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                , pg.encoder g
                                ]
                            )
        }


object8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i)
    -> (i -> T8 a b c d e f g h)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> PropertyCodec h
    -> Codec i
object8 f0 f1 pa pb pc pd pe pf pg ph =
    Codec
        { decoder =
            Roots.Json.Decoder.object8 f0
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
                pg.decoder
                ph.decoder
        , encoder =
            \i ->
                case f1 i of
                    T8 a b c d e f g h ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                , pg.encoder g
                                , ph.encoder h
                                ]
                            )
        }


object9 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j)
    -> (j -> T9 a b c d e f g h i)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> PropertyCodec h
    -> PropertyCodec i
    -> Codec j
object9 f0 f1 pa pb pc pd pe pf pg ph pi =
    Codec
        { decoder =
            Roots.Json.Decoder.object9
                f0
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
                pg.decoder
                ph.decoder
                pi.decoder
        , encoder =
            \j ->
                case f1 j of
                    T9 a b c d e f g h i ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                , pg.encoder g
                                , ph.encoder h
                                , pi.encoder i
                                ]
                            )
        }


object10 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k)
    -> (k -> T10 a b c d e f g h i j)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> PropertyCodec h
    -> PropertyCodec i
    -> PropertyCodec j
    -> Codec k
object10 f0 f1 pa pb pc pd pe pf pg ph pi pj =
    Codec
        { decoder =
            Roots.Json.Decoder.object10
                f0
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
                pg.decoder
                ph.decoder
                pi.decoder
                pj.decoder
        , encoder =
            \k ->
                case f1 k of
                    T10 a b c d e f g h i j ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                , pg.encoder g
                                , ph.encoder h
                                , pi.encoder i
                                , pj.encoder j
                                ]
                            )
        }


object11 :
    (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l)
    -> (l -> T11 a b c d e f g h i j k)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> PropertyCodec h
    -> PropertyCodec i
    -> PropertyCodec j
    -> PropertyCodec k
    -> Codec l
object11 f0 f1 pa pb pc pd pe pf pg ph pi pj pk =
    Codec
        { decoder =
            Roots.Json.Decoder.object11
                f0
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
                pg.decoder
                ph.decoder
                pi.decoder
                pj.decoder
                pk.decoder
        , encoder =
            \l ->
                case f1 l of
                    T11 a b c d e f g h i j k ->
                        Json.Encode.object
                            (List.catMaybes
                                [ pa.encoder a
                                , pb.encoder b
                                , pc.encoder c
                                , pd.encoder d
                                , pe.encoder e
                                , pf.encoder f
                                , pg.encoder g
                                , ph.encoder h
                                , pi.encoder i
                                , pj.encoder j
                                , pk.encoder k
                                ]
                            )
        }


type alias VariantCodec t a =
    { decoder : VariantDecoder t a
    , encoder : t -> Value -> Value
    }


{-| A two-key object codec.
-}
objectVariantCodec : String -> String -> Codec t -> VariantCodec t a
objectVariantCodec typeKey valueKey (Codec typeCodec) =
    { decoder = Roots.Json.Decoder.objectVariantDecoder typeKey valueKey typeCodec.decoder
    , encoder =
        \typ value ->
            Json.Encode.object
                [ ( typeKey, typeCodec.encoder typ )
                , ( valueKey, value )
                ]
    }


variant2 :
    VariantCodec t c
    -> ( t, Prism c a, Codec a )
    -> ( t, Prism c b, Codec b )
    -> (t -> String)
    -> Codec c
variant2 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) unrecognized =
    Codec
        { decoder =
            Roots.Json.Decoder.variant2 variantCodec.decoder
                ( t0, Prism.review p0, c0.decoder )
                ( t1, Prism.review p1, c1.decoder )
                unrecognized
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
                                Json.Encode.null
        }


variant3 :
    VariantCodec t d
    -> ( t, Prism d a, Codec a )
    -> ( t, Prism d b, Codec b )
    -> ( t, Prism d c, Codec c )
    -> (t -> String)
    -> Codec d
variant3 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) unrecognized =
    Codec
        { decoder =
            Roots.Json.Decoder.variant3 variantCodec.decoder
                ( t0, Prism.review p0, c0.decoder )
                ( t1, Prism.review p1, c1.decoder )
                ( t2, Prism.review p2, c2.decoder )
                unrecognized
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
                                        Json.Encode.null
        }


variant4 :
    VariantCodec t e
    -> ( t, Prism e a, Codec a )
    -> ( t, Prism e b, Codec b )
    -> ( t, Prism e c, Codec c )
    -> ( t, Prism e d, Codec d )
    -> (t -> String)
    -> Codec e
variant4 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) ( t3, p3, Codec c3 ) unrecognized =
    Codec
        { decoder =
            Roots.Json.Decoder.variant4 variantCodec.decoder
                ( t0, Prism.review p0, c0.decoder )
                ( t1, Prism.review p1, c1.decoder )
                ( t2, Prism.review p2, c2.decoder )
                ( t3, Prism.review p3, c3.decoder )
                unrecognized
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
                                        case Prism.preview p3 value of
                                            Just inner ->
                                                variantCodec.encoder t3 (c3.encoder inner)

                                            Nothing ->
                                                Json.Encode.null
        }
