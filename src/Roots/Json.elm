module Roots.Json exposing
    ( Codec, toDecoder, toString, toValue
    , iso
    , Decoder, Error, Value
    , bool, int, float, string, null
    , tuple2
    , list, array
    , PropertyCodec, property, optionalProperty
    , object0, object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
    , enum
    , VariantCodec, arrayVariantCodec, objectVariantCodec
    , variant2, variant3, variant4, variant5, variant6
    )

{-| Json.

@docs Codec, toDecoder, toString, toValue
@docs iso
@docs Decoder, Error, Value
@docs bool, int, float, string, null
@docs tuple2
@docs list, array
@docs PropertyCodec, property, optionalProperty
@docs object0, object1, object2, object3, object4, object5, object6, object7, object8, object9, object10, object11
@docs enum
@docs VariantCodec, arrayVariantCodec, objectVariantCodec
@docs variant2, variant3, variant4, variant5, variant6

-}

import Array
import Json.Decode
import Json.Encode
import Roots exposing (..)
import Roots.Array
import Roots.Iso as Iso exposing (Iso_)
import Roots.Json.Decoder exposing (PropertyDecoder)
import Roots.List as List
import Roots.Prism as Prism exposing (Prism_)


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


iso : Iso_ a b -> Codec b -> Codec a
iso iso_ (Codec { decoder, encoder }) =
    Codec
        { decoder = Json.Decode.map (Iso.review iso_) decoder
        , encoder = \value -> encoder (Iso.view iso_ value)
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


object1 : Iso_ b a -> PropertyCodec a -> Codec b
object1 iso_ pa =
    Codec
        { decoder = Roots.Json.Decoder.object1 (Iso.review iso_) pa.decoder
        , encoder = \b -> Json.Encode.object (List.catMaybes [ pa.encoder (Iso.view iso_ b) ])
        }


object2 : Iso_ c ( a, b ) -> PropertyCodec a -> PropertyCodec b -> Codec c
object2 iso_ pa pb =
    Codec
        { decoder = Roots.Json.Decoder.object2 (\a b -> Iso.review iso_ ( a, b )) pa.decoder pb.decoder
        , encoder =
            \c ->
                let
                    ( a, b ) =
                        Iso.view iso_ c
                in
                Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b ])
        }


object3 :
    Iso_ d ( a, b, c )
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> Codec d
object3 iso_ pa pb pc =
    Codec
        { decoder = Roots.Json.Decoder.object3 (\a b c -> Iso.review iso_ ( a, b, c )) pa.decoder pb.decoder pc.decoder
        , encoder =
            \d ->
                let
                    ( a, b, c ) =
                        Iso.view iso_ d
                in
                Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b, pc.encoder c ])
        }


object4 :
    Iso_ e (T4 a b c d)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> Codec e
object4 iso_ pa pb pc pd =
    Codec
        { decoder =
            Roots.Json.Decoder.object4
                (\a b c d -> Iso.review iso_ (T4 a b c d))
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
        , encoder =
            \e ->
                case Iso.view iso_ e of
                    T4 a b c d ->
                        Json.Encode.object (List.catMaybes [ pa.encoder a, pb.encoder b, pc.encoder c, pd.encoder d ])
        }


object5 :
    Iso_ f (T5 a b c d e)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> Codec f
object5 iso_ pa pb pc pd pe =
    Codec
        { decoder =
            Roots.Json.Decoder.object5
                (\a b c d e -> Iso.review iso_ (T5 a b c d e))
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
        , encoder =
            \f ->
                case Iso.view iso_ f of
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
    Iso_ g (T6 a b c d e f)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> Codec g
object6 iso_ pa pb pc pd pe pf =
    Codec
        { decoder =
            Roots.Json.Decoder.object6
                (\a b c d e f -> Iso.review iso_ (T6 a b c d e f))
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
        , encoder =
            \g ->
                case Iso.view iso_ g of
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
    Iso_ h (T7 a b c d e f g)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> Codec h
object7 iso_ pa pb pc pd pe pf pg =
    Codec
        { decoder =
            Roots.Json.Decoder.object7
                (\a b c d e f g -> Iso.review iso_ (T7 a b c d e f g))
                pa.decoder
                pb.decoder
                pc.decoder
                pd.decoder
                pe.decoder
                pf.decoder
                pg.decoder
        , encoder =
            \h ->
                case Iso.view iso_ h of
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
    Iso_ i (T8 a b c d e f g h)
    -> PropertyCodec a
    -> PropertyCodec b
    -> PropertyCodec c
    -> PropertyCodec d
    -> PropertyCodec e
    -> PropertyCodec f
    -> PropertyCodec g
    -> PropertyCodec h
    -> Codec i
object8 iso_ pa pb pc pd pe pf pg ph =
    Codec
        { decoder =
            Roots.Json.Decoder.object8
                (\a b c d e f g h -> Iso.review iso_ (T8 a b c d e f g h))
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
                case Iso.view iso_ i of
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
    Iso_ j (T9 a b c d e f g h i)
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
object9 iso_ pa pb pc pd pe pf pg ph pi =
    Codec
        { decoder =
            Roots.Json.Decoder.object9
                (\a b c d e f g h i -> Iso.review iso_ (T9 a b c d e f g h i))
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
                case Iso.view iso_ j of
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
    Iso_ k (T10 a b c d e f g h i j)
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
object10 iso_ pa pb pc pd pe pf pg ph pi pj =
    Codec
        { decoder =
            Roots.Json.Decoder.object10
                (\a b c d e f g h i j -> Iso.review iso_ (T10 a b c d e f g h i j))
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
                case Iso.view iso_ k of
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
    Iso_ l (T11 a b c d e f g h i j k)
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
object11 iso_ pa pb pc pd pe pf pg ph pi pj pk =
    Codec
        { decoder =
            Roots.Json.Decoder.object11
                (\a b c d e f g h i j k -> Iso.review iso_ (T11 a b c d e f g h i j k))
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
                case Iso.view iso_ l of
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


{-| An enum codec.
-}
enum : Codec t -> List ( t, a ) -> Codec a
enum (Codec codec) values =
    Codec
        { decoder = Roots.Json.Decoder.enum codec.decoder values
        , encoder =
            let
                valuesArray =
                    Array.fromList values
            in
            \value ->
                case
                    Roots.Array.findFirst
                        (\( typ, val ) ->
                            if val == value then
                                Just typ

                            else
                                Nothing
                        )
                        valuesArray
                of
                    Just typ ->
                        codec.encoder typ

                    Nothing ->
                        Json.Encode.null
        }


type alias VariantCodec t a =
    { decoder : List ( t, Decoder a ) -> Decoder a
    , encoder : t -> Value -> Value
    }


{-| A two-element array variant codec.
-}
arrayVariantCodec : Codec t -> VariantCodec t a
arrayVariantCodec (Codec typeCodec) =
    { decoder = Roots.Json.Decoder.arrayVariant typeCodec.decoder
    , encoder =
        \typ value ->
            Json.Encode.list identity [ typeCodec.encoder typ, value ]
    }


{-| A two-key object variant codec.
-}
objectVariantCodec : String -> Codec t -> String -> VariantCodec t a
objectVariantCodec typeKey (Codec typeCodec) valueKey =
    { decoder = Roots.Json.Decoder.objectVariant typeKey typeCodec.decoder valueKey
    , encoder =
        \typ value ->
            Json.Encode.object
                [ ( typeKey, typeCodec.encoder typ )
                , ( valueKey, value )
                ]
    }


variant2 :
    VariantCodec t c
    -> ( t, Prism_ c a, Codec a )
    -> ( t, Prism_ c b, Codec b )
    -> Codec c
variant2 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) =
    Codec
        { decoder =
            variantCodec.decoder
                [ ( t0, Json.Decode.map (Prism.review p0) c0.decoder )
                , ( t1, Json.Decode.map (Prism.review p1) c1.decoder )
                ]
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
    -> ( t, Prism_ d a, Codec a )
    -> ( t, Prism_ d b, Codec b )
    -> ( t, Prism_ d c, Codec c )
    -> Codec d
variant3 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) =
    Codec
        { decoder =
            variantCodec.decoder
                [ ( t0, Json.Decode.map (Prism.review p0) c0.decoder )
                , ( t1, Json.Decode.map (Prism.review p1) c1.decoder )
                , ( t2, Json.Decode.map (Prism.review p2) c2.decoder )
                ]
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
    -> ( t, Prism_ e a, Codec a )
    -> ( t, Prism_ e b, Codec b )
    -> ( t, Prism_ e c, Codec c )
    -> ( t, Prism_ e d, Codec d )
    -> Codec e
variant4 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) ( t3, p3, Codec c3 ) =
    Codec
        { decoder =
            variantCodec.decoder
                [ ( t0, Json.Decode.map (Prism.review p0) c0.decoder )
                , ( t1, Json.Decode.map (Prism.review p1) c1.decoder )
                , ( t2, Json.Decode.map (Prism.review p2) c2.decoder )
                , ( t3, Json.Decode.map (Prism.review p3) c3.decoder )
                ]
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


variant5 :
    VariantCodec t f
    -> ( t, Prism_ f a, Codec a )
    -> ( t, Prism_ f b, Codec b )
    -> ( t, Prism_ f c, Codec c )
    -> ( t, Prism_ f d, Codec d )
    -> ( t, Prism_ f e, Codec e )
    -> Codec f
variant5 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) ( t3, p3, Codec c3 ) ( t4, p4, Codec c4 ) =
    Codec
        { decoder =
            variantCodec.decoder
                [ ( t0, Json.Decode.map (Prism.review p0) c0.decoder )
                , ( t1, Json.Decode.map (Prism.review p1) c1.decoder )
                , ( t2, Json.Decode.map (Prism.review p2) c2.decoder )
                , ( t3, Json.Decode.map (Prism.review p3) c3.decoder )
                , ( t4, Json.Decode.map (Prism.review p4) c4.decoder )
                ]
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
                                                case Prism.preview p4 value of
                                                    Just inner ->
                                                        variantCodec.encoder t4 (c4.encoder inner)

                                                    Nothing ->
                                                        Json.Encode.null
        }


variant6 :
    VariantCodec t g
    -> ( t, Prism_ g a, Codec a )
    -> ( t, Prism_ g b, Codec b )
    -> ( t, Prism_ g c, Codec c )
    -> ( t, Prism_ g d, Codec d )
    -> ( t, Prism_ g e, Codec e )
    -> ( t, Prism_ g f, Codec f )
    -> Codec g
variant6 variantCodec ( t0, p0, Codec c0 ) ( t1, p1, Codec c1 ) ( t2, p2, Codec c2 ) ( t3, p3, Codec c3 ) ( t4, p4, Codec c4 ) ( t5, p5, Codec c5 ) =
    Codec
        { decoder =
            variantCodec.decoder
                [ ( t0, Json.Decode.map (Prism.review p0) c0.decoder )
                , ( t1, Json.Decode.map (Prism.review p1) c1.decoder )
                , ( t2, Json.Decode.map (Prism.review p2) c2.decoder )
                , ( t3, Json.Decode.map (Prism.review p3) c3.decoder )
                , ( t4, Json.Decode.map (Prism.review p4) c4.decoder )
                , ( t5, Json.Decode.map (Prism.review p5) c5.decoder )
                ]
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
                                                case Prism.preview p4 value of
                                                    Just inner ->
                                                        variantCodec.encoder t4 (c4.encoder inner)

                                                    Nothing ->
                                                        case Prism.preview p5 value of
                                                            Just inner ->
                                                                variantCodec.encoder t5 (c5.encoder inner)

                                                            Nothing ->
                                                                Json.Encode.null
        }
