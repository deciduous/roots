module Roots exposing
    ( Array
    , ArrayZipper
    , Dict
    , Prism
    , Set
    , T10(..)
    , T11(..)
    , T4(..)
    , T5(..)
    , T6(..)
    , T7(..)
    , T8(..)
    , T9(..)
    , ifte
    , isEven
    , isOdd
    )

{-| Roots.

@docs Array ArrayZipper Dict Prism Set T4 T5 T6 T7 T8 T9 T10 T11
@docs ifte isEven isOdd

-}

import Array
import Dict
import Roots.ArrayZipper as ArrayZipper
import Roots.Prism as Prism
import Set


type alias Array a =
    Array.Array a


type alias ArrayZipper a =
    ArrayZipper.ArrayZipper a


type alias Dict k v =
    Dict.Dict k v


type alias Prism s a =
    Prism.Prism s a


type alias Set a =
    Set.Set a


type T4 a b c d
    = T4 a b c d


type T5 a b c d e
    = T5 a b c d e


type T6 a b c d e f
    = T6 a b c d e f


type T7 a b c d e f g
    = T7 a b c d e f g


type T8 a b c d e f g h
    = T8 a b c d e f g h


type T9 a b c d e f g h i
    = T9 a b c d e f g h i


type T10 a b c d e f g h i j
    = T10 a b c d e f g h i j


type T11 a b c d e f g h i j k
    = T11 a b c d e f g h i j k


ifte : Bool -> a -> a -> a
ifte p t f =
    if p then
        t

    else
        f


isEven : Int -> Bool
isEven n =
    modBy 2 n == 0


isOdd : Int -> Bool
isOdd n =
    modBy 2 n == 1
