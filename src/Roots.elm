module Roots exposing
    ( Array, ArrayZipper
    , Dict
    , Set
    , T4(..), T5(..), T6(..), T7(..), T8(..), T9(..), T10(..), T11(..)
    , Iso, Iso_
    , Prism, Prism_
    , ifte, isEven, isOdd
    )

{-| Roots.

@docs Array, ArrayZipper
@docs Dict
@docs Set
@docs T4, T5, T6, T7, T8, T9, T10, T11
@docs Iso, Iso_
@docs Prism, Prism_
@docs ifte, isEven, isOdd

-}

import Array
import Dict
import Roots.ArrayZipper as ArrayZipper
import Roots.Iso as Iso
import Roots.Prism as Prism
import Set


type alias Array a =
    Array.Array a


type alias ArrayZipper a =
    ArrayZipper.ArrayZipper a


type alias Dict k v =
    Dict.Dict k v


type alias Iso s t a b =
    Iso.Iso s t a b


type alias Iso_ s a =
    Iso.Iso_ s a


type alias Prism s t a b =
    Prism.Prism s t a b


type alias Prism_ s a =
    Prism.Prism_ s a


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
