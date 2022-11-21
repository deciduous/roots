module Roots.ArrayZipper exposing
    ( ArrayZipper
    , empty
    , focus
    , focusOn
    , fromArray
    , fromList
    , index
    , toArray
    , toList
    )

import Array exposing (Array)


type ArrayZipper a
    = ArrayZipper
        { array : Array a
        , index_ : Int
        }


empty : ArrayZipper a
empty =
    ArrayZipper
        { array = Array.empty
        , index_ = 0
        }


index : ArrayZipper a -> Int
index (ArrayZipper { index_ }) =
    index_


focus : ArrayZipper a -> Maybe a
focus (ArrayZipper { array, index_ }) =
    Array.get index_ array


focusOn : Int -> ArrayZipper a -> ArrayZipper a
focusOn index_ (ArrayZipper zipper) =
    ArrayZipper { zipper | index_ = index_ }


fromArray : Array a -> ArrayZipper a
fromArray array =
    ArrayZipper { array = array, index_ = 0 }


fromList : List a -> ArrayZipper a
fromList list =
    ArrayZipper { array = Array.fromList list, index_ = 0 }


toArray : ArrayZipper a -> Array a
toArray (ArrayZipper { array }) =
    array


toList : ArrayZipper a -> List a
toList (ArrayZipper { array }) =
    Array.toList array
