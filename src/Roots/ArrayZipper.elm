module Roots.ArrayZipper exposing
    ( ArrayZipper
    , cycleLeft
    , cycleRight
    , empty
    , focus
    , focusOn
    , fromArray
    , fromList
    , index
    , length
    , lengthLeft
    , lengthRight
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


length : ArrayZipper a -> Int
length (ArrayZipper { array }) =
    Array.length array


lengthLeft : ArrayZipper a -> Int
lengthLeft =
    index


lengthRight : ArrayZipper a -> Int
lengthRight (ArrayZipper { array, index_ }) =
    max (Array.length array - index_ - 1) 0


cycleLeft : ArrayZipper a -> ArrayZipper a
cycleLeft (ArrayZipper zipper) =
    if zipper.index_ == 0 then
        if Array.length zipper.array == 0 then
            ArrayZipper zipper

        else
            ArrayZipper { zipper | index_ = Array.length zipper.array - 1 }

    else
        ArrayZipper { zipper | index_ = zipper.index_ - 1 }


cycleRight : ArrayZipper a -> ArrayZipper a
cycleRight (ArrayZipper { array, index_ }) =
    let
        i =
            index_ + 1
    in
    if i < Array.length array then
        ArrayZipper { array = array, index_ = i }

    else
        ArrayZipper { array = array, index_ = 0 }


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
