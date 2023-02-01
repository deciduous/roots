module Roots.Internal.Update exposing (..)

import Roots.Internal.Eff exposing (Eff)


type alias Update e a =
    List (a -> Eff e a)
