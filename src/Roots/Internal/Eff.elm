module Roots.Internal.Eff exposing (..)

type Eff e a
    = Eff
        { model : a
        , commands : List (Cmd e)
        }
