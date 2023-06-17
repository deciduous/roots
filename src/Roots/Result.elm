module Roots.Result exposing (errPrism, okPrism)

{-|

@docs errPrism, okPrism

-}

import Roots.Prism as Prism exposing (Prism)


errPrism : Prism (Result a x) (Result b x) a b
errPrism =
    Prism.prism
        (\ax ->
            case ax of
                Err a ->
                    Ok a

                Ok x ->
                    Err (Ok x)
        )
        Err


okPrism : Prism (Result x a) (Result x b) a b
okPrism =
    Prism.prism
        (\xa ->
            case xa of
                Err x ->
                    Err (Err x)

                Ok a ->
                    Ok a
        )
        Ok
