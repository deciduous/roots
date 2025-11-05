module Roots.Eff2 exposing
    ( Eff, wrap, unwrap
    , command, commandIf, commandWhen
    , attempt
    , map, then_, thenIf
    )

{-| Eff.

@docs Eff, wrap, unwrap
@docs command, commandIf, commandWhen
@docs attempt
@docs map, then_, thenIf

-}

import Task exposing (Task)


type alias Eff e a =
    ( a, Cmd e )


{-| Make an effectful value from a value.
-}
wrap : a -> Eff e a
wrap x =
    ( x, Cmd.none )


{-| Extract the value from an effectful value.
-}
unwrap : Eff e a -> a
unwrap ( x, _ ) =
    x


{-| Add a command to an effectful value.
-}
command : Cmd e -> Eff e a -> Eff e a
command e2 ( x, e1 ) =
    ( x, Cmd.batch [ e1, e2 ] )


{-| Conditionally add a command to an effectful value.
-}
commandIf : Bool -> Cmd e -> Eff e a -> Eff e a
commandIf b e x =
    if b then
        command e x

    else
        x


{-| Conditionally add a command to an effectful value.
-}
commandWhen : Maybe a -> (a -> Cmd e) -> Eff e b -> Eff e b
commandWhen mx f y =
    case mx of
        Nothing ->
            y

        Just x ->
            command (f x) y


{-| Add a task to an effectful value.
-}
attempt : (Result r s -> e) -> Task r s -> Eff e a -> Eff e a
attempt f task =
    command (Task.attempt f task)


{-| Map a function over an effectful value.
-}
map : (a -> b) -> Eff e a -> Eff e b
map f ( x, e ) =
    ( f x, e )


{-| Map an effectful function over an effectful value.
-}
then_ : (a -> Eff e b) -> Eff e a -> Eff e b
then_ f ( x1, e1 ) =
    let
        ( x2, e2 ) =
            f x1
    in
    ( x2, Cmd.batch [ e1, e2 ] )


{-| Conditionally map an effectful function over an effectful value.
-}
thenIf : Bool -> (a -> Eff e a) -> Eff e a -> Eff e a
thenIf b f x =
    if b then
        then_ f x

    else
        x
