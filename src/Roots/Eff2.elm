module Roots.Eff2 exposing
    ( Eff, wrap, unwrap
    , command, command_, commandIf, commandIf_, commandWhen, commandWhen_
    , attempt, attempt_
    , map, also, alsoIf
    )

{-| Eff.

@docs Eff, wrap, unwrap
@docs command, command_, commandIf, commandIf_, commandWhen, commandWhen_
@docs attempt, attempt_
@docs map, also, alsoIf

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
command : (a -> Cmd e) -> Eff e a -> Eff e a
command f ( x, e1 ) =
    ( x, Cmd.batch [ e1, f x ] )


{-| Add a command to an effectful value.
-}
command_ : Cmd e -> Eff e a -> Eff e a
command_ e2 ( x, e1 ) =
    ( x, Cmd.batch [ e1, e2 ] )


{-| Conditionally add a command to an effectful value.
-}
commandIf : (a -> Bool) -> Cmd e -> Eff e a -> Eff e a
commandIf f e2 ( x, e1 ) =
    if f x then
        ( x, Cmd.batch [ e1, e2 ] )

    else
        ( x, e1 )


{-| Conditionally add a command to an effectful value.
-}
commandIf_ : Bool -> Cmd e -> Eff e a -> Eff e a
commandIf_ b e x =
    if b then
        command_ e x

    else
        x


{-| Conditionally add a command to an effectful value.
-}
commandWhen : (a -> Maybe b) -> (b -> Cmd e) -> Eff e a -> Eff e a
commandWhen f g ( x, e ) =
    case f x of
        Just b ->
            ( x, Cmd.batch [ g b, e ] )

        Nothing ->
            ( x, e )


{-| Conditionally add a command to an effectful value.
-}
commandWhen_ : Maybe a -> (a -> Cmd e) -> Eff e b -> Eff e b
commandWhen_ mx f y =
    case mx of
        Just x ->
            command_ (f x) y

        Nothing ->
            y


{-| Add a task to an effectful value.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> Eff e a -> Eff e a
attempt f g ( x, e ) =
    ( x, Task.attempt f (g x) )


{-| Add a task to an effectful value.
-}
attempt_ : (Result r s -> e) -> Task r s -> Eff e a -> Eff e a
attempt_ f task =
    command_ (Task.attempt f task)


{-| Map a function over an effectful value.
-}
map : (a -> b) -> Eff e a -> Eff e b
map f ( x, e ) =
    ( f x, e )


{-| Map an effectful function over an effectful value.
-}
also : (a -> Eff e b) -> Eff e a -> Eff e b
also f ( x1, e1 ) =
    let
        ( x2, e2 ) =
            f x1
    in
    ( x2, Cmd.batch [ e1, e2 ] )


{-| Conditionally map an effectful function over an effectful value.
-}
alsoIf : Bool -> (a -> Eff e a) -> Eff e a -> Eff e a
alsoIf b f x =
    if b then
        also f x

    else
        x
