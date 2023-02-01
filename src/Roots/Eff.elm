module Roots.Eff exposing
    ( Eff, pure
    , command, commandIf, commandWhen
    , attempt
    , andThen, andThenIf
    , toTuple
    )

{-|

@docs Eff, pure
@docs command, commandIf, commandWhen
@docs attempt
@docs andThen, andThenIf
@docs toTuple

-}

import Roots.Internal.Eff as Eff
import Roots.Internal.Update exposing (Update)
import Task exposing (Task)


type alias Eff e a =
    Eff.Eff e a


pure : a -> Eff e a
pure model =
    Eff.Eff { model = model, commands = [] }


{-| Add a command to an effectful value.
-}
command : Cmd e -> Eff e a -> Eff e a
command c (Eff.Eff e) =
    Eff.Eff { e | commands = c :: e.commands }


{-| Conditionally add a command to an effectful value.
-}
commandIf : Bool -> Cmd e -> Eff e a -> Eff e a
commandIf b c e =
    if b then
        command c e

    else
        e


{-| Conditionally add a command to an effectful value.
-}
commandWhen : Maybe a -> (a -> Cmd e) -> Eff e a -> Eff e a
commandWhen mx c e =
    case mx of
        Nothing ->
            e

        Just x ->
            command (c x) e


{-| Add a task to an effectful value.
-}
attempt : (Result r s -> e) -> Task r s -> Eff e a -> Eff e a
attempt f task =
    command (Task.attempt f task)


{-| Update an effectful value.
-}
andThen : Update e a -> Eff e a -> Eff e a
andThen upd (Eff.Eff eff0) =
    case upd eff0.model of
        Eff.Eff eff1 ->
            Eff.Eff { eff1 | commands = eff0.commands ++ eff1.commands }


{-| Conditionally update an effectful value.
-}
andThenIf : Bool -> Update e a -> Eff e a -> Eff e a
andThenIf b upd eff =
    if b then
        andThen upd eff

    else
        eff


{-| FIXME export from Internal instead once roots defines its own application type.
-}
toTuple : Eff e a -> ( a, Cmd e )
toTuple (Eff.Eff eff) =
    ( eff.model, Cmd.batch eff.commands )
