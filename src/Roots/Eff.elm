module Roots.Eff exposing
    ( Eff, pure
    , command, commandIf, commandWhen
    , attempt
    , map
    , andThen, andThenIf
    , toTuple
    )

{-| Eff.

@docs Eff, pure
@docs command, commandIf, commandWhen
@docs attempt
@docs map
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
command cmd (Eff.Eff eff) =
    Eff.Eff { eff | commands = cmd :: eff.commands }


{-| Conditionally add a command to an effectful value.
-}
commandIf : Bool -> Cmd e -> Eff e a -> Eff e a
commandIf b cmd eff =
    if b then
        command cmd eff

    else
        eff


{-| Conditionally add a command to an effectful value.
-}
commandWhen : Maybe a -> (a -> Cmd e) -> Eff e a -> Eff e a
commandWhen mx cmd eff =
    case mx of
        Nothing ->
            eff

        Just x ->
            command (cmd x) eff


{-| Add a task to an effectful value.
-}
attempt : (Result r s -> e) -> Task r s -> Eff e a -> Eff e a
attempt f task =
    command (Task.attempt f task)


{-| Map a function over an effectful value.
-}
map : (a -> b) -> Eff e a -> Eff e b
map f (Eff.Eff { model, commands }) =
    Eff.Eff { model = f model, commands = commands }


{-| Update an effectful value.
-}
andThen : Update e a -> Eff e a -> Eff e a
andThen u e =
    List.foldl andThen0 e u


andThen0 : (a -> Eff e b) -> Eff e a -> Eff e b
andThen0 u (Eff.Eff e0) =
    case u e0.model of
        Eff.Eff e1 ->
            Eff.Eff { e1 | commands = e1.commands ++ e0.commands }


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
