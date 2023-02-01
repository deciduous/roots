module Roots.Update exposing
    ( Update, pure
    , command, commandIf, commandWhen, attempt
    , step, stepIf, stepWhen
    , mapModel
    , toUpdate
    )

{-|

@docs Update, pure
@docs command, commandIf, commandWhen, attempt
@docs step, stepIf, stepWhen
@docs mapModel
@docs toUpdate

-}

import Task exposing (Task)


type alias Update model event =
    { commands : List (Cmd event)
    , model : model
    }


{-| Create a pure update from a model.
-}
pure : model -> Update model event
pure m =
    { commands = []
    , model = m
    }


{-| Add a command to an update.
-}
command : Cmd event -> Update model event -> Update model event
command cmd update =
    { update | commands = cmd :: update.commands }


{-| Conditionally add a command to an update.
-}
commandIf : Bool -> Cmd event -> Update model event -> Update model event
commandIf condition cmd update =
    if condition then
        command cmd update

    else
        update


{-| Conditionally add a command to an update.
-}
commandWhen : Maybe a -> (a -> Cmd event) -> Update model event -> Update model event
commandWhen condition cmd update =
    case condition of
        Nothing ->
            update

        Just value ->
            command (cmd value) update


{-| Add a task to an update.
-}
attempt : (Result x a -> event) -> Task x a -> Update model event -> Update model event
attempt f task =
    command (Task.attempt f task)


step : (model -> Update model event) -> Update model event -> Update model event
step stp { commands, model } =
    let
        update =
            stp model
    in
    { update | commands = update.commands ++ commands }


stepIf : Bool -> (model -> Update model event) -> Update model event -> Update model event
stepIf condition stp update =
    if condition then
        step stp update

    else
        update


stepWhen : Maybe a -> (a -> model -> Update model event) -> Update model event -> Update model event
stepWhen condition stp update =
    case condition of
        Nothing ->
            update

        Just value ->
            step (stp value) update


mapModel :
    (model0 -> model1)
    -> Update model0 event
    -> Update model1 event
mapModel f { commands, model } =
    { commands = commands
    , model = f model
    }


toUpdate : Update model event -> ( model, Cmd event )
toUpdate { commands, model } =
    ( model, Cmd.batch commands )
