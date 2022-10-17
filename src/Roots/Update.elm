module Roots.Update exposing
    ( Step
    , Update
    , command
    , commandIf
    , commandWhen
    , mapModel
    , pure
    , step
    , stepIf
    , stepWhen
    , toUpdateFunction
    )


type alias Update model event =
    { commands : List (Cmd event)
    , model : model
    }


type alias Step model event =
    model -> Update model event


{-| Create a pure update from a model.
-}
pure : Step model event
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


step : Step model event -> Update model event -> Update model event
step stp { commands, model } =
    let
        update =
            stp model
    in
    { update | commands = update.commands ++ commands }


stepIf : Bool -> Step model event -> Update model event -> Update model event
stepIf condition stp update =
    if condition then
        step stp update

    else
        update


stepWhen : Maybe a -> (a -> Step model event) -> Update model event -> Update model event
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


toUpdateFunction : (event -> Step model event) -> event -> model -> ( model, Cmd event )
toUpdateFunction f event model0 =
    case f event model0 of
        { commands, model } ->
            ( model, Cmd.batch commands )
