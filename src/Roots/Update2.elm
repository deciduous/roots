module Roots.Update2 exposing
    ( Update, empty, sequence
    , pure, command, attempt
    , if_, when
    , overAffineTraversal, overLens
    )

{-| Update.

@docs Update, empty, sequence
@docs pure, command, attempt
@docs if_, when
@docs overAffineTraversal, overLens

-}

import Roots.AffineTraversal as AffineTraversal exposing (AffineTraversal)
import Roots.Lens as Lens exposing (Lens)
import Task exposing (Task)


type alias Update e a b =
    a -> ( b, Cmd e )


{-| The no-op update.
-}
empty : Update e a a
empty x =
    ( x, Cmd.none )


andThen : Update e b c -> Update e a b -> Update e a c
andThen g f x0 =
    let
        ( x1, e1 ) =
            f x0

        ( x2, e2 ) =
            g x1
    in
    ( x2, Cmd.batch [ e1, e2 ] )


{-| Sequence a list of updates into one, run left-to-right.
-}
sequence : List (Update e a a) -> Update e a a
sequence =
    List.foldl andThen empty


{-| Make an update from a pure function.
-}
pure : (a -> b) -> Update e a b
pure f x =
    ( f x, Cmd.none )


{-| Make an update from a command.
-}
command : (a -> Cmd e) -> Update e a a
command f x =
    ( x, f x )


{-| Make an update from a task.
-}
attempt : (Result r s -> e) -> (a -> Task r s) -> Update e a a
attempt f task =
    command (\x -> Task.attempt f (task x))


{-| Make an update conditional on its model.
-}
if_ : (a -> Bool) -> Update e a a -> Update e a a
if_ p f x =
    if p x then
        f x

    else
        ( x, Cmd.none )


{-| Make an update conditional on its model.
-}
when : (a -> Maybe b) -> (b -> Update e a a) -> Update e a a
when p f x =
    case p x of
        Nothing ->
            ( x, Cmd.none )

        Just y ->
            f y x


{-| Apply an update to the target of an affine traversal.
-}
overAffineTraversal : AffineTraversal s t a b -> Update e a b -> Update e s t
overAffineTraversal l f s =
    case AffineTraversal.matching l s of
        Ok a ->
            let
                ( b, e ) =
                    f a
            in
            ( AffineTraversal.set l b s, e )

        Err t ->
            ( t, Cmd.none )


{-| Apply an update to the target of a lens.
-}
overLens : Lens s t a b -> Update e a b -> Update e s t
overLens l f s =
    let
        ( b, e ) =
            f (Lens.view l s)
    in
    ( Lens.set l b s, e )
