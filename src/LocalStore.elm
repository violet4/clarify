port module LocalStore exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Update exposing (update)
port save: Model -> Cmd msg

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
    let
        (newModel, cmds) = update msg model
    in
        (newModel, Cmd.batch [save newModel, cmds])
