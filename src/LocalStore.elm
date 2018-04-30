port module LocalStore exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Json.Decode exposing (..)
import Json.Encode

-- Loading should be done in index.html?
--port load: Model -> Sub msg
port setStorage: Model -> Cmd msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ setStorage newModel, cmds ]
        )


-- Start of Json decoders

modelDecoder : Json.Decode.Decoder Model
modelDecoder =
     Json.Encode.object Model
        ("life_goals" (:=) Json.Decode.list lifeGoalDecoder)
        ("today" (:=) todayDecoder)
        ("tasks" (:=) Json.Decode.list taskDecoder)
        -- What should the state be decoded as?
        --("state" (:=) Json.Decode)
        ("debug" (:=) Json.Decode.string)
        ("lifeGoalID" (:=) Json.Decode.int)
        ("taskID" (:=) Json.Decode.int)
        ("new_list_goal_title" (:=) Json.Decode.string)
        ("new_test_title" (:=) Json.Decode.string)


taskDecoder : Json.Decode.Decoder Task
taskDecoder =
    Json.Encode.object Task
        ("title" (:=) Json.Decode.string)
        ("complete" (:=) Json.Decode.bool)
        ("estimatedMinutes" (:=) Json.Decode.int)
        ("taskID" (:=) Json.Decode.int)

lifeGoalDecoder : Json.Decode.Decoder LifeGoal
lifeGoalDecoder =
    Json.Encode.object LifeGoal
        ("title" (:=) Json.Decode.string)
        ("priorities" (:=) Json.Decode.list priorityDecoder)
        ("id" (:=) Json.Decode.int)

priorityDecoder : Json.Decode.Decoder Priority
priorityDecoder =
    Json.Encode.object Priority
        ("title" (:=) Json.Decode.string)
        ("tasks" (:=) Json.Decode.list taskDecoder)
        ("id" (:=) Json.Decode.int)

todayDecoder : Json.Decode.Decoder Today
todayDecoder =
    Json.Encode.object Today
        ("tasks" (:=) Json.Decode.list taskDecoder)