port module LocalStore exposing (..)

import Html exposing (..)
import Model exposing (..)
import Msg exposing (Msg)
import Json.Decode exposing (..)
import Json.Encode

-- Loading should be done in index.html
port save: Model -> Cmd msg

-- Might need to change name to avoid conflict with Update.elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
        ( newModel
        , Cmd.batch [ save newModel, cmds ]
        )


--Encoders and decoders based on Model.elm

-- Start of Json decoders
-- Probably need to change Json.Encode.object to something else
-- Can't figure out how to get Json.object4 like in https://stackoverflow.com/questions/37999504/how-to-pass-union-types-through-elm-ports/38006565#38006565

modelDecoder : Json.Decode.Decoder Model
modelDecoder =
     Json.Encode.object Model
        (field "life_goals" Json.Decode.list lifeGoalDecoder)
        (field "today" todayDecoder)
        (field "tasks" Json.Decode.list taskDecoder)
        -- What should the state be decoded as?
        --(field "state" Json.Decode)
        (field "debug" Json.Decode.string)
        (field "lifeGoalID" Json.Decode.int)
        (field "taskID" Json.Decode.int)
        (field "new_list_goal_title" Json.Decode.string)
        (field "new_test_title" Json.Decode.string)

taskDecoder : Json.Decode.Decoder Task
taskDecoder =
    Json.Encode.object Task
        (field "title" Json.Decode.string)
        (field "complete" Json.Decode.bool)
        (field "estimatedMinutes" Json.Decode.int)
        (field "taskID" Json.Decode.int)

lifeGoalDecoder : Json.Decode.Decoder LifeGoal
lifeGoalDecoder =
    Json.Encode.object LifeGoal
        (field "title" Json.Decode.string)
        (field "priorities" Json.Decode.list priorityDecoder)
        (field "id" Json.Decode.int)

priorityDecoder : Json.Decode.Decoder Priority
priorityDecoder =
    Json.Encode.object Priority
        (field "title" Json.Decode.string)
        (field "tasks" Json.Decode.list taskDecoder)
        (field "id" Json.Decode.int)

todayDecoder : Json.Decode.Decoder Today
todayDecoder =
    Json.Encode.object Today
        (field "tasks" Json.Decode.list taskDecoder)



--Start of Json encoders

modelToValue : Model -> Json.Encode.Value
modelToValue model =
    Json.Encode.object
    [

    ]

taskToValue : Task -> Json.Encode.Value
taskToValue task =
    Json.Encode.object
    [
        ("title", Json.Encode.string task.title)
        ("complete", Json.Encode.bool task.complete)
        ("estimatedMinutes", Json.Encode.int task.estimatedMinutes)
        ("taskID", Json.Encode.int task.taskID)
    ]