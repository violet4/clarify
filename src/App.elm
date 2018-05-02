
import Html exposing (Html)

import Model exposing (..)
import Msg exposing (..)
import View exposing (view)
import Update exposing (update)
import LocalStore exposing(updateWithStorage, save)


main: Program (Maybe Model) Model Msg
main = Html.programWithFlags {
    init=init, -- Checks if there is a saved model. If so then use that, else use the default
    update=updateWithStorage,
    view=view,
    subscriptions = \_ -> Sub.none -- Doesn't subscribe to anything - we don't need JavaScript to send us anything
    --view=view
    }


-- types:
-- LifeGoal, Priority, Task, Subtask

type alias Flags = Model

init: Maybe Model -> (Model, Cmd Msg)
init savedModel =
    (Maybe.withDefault defaultModel savedModel, Cmd.batch [])

    --Maybe.withDefault defaultModel savedModel ! []
    -- possibly replace [] with [save (Maybe.withDefault defaultModel savedModel]. Doing that didn't seem to have an effect, but it might be needed later


defaultModel: Model
--model = Model [] (Today [])
defaultModel = Model
    -- life_goals
    [
        LifeGoal "cleanliness" [] 0,
        LifeGoal "education" [] 1
    ]
    -- todayTaskIds: List Int
    []
    -- tasks
    [
        (Task "clean desk" False 0 0 -1),
         Task
             "vacuum room" -- title
             False -- complete
             0 -- estimatedMinutes
             1 -- taskID
             -1 -- lifeGoalID
    ]
    -- starting state
--    TodayState
    --TaskState     this was causing an error so I commented it out. It doesn't seem to match with anything in Model.
    "" -- debug
    2 -- lifeGoalID
    3 -- taskID
    "" -- new_life_goal_title
    (createEmptyTask 2)
    -- showDebug
    False
    -- state
    "TodayState"
    -- settings
    []

-- I read https://www.reddit.com/r/elm/comments/4j2fg6/finding_the_last_list_element/d33671d/
-- and then re-wrote it from scratch myself.
last : List a -> Maybe a
last list =
    case list of
        [] -> Nothing
        [last] -> Just last
        h::t -> last t



