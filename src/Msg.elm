module Msg exposing (..)

type Msg =
    -- simple states
    TodayState
    | CreateState
    | TaskState
    | LifeGoalState
    -- action states
    | CreateLifeGoal
    | CreateTask
    | UpdateCreateLifeGoalRegister String
    | UpdateTaskEstimatedMinutes (Int, String)
    | UpdateTaskRegister String
    | LifeGoalsState
    | DeleteLifeGoal Int
