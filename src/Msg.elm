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
    | UpdateTaskRegister String
    | LifeGoalsState
    | DeleteLifeGoal Int
