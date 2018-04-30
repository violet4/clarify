module Msg exposing (..)

type Msg =
    -- simple states
    TodayState
    | CreateState
    | TaskState
    | LifeGoalState
    -- action states
    | CreateLifeGoal
    | CreateLifeGoalState
    | CreateTaskState
    | CreateTask
    | UpdateCreateLifeGoalRegister String
    | UpdateTaskEstimatedMinutes Int String
    | UpdateTaskRegister String String
    | LifeGoalsState
    | DeleteLifeGoal Int
