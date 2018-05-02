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
    | UpdateTaskGoal Int String
    | LifeGoalsState
    | DeleteLifeGoal Int
    | DeleteTask Int
    | AddToday Int
    | RemoveToday Int
    | UpdateDebug
    | SettingsViewState
    | ToggleSetting String
