module Msg exposing (..)

type Msg =
    -- simple states
    TodayState
    | CreateState
    | TaskState
    | HelpState
    | LifeGoalState
    -- action states
    | CreateLifeGoal
    | CreateLifeGoalState
    | CreateTaskState
    | CreateTask
    | UpdateCreateLifeGoalRegister String
    | UpdateTaskDescription Int String
    | UpdateTaskEstimatedMinutes Int String
    | UpdateTaskRegister String String
    | UpdateTaskGoal Int String
    | LifeGoalsState
    | UpdateLifeGoalDescription Int String
    | DeleteLifeGoal Int
    | DeleteTask Int
    | AddToday Int
    | RemoveToday Int
    | UpdateDebug
    | SettingsViewState
    | ToggleSetting String
    | ChangeTaskSorting String
    | FilterTasks String
    | ViewSubTasks Int
    | UpOneLevel
    | TopLevel
    --Galen edit
    | SetText String
