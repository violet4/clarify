module Msg exposing (..)

type Msg =
    Noop
    -- simple states
    | TodayState
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
    | MoveTaskUp Int
    -- MoveTaskDown thisTaskId newParentTaskId
    | MoveTaskDown Int String
    | UpdateDebug
    | SettingsViewState
    | ToggleSetting String
    | ChangeTaskSorting String
    | FilterTasks String
    | ViewSubTasks Int
    | UpOneLevel
    | TopLevel
