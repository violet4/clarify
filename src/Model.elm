module Model exposing (..)

createEmptyTask: Int -> Int -> Task
createEmptyTask taskID parentTaskID = Task "" False -1 taskID 0 parentTaskID

type alias LifeGoalID = Int
type alias Task = {
    title: String
    , complete: Bool
    , lifeGoalID: LifeGoalID
    , taskID: Int
    , estimatedMinutes: Int
    , parentTaskId: Int
    }

type alias Today = {
    tasks: List Task
    }
type alias Priority = {
    title: String,
    tasks: List Task,
    id: Int
    }
type alias LifeGoal = {
    title: String,
    priorities: List Priority,
    id: LifeGoalID
    }
type alias Model = {
    life_goals: List LifeGoal,
    todayTaskIds: List Int,
    tasks: List Task,
    debug: String,
    lifeGoalID: Int,
    taskID: Int,
    new_life_goal_title: String,
    newTaskRegister: Task,
    state: String,
    settings: List String,
    viewingParentTaskId: Int,
    version: Int,
    textSpace: String
    }
