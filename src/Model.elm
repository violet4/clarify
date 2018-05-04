module Model exposing (..)

createEmptyTask taskID = Task "" False 0 taskID -1
-- don't use this in place of a real task.
emptyTask = createEmptyTask -1


type alias Task = {
    title: String
    , complete: Bool
    , estimatedMinutes: Int
    , taskID: Int
    , lifeGoalID: LifeGoalID
    }

type alias Today = {
    tasks: List Task
    }
type alias Priority = {
    title: String,
    tasks: List Task,
    id: Int
    }
type alias LifeGoalID = Int
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
    settings: List String
    }
