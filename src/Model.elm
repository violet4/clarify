module Model exposing (..)

import Msg exposing (..)

type alias Task = {
    title: String
    , complete: Bool
    , estimatedMinutes: Int
    , taskID: Int
    , lifeGoalID: LifeGoalID
    }
createEmptyTask taskID = Task "" False 0 taskID -1
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
    today: Today,
    tasks: List Task,
    state: Msg,
    debug: String,
    lifeGoalID: Int,
    taskID: Int,
    new_life_goal_title: String,
    newTaskRegister: Task
    }
