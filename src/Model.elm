module Model exposing (..)

createEmptyTask: Int -> Int -> Task
createEmptyTask taskID parentTaskID = Task "" False -1 taskID 0 parentTaskID False False

-- REMEMBER: WHENEVER MODIFYING THIS, YOU NEED
-- TO UPDATE index.html JAVASCRIPT VERSIONING,
-- AND defaultModel IN App.elm!!!!!!!!!!!!
type alias LifeGoalID = Int
type alias Task = {
    title: String
    , complete: Bool
    , lifeGoalID: LifeGoalID
    , taskID: Int
    , estimatedMinutes: Int
    , parentTaskId: Int
    , important: Bool
    , urgent: Bool
    }

-- REMEMBER: WHENEVER MODIFYING THIS, YOU NEED
-- TO UPDATE index.html JAVASCRIPT VERSIONING,
-- AND defaultModel IN App.elm!!!!!!!!!!!!
type alias Today = {
    tasks: List Task
    }

-- REMEMBER: WHENEVER MODIFYING THIS, YOU NEED
-- TO UPDATE index.html JAVASCRIPT VERSIONING,
-- AND defaultModel IN App.elm!!!!!!!!!!!!
type alias Priority = {
    title: String,
    tasks: List Task,
    id: Int
    }
-- REMEMBER: WHENEVER MODIFYING THIS, YOU NEED
-- TO UPDATE index.html JAVASCRIPT VERSIONING,
-- AND defaultModel IN App.elm!!!!!!!!!!!!
type alias LifeGoal = {
    title: String,
    priorities: List Priority,
    id: LifeGoalID
    }

-- REMEMBER: WHENEVER MODIFYING THIS, YOU NEED
-- TO UPDATE index.html JAVASCRIPT VERSIONING,
-- AND defaultModel IN App.elm!!!!!!!!!!!!
type alias Model = {
    life_goals: List LifeGoal,
    todayTaskIds: List Int,
    tasks: List Task,
    completed_tasks: List Task,
    debug: String,

    -- this lifeGoalID is to store the id of
    -- the next newly-created life goal
    lifeGoalID: Int,

    -- this taskID is to store the id of
    -- the next newly-created task
    taskID: Int,

    new_life_goal_title: String,
    newTaskRegister: Task,
    state: String,
    settings: List String,
    viewingParentTaskId: Int,
    version: Int
    }
