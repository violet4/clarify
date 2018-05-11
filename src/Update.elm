port module Update exposing (update, updateWithStorage, save)

import Msg exposing (..)
import Model exposing (..)


port save: Model -> Cmd msg
port updateViewingParentTaskId: Int -> Cmd msg


updateWithStorage: Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
    let
        (newModel, cmds) = update msg model
    in
        (newModel, Cmd.batch [save newModel, cmds])

findTaskById tasks taskID =
    List.head (List.filter (\task -> task.taskID /= taskID) tasks)

sortSettings = [
        "Eisenhower",
        "Life Goal",
        "Estimated Minutes",
        "Description"
    ]


resetFilter settings =
    List.map (\s -> if String.startsWith "filter " s then "filter " else s) settings

-- update the current state, which we use
-- to decide which view to display.
-- here we will also need to use "msg" to be able to
-- add/delete life goals/priorities/tasks
update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
--        TodayState
--        CreateState
--        -- action states
--        LifeGoalsState
        -- bogus update, for triggering events
        Noop -> model ! []

        ToggleSubtaskViewMode ->
            let
                wasSubtaskMode = List.member "Subtask Mode" model.settings
                updatedSettings =
                    if wasSubtaskMode
                    -- then remove it..
                    then List.filter (\s -> s /= "Subtask Mode") model.settings
                    -- otherwise add it
                    else "Subtask Mode" :: model.settings
            in
                {model|settings=updatedSettings} ! []

        ToggleImportance taskId ->
            let
                tasks = List.map (\t -> if t.taskID == taskId then {t|important=not t.important} else t) model.tasks
            in
                {model|tasks=tasks} ! []
        ToggleUrgency taskId ->
            let
                tasks = List.map (\t -> if t.taskID == taskId then {t|urgent=not t.urgent} else t) model.tasks
            in
                {model|tasks=tasks} ! []
        ViewSubTasks taskID ->
            let
                newTaskRegister = model.newTaskRegister
                updatedTaskRegister = {newTaskRegister|parentTaskId=taskID}
                subtaskMode = List.member "Subtask Mode" model.settings
                updatedSettings =
                    if not subtaskMode
                    then "Subtask Mode" :: model.settings
                    else model.settings
            in
            {model|
                viewingParentTaskId=taskID,
                newTaskRegister=updatedTaskRegister,
                -- this allows us to jump from the today page
                state="TaskState",
                settings=updatedSettings
            } ! [updateViewingParentTaskId taskID]
        TopLevel ->
            {model|viewingParentTaskId= -1} ! [updateViewingParentTaskId -1]
        UpOneLevel ->
            let
                parentTaskInList = List.filter (\t -> t.taskID == model.viewingParentTaskId) model.tasks
                parentTaskId = case List.head parentTaskInList of
                    Nothing -> -1
                    Just task -> task.parentTaskId
            in
                {model|viewingParentTaskId=parentTaskId} ! [updateViewingParentTaskId parentTaskId]
        FilterTasks filter ->
            let settingsWithoutFilter = List.filter (\s -> not (String.startsWith "filter " s)) model.settings
            in {model|settings=("filter " ++ filter)::settingsWithoutFilter} ! []
        ChangeTaskSorting sortKey ->
            -- remove the current sort key(s) from settings
            let cleanedSettings = List.filter (\s -> not (List.member s sortSettings)) model.settings
            in
                case sortKey of
                    "None" -> {model|settings=cleanedSettings} ! []
                    newKey -> {model|settings=newKey::cleanedSettings} ! []
        ToggleSetting in_text ->
            let
                settings =
                    if List.member in_text model.settings
                        then List.filter (\s -> s /= in_text) model.settings
                        else in_text :: model.settings
            in
                {model|settings=settings} ! []
        UpdateCreateLifeGoalRegister in_text -> {
            model |
            new_life_goal_title = in_text,
            debug = toString msg
            } ! []
        CreateLifeGoal -> if model.new_life_goal_title == "" then model ! [] else {
            model |
            life_goals = (List.append model.life_goals [LifeGoal model.new_life_goal_title [] model.lifeGoalID]),
            lifeGoalID = model.lifeGoalID + 1,
            new_life_goal_title = "",
            debug = toString msg
            } ! []

        UpdateTaskGoal taskId goalId -> 
            case String.toInt goalId of
                Err _ -> model ! []
                Ok goal ->
                    {model |
                        tasks = List.map (\t -> if t.taskID /= taskId then t else {t|lifeGoalID=goal}) model.tasks,
                        debug = "taskID " ++ (toString taskId) ++ "; goalID " ++ goalId
                        } ! []

        UpdateTaskRegister msgName in_text ->
            case msgName of
                "estimatedMinutes" ->
                    case String.toInt in_text of
                        Err _ -> model ! []
                        Ok estimatedMinutes ->
                            let newTaskRegister=model.newTaskRegister
                            in {model|
                                newTaskRegister={newTaskRegister|estimatedMinutes=estimatedMinutes}
                                } ! []
                "description" ->
                    let newTaskRegister = model.newTaskRegister
                    in {model|
                        newTaskRegister={newTaskRegister|title=in_text}
                    } ! []
                "lifeGoal" ->
                    case String.toInt in_text of
                        Err _ -> model ! []
                        Ok lifeGoalIDInt ->
                           let newTaskRegister = model.newTaskRegister
                            in {model|
                                newTaskRegister={newTaskRegister|lifeGoalID=lifeGoalIDInt}
                                } ! []
--                                lifeGoals = List.map (
--                                        \lg -> if lg.lifeGoalID /= lifeGoalIDInt then
--                                    ) model.life_goals
--
--
--                                banana = List.length [1,2,3]
--                            in
--                                let newTaskRegister = model.newTaskRegister
--                                in {model|newTaskRegister={newTaskRegister|description=in_text}}
                _ -> model ! []
--        {
--            model |
--            debug = toString msg
--            }
        UpdateTaskDescription taskID newDescription ->
            case newDescription of
                "" -> model ! []
                _ -> {model |
                        debug = "taskID " ++ (toString taskID) ++ "; title " ++ newDescription,
                        tasks = List.map (\t -> if t.taskID /= taskID then t else {t|title=newDescription}) model.tasks
                     } ! []
        UpdateTaskEstimatedMinutes taskID estMinutesStr ->
            case String.toInt estMinutesStr of
                Err _ -> model ! []
                Ok estimatedMinutes ->
                    let
                        checkedEstimatedMinutes =
                            if estimatedMinutes < 0 then 0
                            else if estimatedMinutes > 9999 then 9999
                            else estimatedMinutes
                    in
                    {model |
                        debug = "taskID " ++ (toString taskID) ++ "; estMinutesStr " ++ estMinutesStr
                        , tasks = List.map (\t -> if t.taskID /= taskID then t else {t|estimatedMinutes=checkedEstimatedMinutes}) model.tasks
                        } ! []
        -- don't let user create empty task
        CreateTask -> if model.newTaskRegister.title == "" then model ! [] else
            let
                newTaskRegister = model.newTaskRegister
                updatedNewTask = {newTaskRegister|parentTaskId=model.viewingParentTaskId}
            in
            {model |
--                title: String
--                , complete: Bool
--                , estimatedMinutes: Int
--                , taskID: Int

            tasks = List.append model.tasks [updatedNewTask],
            newTaskRegister = createEmptyTask model.taskID 0,
            taskID = model.taskID + 1,
            debug = toString msg,
            lifeGoalID = model.lifeGoalID
            } ! []
        UpdateLifeGoalDescription lifeGoalID newDescription ->
            case newDescription of
                "" -> model ! []
                _ -> {model |
                        debug = "lifeGoalID " ++ (toString lifeGoalID) ++ "; title " ++ newDescription,
                        life_goals = List.map (\g -> if g.id /= lifeGoalID then g else {g|title=newDescription}) model.life_goals
                     } ! []
        -- TODO if user deletes all life goals, what should we do to the tasks marked as that life goal?
        DeleteLifeGoal id -> {
            model |
            debug = toString msg,
            life_goals = List.filter (\lifeGoal -> lifeGoal.id /= id) model.life_goals
            } ! []

        DeleteTask id -> {
            model |
            debug = toString msg,
            completed_tasks = List.filter (\task -> task.taskID /= id) model.completed_tasks
            } ! []

        UncompleteTask id ->
            let
                completedTasks = List.filter (\t -> t.taskID /= id) model.completed_tasks
                uncompletedTaskInList = List.filter (\t -> t.taskID == id) model.completed_tasks
                uncompletedTasks = List.concat [model.tasks, uncompletedTaskInList]
            in
                {model|
                    completed_tasks=completedTasks,
                    tasks=uncompletedTasks} ! []

        CompleteTask id ->
            let
                incompleteTasks = List.filter (\t -> t.taskID /= id) model.tasks
                completedTaskInList = List.filter (\t -> t.taskID == id) model.tasks
                completedTasks = List.concat [model.completed_tasks, completedTaskInList]
            in
                {model|completed_tasks=completedTasks, tasks=incompleteTasks} ! []

        AddToday taskID ->
            {model|todayTaskIds=taskID :: model.todayTaskIds} ! []
        RemoveToday taskID ->
            {model|todayTaskIds=List.filter (\tid -> tid /= taskID) model.todayTaskIds} ! []
        MoveTaskDown thisTaskId newParentTaskIdString ->
            let
                newParentTaskId = Result.withDefault -1 (String.toInt newParentTaskIdString)
                updatedTasks = List.map (\t ->
                    if t.taskID == thisTaskId
                        -- if this somehow fails, don't accidentally move this
                        -- task to the top level.
                        && newParentTaskId /= -1
                    then {t|parentTaskId=newParentTaskId}
                    else t) model.tasks
            in
            {model|tasks=updatedTasks} ! []
        MoveTaskUp taskID ->
            let
                parentTaskId =
                    case List.head (List.filter (\t -> t.taskID == taskID) model.tasks) of
                        Just task -> task.parentTaskId
                        Nothing -> -1
                parentParentTaskId =
                    case List.head (List.filter (\t -> t.taskID == parentTaskId) model.tasks) of
                        Just task -> task.parentTaskId
                        Nothing -> -1
                tasks = List.map (\t -> if t.taskID == taskID then {t|parentTaskId=parentParentTaskId} else t) model.tasks
            in
                {model|tasks=tasks, debug="taskid " ++ toString taskID ++ ", parent task id " ++ toString parentTaskId ++ ", parent parent task id " ++ toString parentParentTaskId} ! []
        -- even if we don't know what the input was,
        -- we should still update the state in case
        -- user clicked on another tab!
        _ -> {model |
            debug = toString msg,
            state=toString msg,
            settings = resetFilter (model.settings)
            } ! []

-- but this is how we update our model with a new life goal called "cleanliness":
-- { model | life_goals = (LifeGoal "cleanliness" []) :: model.life_goals }
-- we need a "msg" that enumerates the actions we could take at this step,
-- (i.e. create a corresponding version of "type Msg = Increment |
-- Decrement")