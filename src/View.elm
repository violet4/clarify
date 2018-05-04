module View exposing (view)

import Regex
import Round exposing (round)
import Html exposing (
    div, text, Html, a, br, hr, button, input, form,
    select, option, map, table, tr, td, tbody
    )
import Html.Attributes exposing (href, style, align, id, type_, value, property, attribute, class)
import Html.Events exposing (onClick, onSubmit, onInput)

import Msg exposing (..)
import Model exposing (..)

-- styles ######################################################################
fullSizeStyle = style [
    ("width", "98%"),
    ("height", "75%"),
    ("padding-left", "1%"),
    ("padding-top", "1%"),
    ("padding-right", "1%")
    ]
width100p = style [("width", "100%")]
htmlAppHeader = div [style [
        ("color", "#1D417D"),
        ("font-size", "30px"),
        ("padding-left", "40%")
    ]] [text "Clarify"]

redFont = style [("color", "red")]
noStyle = style []

currentView model =
    case model.state of
        -- each of these takes a model
        "TodayState" -> todayView model
        "TaskState" -> taskView model
        "LifeGoalsState" -> lifeGoalsView model
        "LifeGoalState" -> lifeGoalsView model
        "SettingsViewState" -> settingsView model
        _ -> todayView model

settingsButton model name helpText =
    div [] [
        button [onClick (ToggleSetting name)] [
            text (if (List.member name model.settings)
                then "Turn off"
                else "Turn on"
            )],
        text (" " ++ name ++ ": " ++ helpText)
    ]

settingsView: Model -> Html Msg
settingsView model =
    div [fullSizeStyle] [
        settingsButton
            model
            "Hide today tasks from tasks page"
            "When you add a task to today from the today page, it will no longer display on the tasks page"
    ]

getLifeGoal goals goalId = 
    case List.head (List.filter (\goal -> goal.id == goalId) goals) of
        Maybe.Just goal -> goal
        Maybe.Nothing -> {title = "Nothing", priorities = [], id = -10}

lifeGoalSelectorForCreating life_goals=
    -- TODO need to map each life goal to its ID and give it a Msg message so we can update the task
    select [onInput ((UpdateTaskRegister "lifeGoal"))] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id)] [text lifeGoal.title]))
        ({title = "Life Goal Selection", priorities = [], id = 0} :: life_goals)
    )

lifeGoalSelectorForEditing life_goals task =
    select [(onInput (UpdateTaskGoal task.taskID))] (
        List.map
        (\lifeGoal -> (option [value (toString lifeGoal.id), Html.Attributes.selected (lifeGoal.id == task.lifeGoalID)] [text lifeGoal.title]))
        ({title = "Nothing", priorities = [], id = -10} :: life_goals)
    )

estimatedMinutesSelector task =
    input [
        type_ "number",
        onInput (UpdateTaskEstimatedMinutes task.taskID),
        value (toString task.estimatedMinutes)
    ] []

taskToHtmlDisplay: Model -> Task -> List (Html Msg)
taskToHtmlDisplay model task = [
        div [] [
            div [] [
                text (if model.showDebug then ((toString task.taskID) ++ " ") else ""),
                -- delete button
                button [
                    onClick (DeleteTask task.taskID)
                ] [text "Delete"],

                -- "add/remove from today" button
                if (List.member task.taskID model.todayTaskIds)
                    then button [
                        addRemoveButton150width ,
                        onClick (RemoveToday task.taskID)
                    ] [text "Remove from Today"]
                    else button [
                        addRemoveButton150width,
                        onClick (AddToday task.taskID)
                    ] [text    "Add to Today"],

                -- ability to select a life goal for this task
                (lifeGoalSelectorForEditing model.life_goals task),
                -- estimated minutes
                estimatedMinutesSelector task
            ],
            div [] [
                -- task text
                text task.title
            ]
        ]
    ]

sortSelectorButton fieldName =
    button [onClick (ChangeTaskSorting fieldName)] [text fieldName]

sortBySelectorButtons model =
    div [] [
        text "Sort by: ",
        sortSelectorButton "None",
        sortSelectorButton "Life Goal",
        sortSelectorButton "Estimated Minutes",
        sortSelectorButton "Description"
    ]

addRemoveButton150width = style [("width", "150px")]

taskTodayMatchesViewState: Model -> Task -> Bool
taskTodayMatchesViewState model task =
    if List.member "Hide today tasks from tasks page" model.settings
        then
            case model.state of
                "TodayState" -> List.member task.taskID model.todayTaskIds
                _ -> not (List.member task.taskID model.todayTaskIds)
        else True


solidBlackBorderStyle = style [
        ("border", "1px solid black")
    ]

taskToTableRow model task =
    tr [] [
        td [class "taskButtons"] [
            text (if model.showDebug then ((toString task.taskID) ++ " ") else ""),
            -- delete button
            button [
                onClick (DeleteTask task.taskID)
            ] [text "Delete"],

            -- "add/remove from today" button
            if (List.member task.taskID model.todayTaskIds)
                then button [
                    addRemoveButton150width ,
                    onClick (RemoveToday task.taskID)
                ] [text "Remove from Today"]
                else button [
                    addRemoveButton150width,
                    onClick (AddToday task.taskID)
                ] [text    "Add to Today"],

            -- ability to select a life goal for this task
            (lifeGoalSelectorForEditing model.life_goals task),
            -- estimated minutes
            estimatedMinutesSelector task
        ],
        td [solidBlackBorderStyle, class "taskText"] [
            text task.title
        ]
    ]

taskListToHtmlTable model tasks =
    table [] [tbody [] (List.map (\t -> taskToTableRow model t) tasks)]

taskListToHtmlList model tasks =
    List.map
        (\task -> taskToHtmlDisplay model task)
        tasks

sortTasks model tasks =
    if List.member "Life Goal" model.settings
    then (List.sortBy .lifeGoalID tasks)
    else if List.member "Estimated Minutes" model.settings
    then (List.sortBy .estimatedMinutes tasks)
    else if List.member "Description" model.settings
    then (List.sortBy .title tasks)
    else (List.sortBy .taskID tasks)

taskFilterTextInput =
    input [onInput FilterTasks] []

getFullTaskText: Model -> Task -> String
getFullTaskText model task =
    let
        lifeGoalID = task.lifeGoalID
        lifeGoalStringList = List.filter (\lg -> lg.id == lifeGoalID) model.life_goals
        lifeGoal =
            case List.head lifeGoalStringList of
                Nothing -> LifeGoal "" [] -1
                Just lg -> lg
        lifeGoalString = lifeGoal.title
    in
        lifeGoalString ++ " " ++ task.title

taskMatchesFilters: Task -> Model -> List String -> Bool
taskMatchesFilters task model filterParts =
    let
        fullTaskText = getFullTaskText model task
        fp = List.map (\p -> Regex.contains (Regex.regex p) fullTaskText) filterParts
    in
        List.all (\x -> x) fp

filterTasks: List Task -> Model -> List String -> List Task
filterTasks tasks model settings =
    let
        settingsThatStartWithFilter = List.filter (\s -> String.startsWith "filter " s) settings
        filterFilterString =
            case List.head settingsThatStartWithFilter of
                Nothing -> ""
                Just s -> s
        fullFilterText = String.dropLeft 7 filterFilterString
        filterParts = String.split " " fullFilterText
    in
        -- need to filter out tasks by the filterParts
        List.filter (\t -> taskMatchesFilters t model filterParts) tasks

-- tasks view shows all tasks
taskView: Model -> Html Msg
taskView model =
    let
        -- filter based on user preferences
        taskViewTasks = (List.filter (\t -> taskTodayMatchesViewState model t) model.tasks)
        filteredTaskViewTasks = filterTasks taskViewTasks model model.settings
        sortedTaskViewTasks = sortTasks model filteredTaskViewTasks
    in
    div [fullSizeStyle]
        (List.append
            [
                -- sorting buttons
                sortBySelectorButtons model,
                br [] [],

                -- filter text input
                taskFilterTextInput,
                br [] [],

                text "Total Estimated Minutes for All Displayed Tasks: ",
                tasksEstimatedMinutesSumText sortedTaskViewTasks,
                text (" (" ++ (Round.round 2 ((toFloat (tasksEstimatedMinutesSum sortedTaskViewTasks))/60)) ++ " hours)"),
                br [] [], br [] [],
                -- list of current tasks
                taskListToHtmlTable model sortedTaskViewTasks
            ]
            -- section to create a new task
            [
                br [] [],
                hr [] [],
                br [] [],

                -- form to create a new task
                form [onSubmit CreateTask] [
                    text "Create a Task",
                    br [] [],
                    --text "TaskState",
                    text "Life Goal: ",
                    lifeGoalSelectorForCreating model.life_goals,
                    br [] [],
                    br [] [],
                    text "Time: ",
                    input [
                        type_ "number",
                        onInput (UpdateTaskRegister "estimatedMinutes")
                    ][],
                    text " Minutes",
                    br [] [],
                    br [] [],
                    text "Description: ",
                    input [
                        onInput (UpdateTaskRegister "description")
                    ] [],

                    br [] [],
                    br [] [],
                    button [type_ "submit" ] [text "Create"]
                ]
                -- end form to create a new task
            ]
        )

tasksEstimatedMinutesSum tasks =
    (List.foldl
         -- sum up the estimated minutes
         (\t1 t2 -> {estimatedMinutes=t1.estimatedMinutes + t2.estimatedMinutes})
         {estimatedMinutes=0}
         tasks
     ).estimatedMinutes

tasksEstimatedMinutesSumText tasks =
    text (toString (tasksEstimatedMinutesSum tasks))

-- today view shows tasks we chose for today
todayView: Model -> Html Msg
todayView model = div [fullSizeStyle] (
    let
        at_least_one_task = ((List.length model.todayTaskIds) > 0)
        todayTasks = List.filter (\t -> (List.member t.taskID model.todayTaskIds)) model.tasks
        filteredTodayTasks = filterTasks todayTasks model model.settings
    in if at_least_one_task
        then ([
            div [] [
                text "Total Estimated Minutes for Today's Tasks: ",
                tasksEstimatedMinutesSumText filteredTodayTasks,
                text " (",
                text (Round.round 2 ((toFloat ((tasksEstimatedMinutesSum filteredTodayTasks))) / 60)),
                text " hours)"
            ],
            -- filter text input
            taskFilterTextInput,
            taskListToHtmlTable model filteredTodayTasks
        ])
        else [
            text "You don't have any tasks for today!",
            br [] [],
            text "Go to ",
            todayLinkButton model,
            text " to add some!"
        ]
    )

lifeGoalElement: LifeGoal -> Html Msg
lifeGoalElement lifeGoal =
    div [width100p] [
        button [(onClick (DeleteLifeGoal lifeGoal.id))] [text "Delete"],
        text " ",
        text lifeGoal.title
    ]

lifeGoalsView: Model -> Html Msg
lifeGoalsView model = div [fullSizeStyle]
    (List.append
         (List.map lifeGoalElement model.life_goals)
         [
            br [] [],
            hr [] [],
            br [] [],
            text "Create a Life Goal",
            br [] [],
            form [onSubmit CreateLifeGoal] [
                text "Description: ",
                input [
                  onInput UpdateCreateLifeGoalRegister
              ] [],
              br [] [],
              br [] [],
              button [type_ "submit"] [text "Create"]
           ]]
     )


--navigation: () -> Html msg
-- navigation should be able to switch us between the states.
htmlNavigationBar model = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#"),
            (onClick TodayState),
            (if model.state == "TodayState" then redFont else noStyle)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        todayLinkButton model,
        text " ",
        lifeGoalsLinkButton model,
        text " ",
        settingsLinkButton model,
        text " ",
        a [href "#", onClick UpdateDebug] [text "Debug"]
    ]

lifeGoalsLinkButton model =
    a [
        (href "#"),
        (onClick LifeGoalsState),
        (if model.state == "LifeGoalsState" then redFont else noStyle)
    ] [text "Life Goals"]

settingsLinkButton model =
    a [
        (href "#"),
        (onClick SettingsViewState),
        (if model.state == "SettingsViewState" then redFont else noStyle)
    ] [text "Settings"]

todayLinkButton model =
    a [
        (href "#"),
        (onClick TaskState),
        (if model.state == "TaskState" then redFont else noStyle)
    ] [text "Tasks"]

createViewButton model =
    a [
        (href "#"),
        (onClick CreateState),
        (if model.state == "CreateState" then redFont else noStyle)
    ] [text "Create"]


mainViewHtmlNavigationBar = div [] [
        -- these links need to be attached to onClick events,
        -- or something of the like
        a [
            -- these links don't take us anywhere yet,
            -- but at least we can click them.
            (href "#"),
            (onClick TodayState)
        ] [text "Today"],
        -- put some spacing between the links
        text " ",
        a [
            (href "#")
--            ,
--            (onClick LifeGoalsState)
        ] [text "Life Goals"]
    ]

-- we need to create a state that holds the current state -
-- are we looking at life goals, priorities, tasks, or
-- today? (i.e. create a corresponding version of "type Msg
-- = Increment | Decrement") depending on the state we're
-- in, we need to show the model in a way that is useful,
-- with interactivity.
view: Model -> Html Msg
view model =
    div [fullSizeStyle] [
        text (if model.showDebug then (toString model.debug) else ""),
        htmlAppHeader,
        htmlNavigationBar model,
        hr [] [],
        text (if model.showDebug then (toString model) else ""),
        if model.showDebug then hr [] [] else text "",
        currentView model
    ]
